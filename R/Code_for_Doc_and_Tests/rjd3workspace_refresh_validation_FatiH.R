# JD3 Workspace Refresh Validation Script
# ------------------------------------------------------------
# Purpose
# ------------------------------------------------------------
# This script provides a simple and reproducible workflow to
# validate JD3 workspace execution against JDemetra+ GUI exports.
#
# Main objectives:
# 1. Run all time series contained in a JD3 workspace XML file.
# 2. Apply a chosen refresh policy.
# 3. Extract seasonally adjusted (SA) results from the R workflow.
# 4. Compare those SA results against the JDemetra+ Excel export.
# 5. Compare selected diagnostics against the JDemetra+ CSV export.
# 6. Produce simple output tables that can be reviewed or shared.
#
# The script is intentionally designed to be:
# - readable,
# - easy to adapt,
# - easy to reproduce,
# - and suitable for documentation or testing purposes.
#
# ------------------------------------------------------------
# Required input files
# ------------------------------------------------------------
# 1. workspace.xml
#    JD3 workspace file created in JDemetra+ GUI.
#
# 2. demetra.xlsx
#    JDemetra+ Excel export containing sheets such as:
#    - sa  : seasonally adjusted series
#    - y   : original series
#    - cal : calendar-adjusted series
#    etc.
#    This script uses the "sa" sheet.
#
# 3. demetra_m.csv
#    JDemetra+ CSV export containing diagnostics and model details.
#
# ------------------------------------------------------------
# What the script produces
# ------------------------------------------------------------
# The script writes:
# - sa_series_comparison.csv
# - diagnostics_series_comparison.csv
# - diagnostics_match_summary.csv
# - workspace_refresh_validation_outputs.xlsx (optional, if openxlsx is installed)
#
# ------------------------------------------------------------
# User configuration
# ------------------------------------------------------------
# Please edit the paths below before running the script.
# Only this section should normally need to be changed.
# ------------------------------------------------------------

workspace_xml <- "path/to/workspace.xml"
demetra_xlsx  <- "path/to/demetra.xlsx"
demetra_csv   <- "path/to/demetra_m.csv"
output_dir    <- "path/to/output"

# Refresh policy used in the test.
# Depending on the installed rjd3workspace version, accepted names may differ.
# If needed, replace with the exact policy name used in your local setup.
refresh_policy <- "FreeParameters"

# Numerical tolerance used for SA comparisons and selected diagnostics.
numeric_tol <- 1e-6

# Selected diagnostic columns to compare from demetra_m.csv.
selected_diag_cols <- c(
  "ll", "nparams", "nobs", "df",
  "p", "d", "q", "bp", "bd", "bq",
  "mean", "ntd", "nout", "mode",
  "seasonal", "log"
)

# ------------------------------------------------------------
# Libraries
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(zoo)
  library(readr)
  library(rjd3workspace)
  library(rjd3tramoseats)
  library(rjd3toolkit)
})

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

clean_series_name <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_replace_all('^"+|"+$', '') |>
    stringr::str_replace_all('"', '') |>
    stringr::str_replace_all('\\s*\\(frozen\\)\\s*$', '') |>
    stringr::str_squish()
}

safe_eval <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

normalize_jd_colnames <- function(nms) {
  nms <- as.character(nms)
  bad <- is.na(nms) | trimws(nms) == ""
  nms[bad] <- paste0("blank_", seq_len(sum(bad)))
  make.unique(nms, sep = "__dup")
}

resolve_selected_cols <- function(actual_names, selected_cols) {
  out <- character(0)
  for (nm in selected_cols) {
    hit <- which(actual_names == nm)
    if (length(hit) > 0) {
      out <- c(out, actual_names[hit[1]])
    }
  }
  unique(out)
}

parse_summary_text <- function(fit) {
  txt <- paste(capture.output(summary(fit)), collapse = "\n")
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))

  grab_num <- function(pattern) {
    out <- stringr::str_match(txt, pattern)[, 2]
    if (length(out) == 0) return(NA_real_)
    suppressWarnings(as.numeric(out))
  }

  grab_int <- function(pattern) {
    out <- stringr::str_match(txt, pattern)[, 2]
    if (length(out) == 0) return(NA_integer_)
    suppressWarnings(as.integer(out))
  }

  log_transform <- dplyr::case_when(
    stringr::str_detect(txt, 'Log-transformation:\\s*yes') ~ 'Multiplicative',
    stringr::str_detect(txt, 'Log-transformation:\\s*no') ~ 'Additive',
    TRUE ~ NA_character_
  )

  arima_model <- stringr::str_match(txt, 'SARIMA model:\\s*([^\\n]+)')[, 2]
  arima_model <- ifelse(length(arima_model) == 0, NA_character_, arima_model)

  reg_start <- grep('^Regression model:', lines)
  reg_names <- character(0)
  if (length(reg_start) == 1) {
    reg_lines <- lines[(reg_start + 1):length(lines)]
    reg_lines <- reg_lines[!grepl('^---|^Signif|^Number of observations|^Loglikelihood|^Standard error|^AIC', reg_lines)]
    reg_lines <- reg_lines[nzchar(trimws(reg_lines))]
    for (ln in reg_lines) {
      nm <- stringr::str_match(ln, '^\\s*([^\\s].*?)\\s+[-0-9]')[, 2]
      if (!is.na(nm)) reg_names <- c(reg_names, trimws(nm))
    }
  }

  has_td <- any(grepl('^td$|trading', reg_names, ignore.case = TRUE))
  has_mean <- any(grepl('mean|mu|intercept|const', reg_names, ignore.case = TRUE))
  out_names <- reg_names[grepl('AO|LS|TC', reg_names, ignore.case = TRUE)]

  list(
    mode = ifelse(log_transform == 'Multiplicative', 'Multiplicative', 'Additive'),
    log = ifelse(log_transform == 'Multiplicative', 1L, 0L),
    ll = grab_num('Loglikelihood:\\s*([-0-9.Ee]+)'),
    aic = grab_num('AIC:\\s*([-0-9.Ee]+)'),
    aicc = grab_num('AICc:\\s*([-0-9.Ee]+)'),
    bic = grab_num('BIC:\\s*([-0-9.Ee]+)'),
    sigma_ml = grab_num('Standard error of the regression \\(ML estimate\\):\\s*([-0-9.Ee]+)'),
    nobs = grab_int('Number of observations:\\s*([0-9]+)'),
    neffectiveobs = grab_int('Number of effective observations:\\s*([0-9]+)'),
    nparams = grab_int('Number of parameters:\\s*([0-9]+)'),
    regression_terms = ifelse(length(reg_names) > 0, paste(reg_names, collapse = '; '), NA_character_),
    nout = length(out_names),
    outlier_terms = ifelse(length(out_names) > 0, paste(out_names, collapse = '; '), NA_character_),
    mean = as.integer(has_mean),
    ntd = as.integer(has_td),
    seasonal = 1L,
    arima_model = arima_model
  )
}

parse_arima_orders <- function(arima_model) {
  if (is.na(arima_model) || !nzchar(arima_model)) {
    return(tibble(p = NA_integer_, d = NA_integer_, q = NA_integer_,
                  bp = NA_integer_, bd = NA_integer_, bq = NA_integer_))
  }
  m <- stringr::str_match(arima_model, '\\((\\d+),(\\d+),(\\d+)\\)\\s*\\((\\d+),(\\d+),(\\d+)\\)')
  if (all(is.na(m))) {
    return(tibble(p = NA_integer_, d = NA_integer_, q = NA_integer_,
                  bp = NA_integer_, bd = NA_integer_, bq = NA_integer_))
  }
  tibble(
    p = as.integer(m[2]),
    d = as.integer(m[3]),
    q = as.integer(m[4]),
    bp = as.integer(m[5]),
    bd = as.integer(m[6]),
    bq = as.integer(m[7])
  )
}

read_jd_sa_sheet <- function(xlsx_path, sheet = "sa") {
  raw <- readxl::read_excel(xlsx_path, sheet = sheet, col_names = FALSE, skip = 1)

  series_raw <- as.character(unlist(raw[1, -1], use.names = FALSE))
  series_clean <- clean_series_name(series_raw)

  dates <- as.Date(raw[-1, 1][[1]])
  values <- raw[-1, -1, drop = FALSE]
  names(values) <- series_clean

  tibble(date = dates) |>
    bind_cols(values) |>
    pivot_longer(-date, names_to = "series", values_to = "jd_value") |>
    mutate(
      series = clean_series_name(series),
      jd_value = as.numeric(jd_value)
    )
}

read_jd_diagnostics <- function(csv_path, selected_cols) {
  raw <- utils::read.delim(
    csv_path,
    sep = ";",
    dec = ",",
    quote = "\"",
    fill = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "None")
  )

  names(raw)[1] <- "series_raw"
  names(raw) <- normalize_jd_colnames(names(raw))

  raw <- raw |>
    dplyr::mutate(
      series = clean_series_name(.data$series_raw)
    )

  if ("mode" %in% names(raw)) {
    raw[["mode"]] <- as.character(raw[["mode"]])
  }

  actual_keep <- resolve_selected_cols(names(raw), selected_cols)

  keep <- c("series", "series_raw", actual_keep)
  keep <- unique(keep[keep %in% names(raw)])

  out <- raw[, keep, drop = FALSE]

  text_cols <- c("series", "series_raw", "mode", "seasonal", "log")
  numeric_candidates <- setdiff(names(out), text_cols)

  for (nm in numeric_candidates) {
    out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
  }

  out
}

extract_sa_ts <- function(fit) {
  sa_ud <- safe_eval(fit$user_defined$sa, NULL)
  if (!is.null(sa_ud)) {
    return(stats::ts(sa_ud, start = start(sa_ud), frequency = frequency(sa_ud)))
  }

  sa_res <- safe_eval(rjd3toolkit::result(fit, "sa"), NULL)
  if (!is.null(sa_res)) return(sa_res)

  stop("Unable to extract the SA series from the fitted object.")
}

fit_workspace_series <- function(y, spec) {
  rjd3tramoseats::tramoseats(y, spec = spec, userdefined = "sa")
}

extract_fit_bundle <- function(fit, y, series_name) {
  sx <- parse_summary_text(fit)
  sa_ts <- extract_sa_ts(fit)

  sa_tbl <- tibble(
    series = series_name,
    date = as.Date(as.yearmon(time(sa_ts))),
    ws_value = as.numeric(sa_ts)
  )

  diag_tbl <- tibble(
    series = series_name,
    ll = sx$ll,
    nparams = sx$nparams,
    nobs = ifelse(is.na(sx$nobs), length(y), sx$nobs),
    neffectiveobs = sx$neffectiveobs,
    df = ifelse(!is.na(sx$neffectiveobs) && !is.na(sx$nparams), sx$neffectiveobs - sx$nparams, NA_integer_),
    mean = sx$mean,
    ntd = sx$ntd,
    nout = sx$nout,
    mode = sx$mode,
    seasonal = sx$seasonal,
    log = sx$log,
    arima_model = sx$arima_model
  ) |>
    bind_cols(parse_arima_orders(sx$arima_model))

  list(sa = sa_tbl, diag = diag_tbl)
}

run_workspace_validation <- function(workspace_xml, refresh_policy = "FreeParameters") {
  jws <- rjd3workspace::jws_open(workspace_xml)
  safe_eval(rjd3workspace::jws_refresh(jws, policy = refresh_policy, info = "All"), NULL)
  rjd3workspace::jws_compute(jws)

  n_sap <- rjd3workspace::ws_sap_count(jws)
  sa_all <- list()
  diag_all <- list()
  idx <- 0L

  for (i in seq_len(n_sap)) {
    sap <- rjd3workspace::jws_sap(jws, i)
    n_sai <- rjd3workspace::sap_sai_count(sap)

    for (j in seq_len(n_sai)) {
      sai <- rjd3workspace::jsap_sai(sap, j)
      mod <- rjd3workspace::read_sai(sai)
      series_name <- clean_series_name(rjd3workspace::sai_name(sai))

      y <- mod$ts$data
      spec <- mod$pointSpec

      if (is.null(spec)) {
        stop(paste("pointSpec is NULL for series:", series_name))
      }

      fit <- fit_workspace_series(y, spec)
      bundle <- extract_fit_bundle(fit, y, series_name)

      idx <- idx + 1L
      sa_all[[idx]] <- bundle$sa
      diag_all[[idx]] <- bundle$diag
    }
  }

  list(
    sa = bind_rows(sa_all),
    diag = bind_rows(diag_all)
  )
}

compare_sa_tables <- function(ws_sa, jd_sa, numeric_tol = 1e-6) {
  ws_sa |>
    inner_join(jd_sa, by = c("series", "date")) |>
    mutate(
      difference = ws_value - jd_value,
      abs_difference = abs(difference),
      is_equal = abs_difference <= numeric_tol
    )
}

compare_diag_tables <- function(ws_diag, jd_diag) {
  ws_diag |>
    inner_join(jd_diag, by = "series", suffix = c("_ws", "_jd")) |>
    mutate(
      arima_model_jd = paste0("(", p_jd, ",", d_jd, ",", q_jd, ") (", bp_jd, ",", bd_jd, ",", bq_jd, ")"),
      arima_model_ws = paste0("(", p_ws, ",", d_ws, ",", q_ws, ") (", bp_ws, ",", bd_ws, ",", bq_ws, ")")
    )
}

build_match_summary <- function(diag_cmp, numeric_tol = 1e-6) {
  n <- nrow(diag_cmp)
  if (n == 0) stop("No matched series were found in the diagnostic comparison table.")

  exact_or_tol <- function(x_ws, x_jd, tol = numeric_tol) {
    if (is.numeric(x_ws) || is.integer(x_ws)) {
      abs(x_ws - x_jd) <= tol
    } else {
      as.character(x_ws) == as.character(x_jd)
    }
  }

  rows <- list(
    list(metric = "ARIMA model", ok = sum(diag_cmp$arima_model_ws == diag_cmp$arima_model_jd, na.rm = TRUE)),
    list(metric = "ll", ok = sum(exact_or_tol(diag_cmp$ll_ws, diag_cmp$ll_jd), na.rm = TRUE)),
    list(metric = "nparams", ok = sum(exact_or_tol(diag_cmp$nparams_ws, diag_cmp$nparams_jd), na.rm = TRUE)),
    list(metric = "nobs", ok = sum(exact_or_tol(diag_cmp$nobs_ws, diag_cmp$nobs_jd), na.rm = TRUE)),
    list(metric = "df", ok = sum(exact_or_tol(diag_cmp$df_ws, diag_cmp$df_jd), na.rm = TRUE)),
    list(metric = "p", ok = sum(exact_or_tol(diag_cmp$p_ws, diag_cmp$p_jd), na.rm = TRUE)),
    list(metric = "d", ok = sum(exact_or_tol(diag_cmp$d_ws, diag_cmp$d_jd), na.rm = TRUE)),
    list(metric = "q", ok = sum(exact_or_tol(diag_cmp$q_ws, diag_cmp$q_jd), na.rm = TRUE)),
    list(metric = "bp", ok = sum(exact_or_tol(diag_cmp$bp_ws, diag_cmp$bp_jd), na.rm = TRUE)),
    list(metric = "bd", ok = sum(exact_or_tol(diag_cmp$bd_ws, diag_cmp$bd_jd), na.rm = TRUE)),
    list(metric = "bq", ok = sum(exact_or_tol(diag_cmp$bq_ws, diag_cmp$bq_jd), na.rm = TRUE)),
    list(metric = "mean", ok = sum(exact_or_tol(diag_cmp$mean_ws, diag_cmp$mean_jd), na.rm = TRUE)),
    list(metric = "ntd", ok = sum(exact_or_tol(diag_cmp$ntd_ws, diag_cmp$ntd_jd), na.rm = TRUE)),
    list(metric = "nout", ok = sum(exact_or_tol(diag_cmp$nout_ws, diag_cmp$nout_jd), na.rm = TRUE)),
    list(metric = "mode", ok = sum(exact_or_tol(diag_cmp$mode_ws, diag_cmp$mode_jd), na.rm = TRUE)),
    list(metric = "seasonal", ok = sum(exact_or_tol(diag_cmp$seasonal_ws, diag_cmp$seasonal_jd), na.rm = TRUE)),
    list(metric = "log", ok = sum(exact_or_tol(diag_cmp$log_ws, diag_cmp$log_jd), na.rm = TRUE))
  )

  bind_rows(rows) |>
    mutate(
      n_series = n,
      agreement_pct = round(100 * ok / n, 2)
    )
}

write_validation_outputs <- function(sa_cmp, diag_cmp, match_summary, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(sa_cmp, file.path(out_dir, "sa_series_comparison.csv"))
  readr::write_csv(diag_cmp, file.path(out_dir, "diagnostics_series_comparison.csv"))
  readr::write_csv(match_summary, file.path(out_dir, "diagnostics_match_summary.csv"))

  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "sa_comparison")
    openxlsx::addWorksheet(wb, "diagnostics_comparison")
    openxlsx::addWorksheet(wb, "match_summary")
    openxlsx::writeData(wb, "sa_comparison", sa_cmp)
    openxlsx::writeData(wb, "diagnostics_comparison", diag_cmp)
    openxlsx::writeData(wb, "match_summary", match_summary)
    openxlsx::saveWorkbook(
      wb,
      file.path(out_dir, "workspace_refresh_validation_outputs.xlsx"),
      overwrite = TRUE
    )
  }
}

# ------------------------------------------------------------
# Execution
# ------------------------------------------------------------

message("Reading JDemetra+ exports...")
jd_sa <- read_jd_sa_sheet(demetra_xlsx, sheet = "sa")
jd_diag <- read_jd_diagnostics(demetra_csv, selected_cols = selected_diag_cols)

message("Running workspace validation...")
ws_results <- run_workspace_validation(
  workspace_xml = workspace_xml,
  refresh_policy = refresh_policy
)

message("Comparing seasonally adjusted series...")
sa_cmp <- compare_sa_tables(
  ws_sa = ws_results$sa,
  jd_sa = jd_sa,
  numeric_tol = numeric_tol
)

message("Comparing diagnostics...")
diag_cmp <- compare_diag_tables(
  ws_diag = ws_results$diag,
  jd_diag = jd_diag
)

message("Building summary table...")
match_summary <- build_match_summary(
  diag_cmp = diag_cmp,
  numeric_tol = numeric_tol
)

message("Writing output files...")
write_validation_outputs(
  sa_cmp = sa_cmp,
  diag_cmp = diag_cmp,
  match_summary = match_summary,
  out_dir = output_dir
)

message("Validation completed successfully.")
