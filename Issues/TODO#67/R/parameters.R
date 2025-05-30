cruncher_items <- c(
    "period",
    "span.start",
    "span.end",
    "span.n",
    "span.missing",
    "espan.start",
    "espan.end",
    "espan.n",
    "log",
    "adjust",
    "regression.lp",
    "regression.ntd",
    "regression.nmh",
    "regression.td-derived",
    "regression.td-ftest",
    "regression.easter",
    "regression.nout",
    "regression.noutao",
    "regression.noutls",
    "regression.nouttc",
    "regression.noutso",
    "regression.td(*)",
    "regression.out(*)",
    "regression.user(*)",
    "likelihood.neffectiveobs",
    "likelihood.np",
    "likelihood.logvalue",
    "likelihood.adjustedlogvalue",
    "likelihood.ssqerr",
    "likelihood.aic",
    "likelihood.aicc",
    "likelihood.bic",
    "likelihood.bicc",
    "residuals.ser",
    "residuals.ser-ml",
    "residuals.mean",
    "residuals.skewness:3",
    "residuals.kurtosis:3",
    "residuals.dh",
    "residuals.lb",
    "residuals.lb2:3",
    "residuals.seaslb",
    "residuals.bp",
    "residuals.bp2",
    "residuals.seasbp",
    "residuals.nudruns",
    "residuals.ludruns",
    "residuals.nruns",
    "residuals.lruns",
    "arima",
    "arima.mean",
    "arima.p",
    "arima.d",
    "arima.q",
    "arima.bp",
    "arima.bd",
    "arima.bq",
    "arima.phi(*)",
    "arima.bphi(*)",
    "arima.th(*)",
    "arima.bth(*)",
    "decomposition.seasonality",
    "decomposition.parameters_cutoff",
    "decomposition.model_changed",
    "decomposition.tvar-estimator",
    "decomposition.tvar-estimate",
    "decomposition.tvar-pvalue",
    "decomposition.savar-estimator",
    "decomposition.savar-estimate",
    "decomposition.savar-pvalue",
    "decomposition.svar-estimator",
    "decomposition.svar-estimate",
    "decomposition.svar-pvalue",
    "decomposition.ivar-estimator",
    "decomposition.ivar-estimate",
    "decomposition.ivar-pvalue",
    "decomposition.tscorr-estimator",
    "decomposition.tscorr-estimate",
    "decomposition.tscorr-pvalue",
    "decomposition.ticorr-estimator",
    "decomposition.ticorr-estimate",
    "decomposition.ticorr-pvalue",
    "decomposition.sicorr-estimator",
    "decomposition.sicorr-estimate",
    "decomposition.sicorr-pvalue",
    "decomposition.ar_root(*)",
    "decomposition.ma_root(*)",
    "method",
    "variancedecomposition.cycle",
    "variancedecomposition.seasonality",
    "variancedecomposition.irregular",
    "variancedecomposition.tdh",
    "variancedecomposition.others",
    "variancedecomposition.total",
    "diagnostics.logstat",
    "diagnostics.levelstat",
    "diagnostics.fcast-insample-mean",
    "diagnostics.fcast-outsample-mean",
    "diagnostics.fcast-outsample-variance",
    "diagnostics.seas-lin-f",
    "diagnostics.seas-lin-qs",
    "diagnostics.seas-lin-kw",
    "diagnostics.seas-lin-friedman",
    "diagnostics.seas-lin-periodogram",
    "diagnostics.seas-lin-spectralpeaks",
    "diagnostics.seas-si-combined",
    "diagnostics.seas-si-evolutive",
    "diagnostics.seas-si-stable",
    "diagnostics.seas-res-f",
    "diagnostics.seas-res-qs",
    "diagnostics.seas-res-kw",
    "diagnostics.seas-res-friedman",
    "diagnostics.seas-res-periodogram",
    "diagnostics.seas-res-spectralpeaks",
    "diagnostics.seas-res-combined",
    "diagnostics.seas-res-combined3",
    "diagnostics.seas-res-evolutive",
    "diagnostics.seas-res-stable",
    "diagnostics.seas-i-f",
    "diagnostics.seas-i-qs",
    "diagnostics.seas-i-kw",
    "diagnostics.seas-i-periodogram",
    "diagnostics.seas-i-spectralpeaks",
    "diagnostics.seas-i-combined",
    "diagnostics.seas-i-combined3",
    "diagnostics.seas-i-evolutive",
    "diagnostics.seas-i-stable",
    "diagnostics.seas-sa-f",
    "diagnostics.seas-sa-qs",
    "diagnostics.seas-sa-kw",
    "diagnostics.seas-sa-friedman",
    "diagnostics.seas-sa-periodogram",
    "diagnostics.seas-sa-spectralpeaks",
    "diagnostics.seas-sa-combined",
    "diagnostics.seas-sa-combined3",
    "diagnostics.seas-sa-evolutive",
    "diagnostics.seas-sa-stable",
    "diagnostics.seas-sa-ac1",
    "diagnostics.td-sa-all",
    "diagnostics.td-sa-last",
    "diagnostics.td-i-all",
    "diagnostics.td-i-last",
    "diagnostics.td-res-all",
    "diagnostics.td-res-last",
    "diagnostics.ic-ratio-henderson",
    "diagnostics.ic-ratio",
    "diagnostics.msr-global",
    "diagnostics.msr(*)",
    "decomposition.trendfilter",
    "decomposition.seasfilter",
    "m-statistics.m1",
    "m-statistics.m2",
    "m-statistics.m3",
    "m-statistics.m4",
    "m-statistics.m5",
    "m-statistics.m6",
    "m-statistics.m7",
    "m-statistics.m8",
    "m-statistics.m9",
    "m-statistics.m10",
    "m-statistics.m11",
    "m-statistics.q",
    "m-statistics.q-m2",
    "diagnostics.basic checks.definition:2",
    "diagnostics.basic checks.annual totals:2",
    "diagnostics.visual spectral analysis.spectral seas peaks",
    "diagnostics.visual spectral analysis.spectral td peaks",
    "diagnostics.regarima residuals.normality:2",
    "diagnostics.regarima residuals.independence:2",
    "diagnostics.regarima residuals.spectral td peaks:2",
    "diagnostics.regarima residuals.spectral seas peaks:2",
    "diagnostics.outliers.number of outliers:2",
    "diagnostics.out-of-sample.mean:2",
    "diagnostics.out-of-sample.mse:2",
    "diagnostics.m-statistics.q:2",
    "diagnostics.m-statistics.q-m2:2",
    "diagnostics.seats.seas variance:2",
    "diagnostics.seats.irregular variance:2",
    "diagnostics.seats.seas/irr cross-correlation:2",
    "diagnostics.residual seasonality tests.qs test on sa:2",
    "diagnostics.residual seasonality tests.qs test on i:2",
    "diagnostics.residual seasonality tests.f-test on sa (seasonal dummies):2",
    "diagnostics.residual seasonality tests.f-test on i (seasonal dummies):2",
    "diagnostics.combined seasonality test.combined seasonality test on sa:2",
    "diagnostics.combined seasonality test.combined seasonality test on sa (last 3 years):2",
    "diagnostics.combined seasonality test.combined seasonality test on irregular:2",
    "diagnostics.residual trading days tests.f-test on sa (td):2",
    "diagnostics.residual trading days tests.f-test on i (td):2",
    "diagnostics.quality"
)

v3_GUI_items <- c(
    "period",
    "span.start",
    "span.end",
    "span.n",
    "span.missing",
    "regression.espan.start",
    "regression.espan.end",
    "regression.espan.n",
    "regression.espan.missing",
    "log",
    "adjust",
    "regression.mean",
    "regression.nlp",
    "regression.lp",
    "regression.ntd",
    "regression.nmh",
    "regression.td-derived",
    "regression.td-ftest",
    "regression.easter",
    "regression.leaster",
    "regression.nout",
    "regression.nao",
    "regression.nls",
    "regression.ntc",
    "regression.nso",
    "regression.td(*)",
    "regression.outlier(*)",
    "regression.user(*)",
    "regression.nusers",
    "regression.mu",
    "regression.missing(*)",
    "likelihood.neffectiveobs",
    "likelihood.nparams",
    "likelihood.ll",
    "likelihood.adjustedll",
    "likelihood.ssqerr",
    "likelihood.aic",
    "likelihood.bic",
    "likelihood.aicc",
    "likelihood.bicc",
    "likelihood.hannanquinn",
    "likelihood.df",
    "likelihood.nobs",
    "likelihood.bic2",
    "residuals.ser",
    "residuals.type",
    "residuals.mean",
    "residuals.skewness",
    "residuals.kurtosis",
    "residuals.doornikhansen",
    "residuals.lb",
    "residuals.lb2",
    "residuals.seaslb",
    "residuals.bp",
    "residuals.bp2",
    "residuals.seasbp",
    "residuals.nudruns",
    "residuals.ludruns",
    "residuals.nruns",
    "residuals.lruns",
    "arima.p",
    "arima.d",
    "arima.q",
    "arima.bp",
    "arima.bd",
    "arima.bq",
    "arima.phi(*)",
    "arima.bphi(*)",
    "arima.theta(*)",
    "arima.btheta(*)",
    "mode",
    "seasonal",
    "diagnostics.seas-lin-qs",
    "diagnostics.seas-lin-f",
    "diagnostics.seas-lin-friedman",
    "diagnostics.seas-lin-kw",
    "diagnostics.seas-lin-periodogram",
    "diagnostics.seas-lin-spectralpeaks",
    "diagnostics.seas-lin-combined",
    "diagnostics.seas-lin-evolutive",
    "diagnostics.seas-lin-stable",
    "diagnostics.seas-si-combined",
    "diagnostics.seas-si-combined3",
    "diagnostics.seas-si-evolutive",
    "diagnostics.seas-si-stable",
    "diagnostics.seas-res-qs",
    "diagnostics.seas-res-f",
    "diagnostics.seas-res-kw",
    "diagnostics.seas-res-friedman",
    "diagnostics.seas-res-periodogram",
    "diagnostics.seas-res-spectralpeaks",
    "diagnostics.seas-res-combined",
    "diagnostics.seas-res-combined3",
    "diagnostics.seas-res-evolutive",
    "diagnostics.seas-res-stable",
    "diagnostics.seas-i-f",
    "diagnostics.seas-i-qs",
    "diagnostics.seas-i-kw",
    "diagnostics.seas-i-friedman",
    "diagnostics.seas-i-periodogram",
    "diagnostics.seas-i-spectralpeaks",
    "diagnostics.seas-i-combined",
    "diagnostics.seas-i-combined3",
    "diagnostics.seas-i-evolutive",
    "diagnostics.seas-i-stable",
    "diagnostics.seas-sa-f",
    "diagnostics.seas-sa-qs",
    "diagnostics.seas-sa-kw",
    "diagnostics.seas-sa-friedman",
    "diagnostics.seas-sa-periodogram",
    "diagnostics.seas-sa-spectralpeaks",
    "diagnostics.seas-sa-combined",
    "diagnostics.seas-sa-combined3",
    "diagnostics.seas-sa-evolutive",
    "diagnostics.seas-sa-stable",
    "diagnostics.seas-sa-ac1",
    "diagnostics.td-sa-all",
    "diagnostics.td-sa-last",
    "diagnostics.td-i-all",
    "diagnostics.td-i-last",
    "diagnostics.td-res-all",
    "diagnostics.td-res-last",
    "decomposition.icratio",
    "decomposition.d9-global-msr",
    "decomposition.trend-filter",
    "decomposition.seasonal-filters",
    "m-statistics.m1",
    "m-statistics.m2",
    "m-statistics.m3",
    "m-statistics.m4",
    "m-statistics.m5",
    "m-statistics.m6",
    "m-statistics.m7",
    "m-statistics.m8",
    "m-statistics.m9",
    "m-statistics.m10",
    "m-statistics.m11",
    "m-statistics.q",
    "m-statistics.q-m2",
    "quality.summary"
)

v2_GUI_items <- c(
    "period",
    "span.start",
    "span.end",
    "span.n",
    "span.missing",
    "espan.start",
    "espan.end",
    "espan.n",
    "log",
    "adjust",
    "regression.lp",
    "regression.ntd",
    "regression.nmh",
    "regression.td-derived",
    "regression.td-ftest",
    "regression.easter",
    "regression.nout",
    "regression.noutao",
    "regression.noutls",
    "regression.nouttc",
    "regression.noutso",
    "regression.td(*)",
    "regression.out(*)",
    "regression.user(*)",
    "likelihood.neffectiveobs",
    "likelihood.np",
    "likelihood.logvalue",
    "likelihood.adjustedlogvalue",
    "likelihood.ssqerr",
    "likelihood.aic",
    "likelihood.aicc",
    "likelihood.bic",
    "likelihood.bicc",
    "residuals.ser",
    "residuals.ser-ml",
    "residuals.mean",
    "residuals.skewness",
    "residuals.kurtosis",
    "residuals.dh",
    "residuals.lb",
    "residuals.lb2",
    "residuals.seaslb",
    "residuals.bp",
    "residuals.bp2",
    "residuals.seasbp",
    "residuals.nudruns",
    "residuals.ludruns",
    "residuals.nruns",
    "residuals.lruns",
    "arima",
    "arima.mean",
    "arima.p",
    "arima.d",
    "arima.q",
    "arima.bp",
    "arima.bd",
    "arima.bq",
    "arima.phi(*)",
    "arima.bphi(*)",
    "arima.th(*)",
    "arima.bth(*)",
    "decomposition.seasonality",
    "decomposition.parameters_cutoff",
    "decomposition.model_changed",
    "decomposition.tvar-estimator",
    "decomposition.tvar-estimate",
    "decomposition.tvar-pvalue",
    "decomposition.savar-estimator",
    "decomposition.savar-estimate",
    "decomposition.savar-pvalue",
    "decomposition.svar-estimator",
    "decomposition.svar-estimate",
    "decomposition.svar-pvalue",
    "decomposition.ivar-estimator",
    "decomposition.ivar-estimate",
    "decomposition.ivar-pvalue",
    "decomposition.tscorr-estimator",
    "decomposition.tscorr-estimate",
    "decomposition.tscorr-pvalue",
    "decomposition.ticorr-estimator",
    "decomposition.ticorr-estimate",
    "decomposition.ticorr-pvalue",
    "decomposition.sicorr-estimator",
    "decomposition.sicorr-estimate",
    "decomposition.sicorr-pvalue",
    "decomposition.ar_root(*)",
    "decomposition.ma_root(*)",
    "method",
    "variancedecomposition.cycle",
    "variancedecomposition.seasonality",
    "variancedecomposition.irregular",
    "variancedecomposition.tdh",
    "variancedecomposition.others",
    "variancedecomposition.total",
    "diagnostics.logstat",
    "diagnostics.levelstat",
    "diagnostics.fcast-insample-mean",
    "diagnostics.fcast-outsample-mean",
    "diagnostics.fcast-outsample-variance",
    "diagnostics.seas-lin-f",
    "diagnostics.seas-lin-qs",
    "diagnostics.seas-lin-kw",
    "diagnostics.seas-lin-friedman",
    "diagnostics.seas-lin-periodogram",
    "diagnostics.seas-lin-spectralpeaks",
    "diagnostics.seas-si-combined",
    "diagnostics.seas-si-evolutive",
    "diagnostics.seas-si-stable",
    "diagnostics.seas-res-f",
    "diagnostics.seas-res-qs",
    "diagnostics.seas-res-kw",
    "diagnostics.seas-res-friedman",
    "diagnostics.seas-res-periodogram",
    "diagnostics.seas-res-spectralpeaks",
    "diagnostics.seas-res-combined",
    "diagnostics.seas-res-combined3",
    "diagnostics.seas-res-evolutive",
    "diagnostics.seas-res-stable",
    "diagnostics.seas-i-f",
    "diagnostics.seas-i-qs",
    "diagnostics.seas-i-kw",
    "diagnostics.seas-i-periodogram",
    "diagnostics.seas-i-spectralpeaks",
    "diagnostics.seas-i-combined",
    "diagnostics.seas-i-combined3",
    "diagnostics.seas-i-evolutive",
    "diagnostics.seas-i-stable",
    "diagnostics.seas-sa-f",
    "diagnostics.seas-sa-qs",
    "diagnostics.seas-sa-kw",
    "diagnostics.seas-sa-friedman",
    "diagnostics.seas-sa-periodogram",
    "diagnostics.seas-sa-spectralpeaks",
    "diagnostics.seas-sa-combined",
    "diagnostics.seas-sa-combined3",
    "diagnostics.seas-sa-evolutive",
    "diagnostics.seas-sa-stable",
    "diagnostics.seas-sa-ac1",
    "diagnostics.td-sa-all",
    "diagnostics.td-sa-last",
    "diagnostics.td-i-all",
    "diagnostics.td-i-last",
    "diagnostics.td-res-all",
    "diagnostics.td-res-last",
    "diagnostics.ic-ratio-henderson",
    "diagnostics.ic-ratio",
    "diagnostics.msr-global",
    "diagnostics.msr(*)",
    "decomposition.trendfilter",
    "decomposition.seasfilter",
    "m-statistics.m1",
    "m-statistics.m2",
    "m-statistics.m3",
    "m-statistics.m4",
    "m-statistics.m5",
    "m-statistics.m6",
    "m-statistics.m7",
    "m-statistics.m8",
    "m-statistics.m9",
    "m-statistics.m10",
    "m-statistics.m11",
    "m-statistics.q",
    "m-statistics.q-m2",
    "diagnostics.basic checks.definition:2",
    "diagnostics.basic checks.annual totals:2",
    "diagnostics.regarima residuals.normality:2",
    "diagnostics.regarima residuals.independence:2",
    "diagnostics.regarima residuals.spectral td peaks:2",
    "diagnostics.regarima residuals.spectral seas peaks:2",
    "diagnostics.outliers.number of outliers:2",
    "diagnostics.m-statistics.q:2",
    "diagnostics.m-statistics.q-m2:2",
    "diagnostics.seats.seas variance:2",
    "diagnostics.seats.irregular variance:2",
    "diagnostics.seats.seas/irr cross-correlation:2",
    "diagnostics.residual trading days tests.f-test on sa (td):2",
    "diagnostics.residual trading days tests.f-test on i (td):2",
    "diagnostics.residual seasonality tests.qs test on sa:2",
    "diagnostics.residual seasonality tests.qs test on i:2",
    "diagnostics.residual seasonality tests.f-test on sa (seasonal dummies):2",
    "diagnostics.residual seasonality tests.f-test on i (seasonal dummies):2"
)
