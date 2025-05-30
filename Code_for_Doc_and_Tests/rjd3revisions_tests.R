# rjd3revisions

# ## Installation
# ``` r
# remotes::install_github("rjdverse/rjd3revisions")
# ```
#

library(rjd3revisions)
browseVignettes("rjd3revisions") # how does this work ?

# Example
df <- data.frame(
    rev_date = c(
        rep("2022-07-31", 4), rep("2022-08-31", 4),
        rep("2022-09-30", 4), rep("2022-10-31", 4),
        rep("2022-11-30", 4), rep("2022-12-31", 4),
        rep("2023-01-31", 4), rep("2023-02-28", 4)
    ),
    time_period = c(rep(c("2022Q1", "2022Q2", "2022Q3", "2022Q4"), 8)),
    obs_values = c(
        .8, .2, NA, NA, .8, .1, NA, NA,
        .7, .1, NA, NA, .7, .2, .5, NA,
        .7, .2, .5, NA, .7, .3, .7, NA,
        .7, .2, .7, .4, .7, .3, .7, .3
    )
)

# Then you can create your vintages, inspect revisions if you want and make the analysis

library("rjd3revisions")

vintages <- create_vintages(df, periodicity = 4)
vintages$revisions$vertical_view
revisions <- get_revisions(vintages, gap = 2)
revisions$horizontal_view
revisions$
    rslt <- revision_analysis(vintages, gap = 1, view = "diagonal", n.releases = 3)


# Finally to create a report and get a summary of the results, you can use

# render_report(rslt, output_format = "html_document")

summary(rslt)
print(rslt)
plot(rslt)

## Additional information
# This README.md file gives you an example of how to proceed. Then, you can consult
# the documentation of each function separately
# (for example: `?create_vintages`, `?revision_analysis`) to see the current possibilities
# of the tool. Finally, the function `get_report()` generates a report that includes a summary of the results but also explanation about all the tests being performed.
#
