# Appendix: Manipulation checks, frequency reward CAHW — Selection and Incentives in Local Service Provision: Theory and Evidence from Sierra Leone
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.5555_cahw
# Run from the paper's code/ folder: Rscript tab_8.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(patchwork)
library(scales)

make_tab_8 <- function(data){
  tab <- kableExtra::kbl(
    data[[1]],
    digits = 3,
    booktabs = TRUE,
    format = "html",
    col.names = c(" ", "Freq.", "Frac")
  ) |>
    kableExtra::kable_styling(
      full_width = FALSE
    ) |>
    kableExtra::column_spec(2:3, extra_css = "text-align: right; padding-left: 12px; padding-right: 12px;")
  return(tab)
}


make_tab_8(readRDS("../data/sanctioning_plans.rds"))
