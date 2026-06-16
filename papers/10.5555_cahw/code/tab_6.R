# Appendix: Manipulation checks — Selection and Incentives in Local Service Provision: Theory and Evidence from Sierra Leone
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.5555_cahw
# Run from the paper's code/ folder: Rscript tab_6.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(patchwork)
library(scales)

make_tab_6 <- function(data){
tabd1 <- 
  data |>
  dplyr::mutate(
    mean_difference = add_star(mean_difference, mean_difference_se),
    mean_difference_ps = add_star(mean_difference_ps, mean_difference_se_ps)
  ) |>
  kableExtra::kbl(
    col.names = c(
      "",  # First column
      "Mean", "(sd)",
      "Mean", "(sd)",
      "Mean", "(se)",
      "Mean", "(sd)",
      "Mean", "(sd)",
      "Mean", "(se)"
    ),
    booktabs = TRUE, escape = FALSE, digits = 2, format = "html",
    caption = "\\label{table_manipcheck_ts} CAHWs at Baseline (by Recruitment Experiment, $T_s$)"
  ) |>
  kableExtra::kable_styling(latex_options="scale_down") |>
  kableExtra::add_header_above(
    c(" " = 1, "Community" = 2, "Paramount Chief" = 2, "Difference" = 2,
      "Community" = 2, "Paramount Chief" = 2, "Difference" = 2), escape = F
  ) |>
  kableExtra::add_header_above(
    c(" " = 1, "All 287 with CAHWs" = 6, "Subset only" = 6)
  ) |>
  kableExtra::pack_rows("CAHW Survey", 1, 17) %>% 
  kableExtra::pack_rows("Household Survey", 18, 30) %>% 
  kableExtra::pack_rows("Training", 31, 32) %>% 
  kableExtra::row_spec(17, extra_latex_after = "\\midrule") %>% 
  kableExtra::row_spec(30, extra_latex_after = "\\midrule") %>% 
  kableExtra::footnote(general = "CAHW Surveys.", footnote_as_chunk = TRUE, threeparttable = TRUE)

return(tabd1)
}


make_tab_6(readRDS("../data/tab_baseline.rds"))
