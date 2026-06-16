generate_table <- function(data){
tabd1 <- 
  data |>
  mutate(
    mean_difference = add_star(mean_difference, mean_difference_se),
    mean_difference_ps = add_star(mean_difference_ps, mean_difference_se_ps)
  ) |>
  kbl(
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
  kable_styling(latex_options="scale_down") |>
  add_header_above(
    c(" " = 1, "Community" = 2, "Paramount Chief" = 2, "Difference" = 2,
      "Community" = 2, "Paramount Chief" = 2, "Difference" = 2), escape = F
  ) |>
  add_header_above(
    c(" " = 1, "All 287 with CAHWs" = 6, "Subset only" = 6)
  ) |>
  pack_rows("CAHW Survey", 1, 17) %>% 
  pack_rows("Household Survey", 18, 30) %>% 
  pack_rows("Training", 31, 32) %>% 
  row_spec(17, extra_latex_after = "\\midrule") %>% 
  row_spec(30, extra_latex_after = "\\midrule") %>% 
  footnote(general = "CAHW Surveys.", footnote_as_chunk = TRUE, threeparttable = TRUE)

return(tabd1)
}