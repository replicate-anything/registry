generate_table <- function(data){
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