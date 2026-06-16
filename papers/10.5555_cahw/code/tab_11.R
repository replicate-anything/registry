generate_table <- function(data){
  
  tbl_formatted <- data
  num_cols <- 2:11
  # Format all but last row with 2 decimals
  tbl_formatted[-nrow(tbl_formatted), num_cols] <- lapply(
    tbl_formatted[-nrow(tbl_formatted), num_cols],
    function(x) sprintf("%.2f", as.numeric(x))
  )
  
  # Format last row with 0 decimals
  tbl_formatted[nrow(tbl_formatted), num_cols] <- lapply(
    tbl_formatted[nrow(tbl_formatted), num_cols],
    function(x) sprintf("%.0f", as.numeric(x))
  )
  
  # Make sure the first column (e.g. names) remains character
  tbl_formatted[[1]] <- as.character(tbl_formatted[[1]])
  
  tab_only <-
    tbl_formatted |>
    kable(
      col.names = c(
        "", "Mean", "(sd)", "Mean", "(sd)", "Mean", "(sd)", "Mean", "(sd)",
        "F-statistic", "p-value"
      ),
      booktabs = TRUE, escape = FALSE, digits = 2, format = "html"
      # no longtable, no caption => plain tabular
    ) |>
    add_header_above(
      c(" " = 1, "No incentive" = 2, "Community Monitoring" = 2,
        "Pay for Performance" = 2, "Community Monitoring + P4P" = 2,
        "Differences" = 2)
    ) |>
    add_header_above(c(" " = 1, "All 287 with CAHWs" = 10)) |>
    row_spec(17, hline_after = T)
  
  
  # Optional: scale without introducing a table float
  tab_only_scaled <- paste0("\\begingroup\n\\resizebox{\\linewidth}{!}{%\n", tab_only, "\n}%\n\\endgroup\n")
  
  return(tab_only)
}
