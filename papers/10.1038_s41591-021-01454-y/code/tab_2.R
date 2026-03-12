generate_table <- function(data){

  tab_sampling <-
    readxl::read_excel(data, sheet = "sample") %>%
    dplyr::select("Study" = "country", "Date" = "date",
                  "Geographic scope", "Sampling methodology", "Survey modality", "Weights") %>%
    knitr::kable(
      caption =  "Summary of studies sampling",
      format = "latex", booktabs = T, linesep = "", label = "sampling") %>%
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
                              font_size = base_font_size - 2) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1:2, width = "8em") %>%
    kableExtra::column_spec(3, width = "12em")  %>%
    kableExtra::column_spec(4, width = "30em") %>%
    kableExtra::landscape()

  readxl::read_excel(data, sheet = "sample") %>%
    dplyr::select("Study" = "country", "Date" = "date",
                  "Geographic scope", "Sampling methodology", "Survey modality", "Weights") %>%
    knitr::kable(caption =  "Summary of studies' sampling", linesep = "", format = "html") %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1:2, width = "8em") %>%
    kableExtra::column_spec(3, width = "12em")  %>%
    kableExtra::column_spec(4, width = "30em") %>%
    kableExtra::landscape()

}
