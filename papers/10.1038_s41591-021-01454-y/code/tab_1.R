generate_table <- function(data){

  knitr::kable(
    data,
    caption =  "Vaccination beliefs and coverage for the countries in our sample",
    col.names = c("",
                  "Effective","Safe","Important for children to have",
                  "Tuberculosis (BCG)", "Diphtheria, Tetanus and Pertussis (DTP1)",
                  "Measles (MCV1)",
                  "% of parents with any child that was ever vaccinated"),
    format = "html", booktabs = T, linesep = "", align = c("l", rep("c", 7)), label = "otherv") %>%
    kableExtra::kable_styling(full_width = FALSE)  %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::add_header_above(c(" " = 1,
                                   "% Respondents agreeing Vaccines are..." = 3,
                                   "Vaccine coverage in 2019 (% of infants)" = 3,
                                   " " = 1), bold = TRUE) %>%
    kableExtra::column_spec(1:5, width = "9em") %>%
    kableExtra::column_spec(6:7, width = "5em") %>%
    kableExtra::column_spec(8, width = "9em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table 2 presents an overview of vaccination beliefs and incidence across countries in our sample. Columns 2-5 use data from the Wellcome Global Monitor 2018. Column 2 shows the percentage of respondents who are parents and report having had any of their children ever vaccinated. Columns 3-5 show the percentage of all respondents that either strongly agree or somewhat agree with the statement above each column. All percentages are obtained using national weights. Columns 6-8 use data from the World Health Organization on vaccine incidence. Columns 6-8 report the percentage of infants per country receiving the vaccine indicated in each column.",
      threeparttable = T)


}
