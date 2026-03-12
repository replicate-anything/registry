generate_table <- function(data){

  #There are idiosyncratic reasons why people would take the vaccine. I recoded them. But we keep only the core, which is common almost in all studies.
  yes_vars <-
    data %>%
    dplyr::select(yes_vaccine_1, yes_vaccine_2, yes_vaccine_3) %>%
    names

  ## Generate data for analysis of yes reasons
  yes_vacc1 <-
    lapply(yes_vars, reasons_together, df = data, num = "Yes") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 0)))

  #Get percentage per yes reason category and make wide table
  yes_vacc2 <-
    yes_vacc1 %>%
    dplyr::mutate(estimate = format(estimate, nsmall = 0),
                  conf_int = paste0("(", conf.low,
                                    ", ", conf.high, ")")) %>%
    dplyr::select(group, estimate, conf_int, outcome, n) %>%
    tidyr::pivot_wider(names_from = outcome,
                       values_from = c(estimate, conf_int, n),
                       names_sep = "__") %>%
    tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__")),
                        names_to = c("type", ".value"),
                        names_pattern = "(.*)__(.*)") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n = ifelse(group == "All", NA, unique(na.omit(c_across(starts_with("n__")))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      group = forcats::fct_relevel(as.factor(group), "All", "Russia", "USA", after = Inf)) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(across(c(group, n), ~ifelse(type == "conf_int", "", as.character(.))),
                  group = ifelse(group == "All", "All LMICs", group)) %>%
    dplyr::select(group, n, type, starts_with("yes_vaccine_"), -starts_with("n_yes_vaccine"), -type)

  cnames <- c("Study", "N", "Self", "Family",
              "Community")


  #Table to Latex
  tab_reasons_y <-
    yes_vacc2 %>%
    knitr::kable(
      col.names = cnames,
      caption = "Reasons to take the vaccine", format = "latex", booktabs = T,
      linesep = "", label = "yes",
      format.args = list(big.mark = ",", scientific = FALSE),
      escape = F, align = "lcccc") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              font_size = base_font_size - 2) %>%
    kableExtra::add_header_above(c(" " = 2, "Protection" = 3), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "8em") %>%
    kableExtra::column_spec(2:4, width = "4em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S2 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine. The number of observations and percentage corresponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses. Studies India, Pakistan 1 and Pakistan 2 are not included because they either did not include the question or were not properly harmonized with the other studies.",
      threeparttable = T)

  yes_vacc2 %>%
    knitr::kable(
      col.names = cnames,
      caption = "\\label{yes}Reasons to take the vaccine",
      booktabs = T, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE),
      align = "lcccc") %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::add_header_above(c(" " = 2, "Protection" = 3), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "8em") %>%
    kableExtra::column_spec(2:5, width = "8em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S2 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine. The number of observations and percentage correponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses. Studies India, Pakistan 1 and Pakistan 2 are not included because they either did not include the question or were not properly harmonized with the other studies.",
      threeparttable = T)
}
