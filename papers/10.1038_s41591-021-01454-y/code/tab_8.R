generate_table <- function(data){

  yes_vars <-
    data %>%
    dplyr::select(yes_vaccine_1, yes_vaccine_2, yes_vaccine_3) %>%
    names

  ## Generate data for analysis of yes reasons for different age groups
  yes_vacc_age_1 <-
    lapply(yes_vars, age_analysis, df = data, num = "Yes", filter_by = `age_less24`) %>%
    dplyr::bind_rows() %>%
    mutate(age = "<25")

  yes_vacc_age_2 <-
    lapply(yes_vars, age_analysis, df = data, num = "Yes", filter_by = `age_25_54`) %>%
    dplyr::bind_rows() %>%
    mutate(age = "25-54")

  yes_vacc_age_3 <-
    lapply(yes_vars, age_analysis, df = data, num = "Yes", filter_by = `age_55_more`) %>%
    dplyr::bind_rows() %>%
    mutate(age = "55+")

  #Get percentage per yes reason category and make wide table
  yes_vacc_age <-
    rbind(yes_vacc_age_1, yes_vacc_age_2, yes_vacc_age_3) %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 0))) %>%
    dplyr::mutate(estimate = format(estimate, nsmall = 0),
                  conf_int = paste0("(", conf.low,
                                    ", ", conf.high, ")"),
                  n = as.character(n)) %>%
    dplyr::select(group, estimate, conf_int, outcome, age, n) %>%
    tidyr::pivot_wider(names_from = c(outcome, age), values_from = c(estimate, conf_int, n), names_sep = "__") %>%
    tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__"), starts_with("n__")),
                        names_to = c("type", ".value"),
                        names_pattern = "(.*)__(.*)")

  y1 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_1", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))


  y2 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_2", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))

  y3 <- yes_vacc_age %>%
    dplyr::filter(grepl("yes_vaccine_3", type)) %>%
    dplyr::mutate(type = ifelse(grepl("estimate", type), "estimate", type),
                  type = ifelse(grepl("conf_int", type), "conf_int", type),
                  type = ifelse(grepl("n__", type), "n", type))

  yes_vacc_age <-
    dplyr::left_join(y1, y2, by = c("group", "type")) %>%
    dplyr::left_join(y3, by = c("group", "type")) %>%
    dplyr::mutate(
      group = forcats::fct_relevel(as.factor(group), "All", "Russia", "USA", after = Inf)) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(
      across(c(group),
             ~ifelse(type == "conf_int", "Conf. interval", as.character(.))),
      across(c(group), ~ifelse(type == "n", "n", as.character(.))),
      group = ifelse(group == "All", "All LMICs", group)) %>%
    dplyr::select(-type)

  cnames <- c("Study", "<25", "25-54", "55+", "<25", "25-54", "55+", "<25", "25-54", "55+")


  #Table to Latex
  tab_reasons_y_age <-
    yes_vacc_age %>%
    knitr::kable(
      col.names = cnames,
      caption = "Reasons to take the vaccine- by age groups",
      format = "latex", booktabs = T,
      linesep = "", label = "yes1",
      format.args = list(big.mark = ",", scientific = FALSE),
      escape = F, align = c("l", rep("c", 9))) %>%
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
                              font_size = base_font_size - 2, full_width = FALSE) %>%
    kableExtra::add_header_above(c(" " = 1, "Self" = 3, "Family" = 3, "Community" = 3),
                                 bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::row_spec(seq(from = 3, to = 36, by = 3), hline_after = TRUE) %>%
    kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::column_spec(2:10, width = "5em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S4 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine by age groups. The number of observations and percentage correponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)


  yes_vacc_age %>%
    knitr::kable(
      col.names = cnames,
      caption = "\\label{yes1}Reasons to take the vaccine",
      booktabs = T, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE),
      align = c("l", rep("c", 9))) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::add_header_above(c(" " = 1, "Self" = 3, "Family" = 3, "Community" = 3),
                                 bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "8em") %>%
    kableExtra::column_spec(2:10, width = "4em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S4 shows percentage of respondents mentioning reasons why they would take the Covid-19 vaccine by age groups. The number of observations and percentage correponds only to people who would take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)


}
