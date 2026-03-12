generate_table <- function(data){

  no_vacc2 <-
    data %>%
    dplyr::mutate(estimate = format(estimate, nsmall = 0),
                  conf_int = paste0("(", format(conf.low, nsmall = 0),
                                    ", ", format(conf.high, nsmall = 0), ")")) %>%
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
      group = forcats::fct_relevel(forcats::fct_rev(group), "All", "Russia", "USA", after = Inf)) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(across(c(group, n), ~ifelse(type == "conf_int", "", as.character(.))),
                  group = ifelse(group == "All", "All LMICs", group)) %>%
    dplyr::select(group, n, starts_with("no_vaccine_"), -starts_with("n__no_vaccine"), -type) %>%
    dplyr::relocate("no_vaccine_666", .after = last_col())


  tab_fig2 <-
    no_vacc2 %>%
    knitr::kable(
      col.names =
        c("Study", "N",
          "Concerned about side effects",
          "Concerned about getting coronavirus from the vaccine",
          "Not concerned about getting seriously ill",
          "Doesn't think vaccines are effective",
          "Doesn't think Coronavirus outbreak is as serious as people say",
          "Doesn't like needles",
          "Allergic to vaccines",
          "Won't have time to get vaccinated",
          "Mentions a conspiracy theory",
          "Other reasons"),
      caption = "Reasons not to take the vaccine",
      align = c("l", rep("c", 11)),
      format = "latex", booktabs = T, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE),
      label = "no") %>%
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
                              font_size = base_font_size - 2, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1:12, width = "7em") %>%
    # kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S5 shows percentage of respondents mentioning reasons why they would not take the Covid-19 vaccine. The number of observations and percentage correponds only to people who would NOT take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)  %>%
    kableExtra::landscape()

  no_vacc2 %>%
    knitr::kable(
      col.names = c("Study", "N",
                    "Concerned about side effects",
                    "Concerned about getting coronavirus from the vaccine",
                    "Not concerned about getting seriously ill",
                    "Doesn't think vaccines are effective",
                    "Doesn't think Coronavirus outbreak is as serious as people say",
                    "Doesn't like needles",
                    "Allergic to vaccines",
                    "Won't have time to get vaccinated",
                    "Mentions a conspiracy theory",
                    "Other reasons"),
      caption = "\\label{no}Reasons not to take the vaccine",
      booktabs = T, linesep = "",
      format.args = list(big.mark = ",",scientific = FALSE),
      align = c("l", rep("c", 11))) %>%
    kableExtra::kable_styling(full_width = F) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1:12, width = "7em") %>%
    # kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S5 shows percentage of respondents mentioning reasons why they would not take the Covid-19 vaccine. The number of observations and percentage correponds only to people who would NOT take the vaccine. Respondents in all countries could give more than one reason. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)

}
