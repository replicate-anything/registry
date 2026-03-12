generate_table <- function(data){

  trust_vacc <-
    plyr::ldply(
      .data = list("Yes", "No", "All"),
      .fun = function(take_vac) {
        list(Yes = "Yes",
             No = c("No", "DK"),
             All = c("Yes", "No", "DK")) %>%
          .[[take_vac]] %>%
          plyr::ldply(trust_names, reasons_together, df = data, num = .) %>%
          dplyr::mutate(
            across(c(conf.low, conf.high, estimate),
                   ~ format(round(. * 100, digits = 1), nsmall = 1)),
            conf_int = paste0("(", conf.low, ", ", conf.high, ")")) %>%
          dplyr::select(group, estimate, conf_int, outcome, n) %>%
          tidyr::pivot_wider(names_from = outcome, values_from = c(estimate, conf_int, n),
                             names_sep = "__") %>%
          tidyr::pivot_longer(cols = c(starts_with("estimate__"), starts_with("conf_int__")),
                              names_to = c("type", ".value"),
                              names_pattern = "(.*)__(.*)") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            n = ifelse(group == "All", NA, unique(na.omit(c_across(starts_with("n__")))))) %>%
          dplyr::ungroup() %>%
          dplyr::mutate("Take vaccine?" = take_vac) %>%
          dplyr::select(group, n, type, "Take vaccine?", starts_with("trust_")) %>%
          dplyr::filter(!(group %in% c("Mozambique", "Pakistan 1",
                                       "Pakistan 2", "Uganda 1", "India")))
      }) %>%
    dplyr::mutate(
      group = as.factor(group),
      group = forcats::fct_relevel(group, "All", "Russia", "USA", after = Inf)) %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(across(c(group, n, `Take vaccine?`), ~ifelse(type == "conf_int", "", as.character(.))),
                  group = ifelse(group == "All", "All LMICs", group)) %>%
    dplyr::select(-type)

  tab_trust <-
    trust_vacc %>%
    dplyr::select("group", "n",
                  "Take vaccine?", "trust_vaccine_5",
                  "trust_vaccine_6", "trust_recode_1",
                  "trust_recode_3", "trust_recode_2",
                  "trust_recode_4", "trust_recode_5") %>%
    knitr::kable(
      col.names = c("Study", "N", "Take vaccine?", "Health workers",
                    "Government or Ministry of Health",
                    "Family or friends",
                    "Famous person, religious leader or traditional healers",
                    "Newspapers, radio or online groups", "Other",
                    "Don't know or Refuse"),
      caption = "COVID-19 Vaccination Decision-making: most trusted source",
      align = c("l", rep("c", 9)),
      format = "latex", booktabs = T, linesep = "", longtable = TRUE,
      format.args = list(big.mark = ",", scientific = FALSE),
      label = "trust") %>%
    kableExtra::kable_styling(latex_options = c("scale_down", "hold_position", "repeat_header"),
                              font_size = base_font_size - 2, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::column_spec(2:10, width = "4em") %>%
    kableExtra::column_spec(4:10, width = "6em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S6 shows percentage of respondents that mention actors who they would trust the most to help them decide whether to get a COVID-19 vaccine. For all countries the questions was asked regardless if respondent would take a vaccine, would not take it, does not know or does not respond. For India respondents were able to mention more than one actor, for the rest of countries only one actor was allowed. While rows should sum to 100%, rounding makes number slightly above or below. A 95% confidence interval is shown between parentheses.",
      threeparttable = T) %>%
    kableExtra::landscape()



  trust_vacc %>%
    dplyr::select("group", "n",
                  "Take vaccine?", "trust_vaccine_5",
                  "trust_vaccine_6", "trust_recode_1",
                  "trust_recode_3", "trust_recode_2",
                  "trust_recode_4", "trust_recode_5") %>%
    knitr::kable(
      col.names = c("Study", "N", "Take vaccine?", "Health workers",
                    "Government or \n Ministry of Health",
                    "Family or friends",
                    "Famous person, \n religious leader or \n traditional healers",
                    "Newspapers, radio \n or online groups", "Other",
                    "Don't know or Refuse"),
      caption = "COVID-19 Vaccination Decision-making: most trusted source",
      format.args = list(big.mark = ",", scientific = FALSE),
      align = c("l", rep("c", 9))) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "7em") %>%
    kableExtra::column_spec(2:10, width = "4em") %>%
    kableExtra::column_spec(4:10, width = "6em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S6 shows percentage of respondents that mention actors who they would trust the most to help them decide whether to get a COVID-19 vaccine. For all countries the questions was asked regardless if respondent would take a vaccine, would not take it, does not know or does not respond. For India respondents were able to mention more than one actor, for the rest of countries only one actor was allowed. While rows should sum to 100%, rounding makes number slightly above or below. A 95% confidence interval is shown between parentheses.",
      threeparttable = T)

}
