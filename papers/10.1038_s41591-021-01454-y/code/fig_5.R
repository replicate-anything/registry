generate_figure <- function(data){

  #Group together categories

  data %<>%
    dplyr::mutate(
      trust_recode_1 = ifelse(trust_vaccine_1 == 1 | trust_vaccine_2 == 1, 1, 0),
      trust_recode_1 = ifelse((country == "Nigeria" | country == "USA") &
                                is.na(trust_recode_1), 0, trust_recode_1),
      trust_recode_2 = ifelse(trust_vaccine_8 == 1 | trust_vaccine_9 == 1, 1, 0),
      trust_recode_2 = ifelse((country == "Sierra Leone 2") &
                                is.na(trust_recode_2), 0, trust_recode_2),
      trust_recode_3 = ifelse(trust_vaccine_3 == 1 |
                                trust_vaccine_7 == 1 |
                                trust_vaccine_4 == 1, 1, 0),
      trust_recode_3 =
        ifelse((country == "Nigeria" | country == "USA" | country == "Russia") &
                 is.na(trust_recode_3), 0, trust_recode_3),
      trust_recode_4 = ifelse(trust_vaccine_666 == 1 | trust_vaccine_other == 1, 1, 0),
      trust_recode_4 =
        ifelse((country == "Burkina Faso" |
                  country == "Sierra Leone 2" |
                  country == "Russia") &
                 is.na(trust_recode_4), 0, trust_recode_4),
      trust_recode_5 = ifelse(trust_vaccine_dk == 1 |
                                trust_vaccine_refuse == 1 |
                                trust_vaccine_nr == 1, 1, 0),
      trust_recode_5 =
        ifelse((country == "Nigeria" | country == "Sierra Leone 2" | country == "USA") &
                 is.na(trust_recode_5), 0, trust_recode_5))

  #Recoded groups
  trust_names <- c("trust_recode_1", "trust_recode_2", "trust_recode_3",
                   "trust_recode_4", "trust_recode_5", "trust_vaccine_5", "trust_vaccine_6")

  studies_levels <-
    c("Burkina Faso", "Colombia", "India", "Mozambique",
      "Nepal", "Nigeria", "Pakistan 1", "Rwanda",
      "Sierra Leone 1", "Sierra Leone 2", "Uganda 2",
      "All", "Russia", "USA" )

  #Get estimates
  trust_vacc_together <-
    list(
      All = lapply(trust_names, reasons_together,
                   df = data, num = c("Yes", "No", "DK")) %>%
        dplyr::bind_rows(),
      Yes = lapply(trust_names, reasons_together,
                   df = data, num = c("Yes")) %>%
        dplyr::bind_rows(),
      No = lapply(trust_names, reasons_together,
                  df = data, num = c("No", "DK")) %>%
        dplyr::bind_rows()) %>%
    dplyr::bind_rows(.id = "sub") %>%
    dplyr::filter(!is.nan(statistic)) %>%
    dplyr::mutate(
      across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)),
      n_sub = round(n * estimate, 0),
      n_sub = ifelse(n_sub == 0, NA_integer_, n_sub),
      group = factor(group, levels = studies_levels)) %>%
    dplyr::left_join(dictionary, by = "outcome") %>%
    dplyr::mutate(
      size = cut(n_sub, c(0, 50, 500, Inf), include.lowest = TRUE),
      size = forcats::fct_recode(size, "500+" = "(500,Inf]"),
      tag = as.factor(tag),
      tag = forcats::fct_relevel(tag,
                                 "Health workers",
                                 "Government or MoH",
                                 "Family or Friends",
                                 "Famous person, religious leader or traditional healers",
                                 "Newspapers, radio or online groups",
                                 "Other",
                                 "Don't know or Refuse"),
      sub = forcats::fct_relevel(as.factor(sub), "No", "Yes", "All"),
      sub = plyr::mapvalues(sub, from = c("No", "Yes", "All"),
                            to = c("No, Don't know", "Yes", "Any")))

  trust <- filter(trust_vacc_together, sub == "Any")

  # All LMIcs estimate of trust HW

  trust_all <-
    trust %>%
    dplyr::filter(group == "All" & tag == "Health workers") %>%
    dplyr::arrange(desc(estimate))

  # Top and bottom LMICs countries for trust in Health works
  trust_ <-
    trust %>%
    dplyr::filter(group != "All" & group != "USA" &
                    group != "Russia" & tag == "Health workers")

  trust_top <- trust_ %>% dplyr::arrange(desc(estimate))

  trust_rwa <-
    trust %>%
    dplyr::filter(group == "Rwanda") %>%
    dplyr::arrange(desc(estimate))

  trust_npl <-
    trust %>%
    dplyr::filter(
      group == "Nepal" &
        tag == "Famous person, religious leader or traditional healers") %>%
    dplyr::arrange(desc(estimate))

  # Top and bottom LMICs countries for trust in Family and friends
  trust_fam <-
    trust %>%
    dplyr::filter(group != "All" & tag == "Family or Friends") %>%
    dplyr::arrange(desc(estimate))

  # Top and bottom LMICs countries for trust in Gov
  trust_gov <-
    trust %>%
    dplyr::filter(group != "All" & group != "Rwanda" & tag == "Government or MoH") %>%
    dplyr::arrange(desc(estimate))

  #Plot
  fig_hist_categories <-
    trust_vacc_together %>%
    dplyr::mutate(group = plyr::mapvalues(group, "All", "All LMICs")) %>%
    ggplot(aes(estimate, tag, fill = sub)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~group, ncol = 2, strip.position = "left")  +
    coord_flip() +
    scale_fill_manual(
      name = "Answer",
      values = safe_colorblind_palette[c(1,3,2)]) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 16),
                     guide = guide_axis(angle = 90)) +
    labs(title = "Which of the following people would you trust MOST to help you decide whether you would get a COVID-19 vaccine?",
         y = "") +
    theme_bw() +
    theme(legend.position = "bottom",
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          axis.text.y = element_text(hjust = 0))

  return(fig_hist_categories)

}
