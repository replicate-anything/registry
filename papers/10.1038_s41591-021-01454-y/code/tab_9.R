generate_table <- function(data){
  
  # define helper functions
  study_weighting <- function(data){ 
    data = data |> 
      dplyr::group_by(country) |> 
      dplyr::mutate(weight = weight/sum(weight)) |> 
      dplyr::ungroup() 
    
    return(data)
  }
  
  lm_helper <- function(data, ...) {
    data <- study_weighting(data)
    fit  <- estimatr::lm_robust(data = data, ...)
    out  <- dplyr::bind_cols(broom::tidy(fit), n = nobs(fit))
    return(out)
  }
  
  reasons_together <- function(df, 
                               reason, 
                               num = "Yes") {
    df %>%
      dplyr::filter(take_vaccine %in% num, 
                    if_all(c(all_of(reason), cluster, weight), ~ !is.na(.))) %>%
      dplyr::nest_by(group) %>%
      dplyr::summarize(
        lm_helper(data = data, 
                  formula = as.formula(paste0(reason, "~ 1")), 
                  cluster = cluster,
                  weight = weight, se_type = "stata"), .groups = "drop")
  }
  
  
  
  # Ensure cluster ids are distinct across studies
  df <- 
    data %>% 
    dplyr::group_by(study) %>% 
    dplyr::mutate(
      cluster = ifelse(is.na(cluster), paste(1:n()), cluster),
      cluster = paste0(gsub(" ", "_", tolower(country)), "_", cluster))
  
  
  # Weights sum to 1 in each study and recode age and education into bins
  df <- 
    df %>% 
    dplyr::group_by(study) %>% 
    dplyr::mutate(
      weight_replace = mean(weight, rm.na = TRUE),
      weight = if_else(is.na(weight), 
                       if_else(is.na(weight_replace), 1, weight_replace), 
                       weight),
      weight = weight/sum(weight)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      age_groups = 
        as.character(cut(x = age, breaks = c(-Inf, 18, 30, 45, 60, +Inf), right = F)),
      age_groups_binary = ifelse(age >= 55, "55+", NA),
      age_groups_binary = ifelse(age < 55, "<55", age_groups_binary),
      age_less24 = ifelse(age <= 24, 1, 0),
      age_25_54 = ifelse(age >= 25 & age <= 54, 1, 0),
      age_55_more = ifelse(age >= 55, 1, 0),
      age_groups_three = ifelse(age <= 24, "<25", NA),
      age_groups_three = ifelse(age >= 25 & age <= 54, "25-54", age_groups_three),
      age_groups_three = ifelse(age >= 55, "55+", age_groups_three),
      educ_binary = if_else(educ == "More than secondary", "> Secondary", "Up to Secondary")) 
  
  
  # We create a new dataframe with countries and with "All" (only LMICs). Countries are clusters in "All" analysis
  # USA and Russia excluded from "All" set
  
  df2 <- 
    dplyr::bind_rows(
      mutate(df, group = country),
      mutate(filter(df, country != "USA" & country != "Russia"), group = "All")) %>% 
    mutate(
      cluster = if_else(group == "All", 
                        gsub(pattern = " ", replacement = "_", x = tolower(country)), 
                        cluster)) 
  
  no_vars <- 
    df2 %>% 
    dplyr::select(starts_with("no_vaccine_")) %>% 
    names
  
  dictionary <- data.frame(
    outcome = c(
      "study", "country", "take_vaccine", "take_vaccine_num",
      "age", "age_groups", "age_groups_binary", "educ", "educ_binary",
      "gender", "cluster", "weight",
      "yes_vaccine_1", "yes_vaccine_2", "yes_vaccine_3", "yes_vaccine_4",
      "yes_vaccine_5", "yes_vaccine_666",
      "no_vaccine_1", "no_vaccine_2", "no_vaccine_3", "no_vaccine_4",
      "no_vaccine_5", "no_vaccine_6", "no_vaccine_7", "no_vaccine_8",
      "no_vaccine_9", "no_vaccine_666",
      "trust_vaccine_1", "trust_vaccine_2", "trust_vaccine_3", "trust_vaccine_4",
      "trust_vaccine_5", "trust_vaccine_6", "trust_vaccine_7", "trust_vaccine_8",
      "trust_vaccine_9", "trust_vaccine_dk", "trust_vaccine_refuse",
      "trust_vaccine_nr", "trust_vaccine_666", "trust_vaccine_other",
      "trust_recode_1", "trust_recode_2", "trust_recode_3",
      "trust_recode_4", "trust_recode_5"
    ),
    tag = c(
      "Study code", "Study name", "Respondent would take the vaccine if available?",
      "Respondent would take the vaccine if available? Yes = 1",
      "Age", "Age grouped", "Age recoded", "Education", "Education recoded",
      "Male", "Survey clusters", "Survey weights",
      "Protection: self", "Protection: family", "Protection: community",
      "If recommended by: Health workers", "If recommended by: Government", "Other",
      "Concerned about side effects", "Concerned about getting coronavirus from the vaccine",
      "Not concerned about getting seriously ill", "Doesn't think vaccines are effective",
      "Doesn't think Coronavirus outbreak is as serious as people say",
      "Doesn't like needles", "Allergic to vaccines", "Won't have time to get vaccinated",
      "Mentions a conspiracy theory", "Other reasons",
      "Family", "Friends", "Religious leader", "Famous person",
      "Health workers", "Government or MoH", "Traditional healers",
      "Media", "Online medical groups", "Don't know", "Refuse",
      "No response", "Other (specify)", "Other (category)",
      "Family or Friends", "Newspapers, radio or online groups",
      "Famous person, religious leader or traditional healers",
      "Other", "Don't know or Refuse"
    ),
    stringsAsFactors = FALSE
  )
  
  no_vacc <- 
    lapply(no_vars, reasons_together, df = df2, num = c("No", "DK")) %>%
    dplyr::bind_rows() %>% 
    dplyr::arrange(outcome) %>% 
    dplyr::mutate(
      across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)),
      n_sub = round(n * estimate, 0),
      n_sub = ifelse(n_sub == 0, NA_integer_, n_sub),
      group = 
        factor(group, 
               levels = rev(c("Burkina Faso", "Colombia", "Mozambique", 
                              "Nepal", "Nigeria", "Pakistan 1", "Rwanda", 
                              "Sierra Leone 1", "Sierra Leone 2", "Uganda 1", 
                              "Uganda 2", "All", "Russia", "USA" )))
    ) %>%
    dplyr::left_join(dictionary, by = "outcome") %>%
    dplyr::mutate(
      name = ifelse(group != "All", paste0(group, " (n=", n, ")"), "All"),
      name = gsub(pattern = " \\(", "\\\n\\(", name),
      tag = as.factor(tag),
      tag = forcats::fct_relevel(
        tag,  
        "Concerned about side effects", 
        "Concerned about getting coronavirus from the vaccine", 
        "Not concerned about getting seriously ill", 
        "Doesn't think vaccines are effective", 
        "Doesn't think Coronavirus outbreak is as serious as people say", 
        "Doesn't like needles", 
        "Allergic to vaccines", 
        "Won't have time to get vaccinated", 
        "Mentions a conspiracy theory", 
        "Other reasons"))
  
  special_cases <- 
    sort(unique(no_vacc$name)[grep(unique(no_vacc$name), pattern = "All|Russia|USA")])
  
  
  no_vacc <- no_vacc |> 
    dplyr::mutate(
      name = 
        factor(x = name, ordered = TRUE,
               levels = rev(c(sort(unique(name)[!(unique(name) %in% special_cases)]), special_cases))))
  
  no_vacc2 <-
    no_vacc %>%
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



  tab <-
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
  
  return(shiny::HTML(as.character(tab)))

}
