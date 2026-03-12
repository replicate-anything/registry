generate_table <- function(data){

  # Analysis of differences in means only LMICs
  # Notice that Uganda 1 is dropped, because it does not have reference categories for gender or age
  # Notice that we are using df (and not df2) as data, since it does not include "All"

  # Population estimate (clustering on country)

  differences_means_gen_age <-
    lapply(c("gender", "age_groups_binary", "age_groups_three"), function(i) {
      data %>%
        dplyr::filter(country != "USA" & country != "Russia" & country != "Uganda 1") %>%
        dplyr::filter(if_all(c(all_of(i), all_of("take_vaccine_num")), ~ !is.na(.))) %>%
        study_weighting() %>%
        estimatr::lm_robust(as.formula(paste("take_vaccine_num ~", i)),
                            fixed_effects = ~country,
                            weight = weight,
                            cluster = country,
                            se_type = "stata",
                            data = .) %>%
        tidy %>%
        dplyr::select(estimate, std.error, p.value, data, term)
    }) %>%
    dplyr::bind_rows(.)

  differences_means_educ <-
    data %>%
    dplyr::filter(country != "USA" & country != "Russia") %>%
    dplyr::filter(if_all(c(all_of("educ_binary"), all_of("take_vaccine_num")), ~ !is.na(.))) %>%
    study_weighting() %>%
    estimatr::lm_robust(
      take_vaccine_num ~educ_binary,
      fixed_effects = ~country,
      weight = weight,
      cluster = country,
      se_type = "stata",
      data = .) %>%
    tidy %>%
    dplyr::select(estimate, std.error, p.value, data, term)


  dif_gender <-
    differences_means_gen_age %>%
    dplyr::filter(term == "genderMale") %>%
    pull(estimate) %>%
    {. * 100} %>%
    round(., 1)

  dif_age <-
    differences_means_gen_age %>%
    dplyr::filter(term == "age_groups_binary55+") %>%
    pull(estimate) %>%
    {. * 100} %>%
    round(., 1)

  dif_age_three <-
    differences_means_gen_age %>%
    dplyr::filter(term == "age_groups_three25-54" | term == "age_groups_three55+") %>%
    pull(estimate) %>%
    {. * 100} %>%
    round(., 1)

  dif_educ <-
    differences_means_educ %>%
    dplyr::filter(term == "educ_binaryUp to Secondary") %>%
    pull(estimate) %>%
    {. * 100} %>%
    round(., 1)

  diffmeans <-
    rbind(differences_means_gen_age, differences_means_educ) %>%
    dplyr::rename(Estimate = estimate,
                  Std.error = std.error,
                  `P-value` = p.value,
                  "Degrees of freedom" = data,
                  "Baseline category" = term) %>%
    dplyr::mutate(
      Variable = ifelse(`Baseline category` == "genderMale",
                        "Gender (Female)", ""),
      Variable = ifelse(`Baseline category` == "age_groups_binary55+",
                        "Age", Variable),
      Variable = ifelse(`Baseline category` == "educ_binaryUp to Secondary",
                        "Education (Secondary +)", Variable),
      `Baseline category` = ifelse(`Baseline category` == "genderMale",
                                   "Male", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_binary55+",
                                   "55+", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "educ_binaryUp to Secondary",
                                   "Up to secondary", `Baseline category`))


  diffmeans <-
    diffmeans %>%
    filter(Variable != "Age") %>%
    dplyr::mutate(
      Variable = ifelse(`Baseline category` == "age_groups_three25-54",
                        "Age (25-54)", Variable),
      Variable = ifelse(`Baseline category` == "age_groups_three55+",
                        "Age (55+)", Variable),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_three25-54",
                                   "<25", `Baseline category`),
      `Baseline category` = ifelse(`Baseline category` == "age_groups_three55+",
                                   "<25", `Baseline category`))

  dmeans <- diffmeans %>%
    filter(Variable != "") %>%
    knitr::kable(
      digits = 2,
      caption =  "Differences in means",
      format = "latex", booktabs = T, linesep = "", label = "dmeans", align = "c") %>%
    kableExtra::kable_styling(latex_options = c("hold_position"),
                              font_size = base_font_size - 2, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S7 shows the results of subgroup mean differences. Subgroup differences were generated considering only LMICs. p-values come from a two-sided t-test from a linear regression.",
      threeparttable = T)

  diffmeans %>%
    filter(Variable != "") %>%
    knitr::kable(
      caption =  "Differences in means",
      booktabs = T, linesep = "", label = "dmeans", digits = 2) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S7 shows the results of subgroup mean differences. Subgroup differences were generated considering only LMICs. The differences in means for gender and age do not include the Uganda 1 study, which only included female respondents under the age of 55.",
      threeparttable = T)

}
