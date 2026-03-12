generate_table <- function(data){

  country_differences <-
    unique(df$country) %>%
    lapply(function(j){{
      dff <- filter(df, country == j)

      lapply(c("gender", "age_groups_three", "educ_binary"), function(i){
        if (length(table(dff[[i]])) < 2)  {
          return(NULL)
        } else {
          m <- estimatr::lm_robust(as.formula(paste("take_vaccine_num ~", i)),
                                   weight = weight,
                                   cluster = cluster,
                                   se_type = "stata",
                                   data = dff)
          m %>%
            tidy %>%
            dplyr::select(estimate, std.error, p.value, df, term) %>%
            dplyr::mutate(n = m$nobs)
        }}
      ) } %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(country = j)}) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(term, country) %>%
    dplyr::relocate(country, term) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(significant = p.value <= .05) %>%
    dplyr::mutate(
      term = ifelse(term == "age_groups_three25-54", "25-54", term),
      term = ifelse(term == "age_groups_three55+", "55+", term),
      term = ifelse(term == "educ_binaryUp to Secondary", "Up to secondary", term),
      term = ifelse(term == "genderMale", "Male", term))

  country_differences_summary <-
    country_differences %>%
    dplyr::filter(!(country %in% c("Russia", "USA"))) %>%
    dplyr::group_by(term) %>% summarize(
      "positive " = sum(estimate > 0),
      "positive and significant" = sum(estimate > 0 & significant),
      "negative and significant" = sum(estimate < 0 & significant),
      "not significant" = sum(!significant),
      n = n())

  t_country_differences <- country_differences %>%
    dplyr::mutate(
      baseline = ifelse(term == "Male", "Female", NA),
      baseline = ifelse(term == "Up to secondary", "Secondary +", baseline),
      baseline = ifelse(is.na(baseline), "<25", baseline),
      group = ifelse(baseline == "<25", "Age", NA),
      group = ifelse(baseline == "Secondary +", "Education", group),
      group = ifelse(baseline == "Female", "Gender", group),
      estimate = round(estimate, 2),
      std.error = round(std.error, 2),
      p.value = round(p.value, 2)) %>%
    dplyr::select(country, group, baseline, term, everything(), -significant)

  t_country <- t_country_differences %>%
    kable(digits = 2,
          col.names = c("Country", "Variable", "Baseline category", "Group", "Estimate", "Std. Error", "P-value", "Degrees of freedom", "N Obs"),
          caption = "Differences between groups within studies",
          booktabs = TRUE, linesep = "",
          format.args = list(big.mark = ",", scientific = FALSE),
          format = "latex", label = "countrydiff") %>%
    kableExtra::kable_styling(latex_options = c("scale_down"), font_size = base_font_size - 2, full_width = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1, width = "6em") %>%
    kableExtra::column_spec(2:3, width = "6em") %>%
    kableExtra::column_spec(4, width = "9em")  %>%
    kableExtra::column_spec(5:8, width = "6em") %>%
    kableExtra::footnote(
      general_title = "",
      general = "Table S9 shows differences of means between groups within single studies. Estimates are calculated through OLS and represent the difference in the average acceptance rate between the subgroup in column Group and that in column Baseline category. p-values come from a two-sided t-test from a linear regression.",
      threeparttable = T)


  knitr::kable(country_differences_summary,
               digits = 2, caption = "Differences between groups within studies (Summary)")

}
