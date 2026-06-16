# Reason not to take the vaccine — COVID-19 vaccine acceptance and hesitancy in low- and middle-income countries
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1038_s41591-021-01454-y
# Run from the paper's code/ folder: Rscript tab_9.R
# Requires the data/ folder alongside code/ (see replication.yml).

make_tab_9 <- function(data){
  
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
      "trust_vaccine_nr", "trust_vaccine_666", "t