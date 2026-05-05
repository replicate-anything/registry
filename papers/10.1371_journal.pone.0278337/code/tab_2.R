generate_table <- function(data){
  
  df_2 <- data$fig_3
  label <- data$labels
  
  conjoints <- 
    
    list(
      conjoint1_1 =
        df_2 %>%
        mutate(vignr = as.numeric(c_0031_w2),
               outcome = conjoint_choice1_exp3, 
               rating = conjoint_rating1_exp3, 
               contest = 1, candidate=1), 
      
      conjoint1_2 =
        df_2 %>%
        mutate(vignr = as.numeric(c_0032_w2),
               outcome = conjoint_choice1_exp3, 
               rating = conjoint_rating2_exp3, 
               contest = 1, candidate=2),
      
      conjoint2_1 =
        df_2 %>%
        mutate(vignr = as.numeric(vignette3),
               outcome = conjoint_choice2_exp3, 
               rating = conjoint_rating3_exp3, 
               contest = 2, candidate=1),
      
      conjoint2_2 =
        df_2 %>%
        mutate(vignr = as.numeric(vignette4),
               outcome = conjoint_choice2_exp3, 
               rating = conjoint_rating4_exp3, 
               contest = 2, candidate=2),
      
      conjoint3_1 =
        df_2 %>%
        mutate(vignr = as.numeric(vignette5),
               outcome = conjoint_choice3_exp3, 
               rating = conjoint_rating5_exp3, 
               contest = 3, candidate=1),
      
      conjoint3_2 =
        df_2 %>%
        mutate(vignr = as.numeric(vignette6),
               outcome = conjoint_choice3_exp3, 
               rating = conjoint_rating6_exp3, 
               contest = 3, candidate=2)  
      
    ) %>% 
    bind_rows() %>%
    select(ID, vignr, outcome, rating, contest, candidate, treatment_video) %>%
    mutate(
      outcome = ifelse(candidate ==1, 
                       as.numeric(outcome=="1"),
                       as.numeric(outcome=="2"))) %>%
    left_join(label) %>%
    mutate(
      doses = factor(
        vig_doses, 0:3,
        c("1 Million doses", "5 Million doses", "10 Million doses", "20 Million doses")),
      share = factor(vig_dose_share, 0:3,
                     c("1 % of the vaccines", "5 % of the vaccines", "10 % of the vaccines", "20 % of the vaccines")),
      number= factor(vig_countries, 0:2, c("20 countries", "80 countries", "160 countries")), 
      economic_benefits= factor(vig_benefit_economic, 0:1, c("Without economic importance", "With economic importance")),
      health_benefits=factor(vig_benefit_health, 0:1,  
                             c("No risk of infection", "Risk of infection"))) 
  
  # Demean data
  # Note that mean rating is removed
  conjoints_norm <- conjoints %>% 
    mutate(doses_norm = vig_doses - mean(vig_doses),
           number_norm = vig_countries - mean(vig_countries),
           share_norm = vig_dose_share - mean(vig_dose_share),
           economic_benefits = vig_benefit_economic - mean(vig_benefit_economic),
           health_benefits = vig_benefit_health - mean(vig_benefit_health)) %>%
    group_by(ID) %>% 
    mutate(rating = rating - mean(rating)) %>% ungroup 
  
  
  models_treatments <-
    list(
      rating = lm_robust(rating ~  treatment_video*vig_doses + treatment_video*vig_dose_share + treatment_video*vig_countries + treatment_video*vig_benefit_economic + treatment_video*vig_benefit_health, 
                         data = conjoints_norm),
      choice = lm_robust(outcome ~  treatment_video*vig_doses + treatment_video*vig_dose_share + treatment_video*vig_countries + treatment_video*vig_benefit_economic + treatment_video*vig_benefit_health, 
                         data = conjoints_norm)
    )
  
  coef_map <- c(
    "(Intercept)" = "Constant (Average rating)",
    "treatment_video" = "Video effect (given ungenerous agreement)",
    "vig_doses" = "German contribution",
    "vig_dose_share" = "German share",
    "vig_countries" = "Number of donors",
    "vig_benefit_economic" = "Economic benefits",
    "vig_benefit_health" = "Health benefits",
    "treatment_video:vig_doses" = "Video * Contribution",
    "treatment_video:vig_dose_share" = "Video * Share",
    "treatment_video:vig_countries" = "Video * Donors",
    "treatment_video:vig_benefit_economic" = "Video * Economics",
    "treatment_video:vig_benefit_health" = "Video * Health"
  )
  
  tbl <- modelsummary(
    models_treatments,
    coef_map = coef_map,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = c('***' = .001, '**' = .01, '*' = .05),
    gof_map = c("r.squared", "adj.r.squared", "nobs", "rmse"),
    output = "kableExtra",
    title = "Effects of treatment on drivers of support for agreements.",
    notes = "*** p<0.001; ** p<0.01; * p<0.05",
    colnames = c("rating", "choice")
  ) %>%
    kable_styling(latex_options = c("hold_position"))
  
  return(tbl)
}


