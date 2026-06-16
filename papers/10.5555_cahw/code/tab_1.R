generate_table <- function(data){
  
  # === Standard coefficient map ===
  standard_coef_map <- 
    list(
      "t1_COMC" = "Village Selection",
      "t2_CM" = "Community Monitoring", 
      "t3_P4P" = "Pay for Performance",
      "t1_COMC:t2_CM" = "Village Selection $\\times$ Community Monitoring",
      "t1_COMC:t3_P4P" = "Village Selection $\\times$ Pay for Performance",
      "t2_CM:t3_P4P" = "Community Monitoring $\\times$ Pay for Performance",
      "t1_COMC:t2_CM:t3_P4P" = "Village Selection $\\times$ Community Monitoring $\\times$ Pay for Performance"
    )
  
  header_list_1 <- 
    list(
      '\\shortstack{Village\\\\selection}' = 1:2,
      '\\shortstack{Community\\\\monitoring}' = 3,
      '\\shortstack{Pay for\\\\performance}' = 4,
      'Full Model' = 5:6
    )
  
  # === Main table generation ===
  html_table <- function(table_objects,
                         header_list = header_list_1, 
                         coef_map = standard_coef_map,
                         FEs = c('$B_s$', '$B_s$', '$B_{mp}$', '$B_{mp}$', 'No', '$B_{mp}$'),
                         Sample = c('Subset', rep('All', 5)),
                         model_names = paste0('(', 1:length(table_objects$models), ')')) {
    
    
    
    # Generate table
    table_objects$models |>
      texreg::htmlreg(
        custom.coef.map = coef_map,
        custom.header = header_list,
        custom.model.names = model_names,
        include.rsquared = TRUE, 
        include.adjrs = FALSE,
        include.rmse = FALSE,
        #      custom.gof.names = c("$R^2$", "Observations"),
        custom.gof.rows = list(
          "Control Mean" = table_objects$control_stats[[1]],
          "Control SD" = table_objects$control_stats[[2]],
          "$T_m=T_p$ (p-value)" = table_objects$equality_tests,
          "Fixed Effects" = FEs,
          "Sample" = Sample
        ),
        table = FALSE,
        include.ci = FALSE,
        fontsize = 'small',
        use.packages = FALSE,
        booktabs = TRUE,
        dcolumn = TRUE,
        stars = c(0.01, 0.05, 0.1)
      )
  }
  
  fit_models_community_level <- function(data, outcome_var) {
    list(
      T1_strata1 = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t1_COMC")), 
                             data = data, subset = primary_stratum == TRUE, 
                             fixed_effects = ~t1_block),
      
      # weighted
      T1 = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t1_COMC")), fixed_effects = ~t1_block, data = data),
      
      T2 = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t2_CM")), fixed_effects = ~t2t3_block,  
                     data = data),
      
      T3 = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t3_P4P")), fixed_effects = ~t2t3_block, 
                     data = data),
      
      # weighted
      All = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t1_COMC * t2_CM * t3_P4P + primary_stratum")), 
                      data = data |> dplyr::mutate(t1_COMC = t1_COMC - mean(t1_COMC), 
                                            t2_CM = t2_CM - mean(t2_CM), 
                                            t3_P4P = t3_P4P - mean(t3_P4P))),
      
      `All (FE)` = estimatr::lm_robust(as.formula(paste(outcome_var, "~ t1_COMC * t2_CM * t3_P4P")), 
                             fixed_effects = ~t2t3_block, 
                             data = data |> dplyr::mutate(t1_COMC = t1_COMC - mean(t1_COMC), 
                                                   t2_CM = t2_CM - mean(t2_CM), 
                                                   t3_P4P = t3_P4P - mean(t3_P4P)))
    )
  }
  
  
  # === Control statistics ===
  get_control_stats <- function(data, outcome_var, prefix = "") {
    
    data$y <- data[[outcome_var]]
    
    list(
      "Control Mean" = with(data,
                            c(mean(y[primary_stratum == "TRUE" & t1_COMC==0], na.rm = TRUE),
                              mean(y[t1_COMC==0], na.rm = TRUE),
                              mean(y[t2_CM==0], na.rm = TRUE),
                              mean(y[t3_P4P==0], na.rm = TRUE),
                              mean(y[t1_COMC==0  & t2_CM==0 & t3_P4P==0], na.rm = TRUE),
                              mean(y[t1_COMC==0  & t2_CM==0 & t3_P4P==0], na.rm = TRUE))),
      
      "Control SD" = with(data,
                          c(sd(y[primary_stratum  == "TRUE"  & t1_COMC==0], na.rm = TRUE),
                            sd(y[t1_COMC==0], na.rm = TRUE),
                            sd(y[t2_CM==0], na.rm = TRUE),
                            sd(y[t3_P4P==0], na.rm = TRUE),
                            sd(y[t1_COMC==0  & t2_CM==0 & t3_P4P==0], na.rm = TRUE),
                            sd(y[t1_COMC==0  & t2_CM==0 & t3_P4P==0], na.rm = TRUE)))
    )
    
  }
  
  
  # === Equality tests ===
  test_treatment_equality <- function(models) {
    c(NA, NA, NA, NA, 
      car::linearHypothesis(models$All, "t2_CM = t3_P4P")$`Pr(>Chisq)`[2],
      car::linearHypothesis(models$`All (FE)`, "t2_CM = t3_P4P", singular.ok = TRUE)$`Pr(>Chisq)`[2])
  }
  
  # === combine table objects ===
  get_table_objects <- function(df, outcome_var){
    models = fit_models_community_level(df, outcome_var)
    control_stats = get_control_stats(df, outcome_var)
    equality_tests = test_treatment_equality(models)
    return(list(
      models = models, 
      control_stats = control_stats, 
      equality_tests = equality_tests
    ))
  }
  
  
  table <- get_table_objects(data, "y_qual_2") |>
    html_table()
  
  return(table)
  
}