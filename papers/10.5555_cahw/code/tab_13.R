# Appendix: Results by CAHW Performance Index's Subcomponents — Selection and Incentives in Local Service Provision: Theory and Evidence from Sierra Leone
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.5555_cahw
# Run from the paper's code/ folder: Rscript tab_13.R


library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(patchwork)
library(scales)

make_tab_13 <- function(data){
    
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
    
    table <- data |> html_table(
      header_list = NULL,
      FEs = rep('No', 4), 
      Sample = rep('All', 4),
      model_names = c( 'All', 'Num. of reports', 'Num. of reported animals', 'CAHW found in village')
    )
    
    #FEs = c("Yes", "Yes"), 
    # Sample = c("All", "All"), 
    #header_list = NULL)
    
    return(table)
    
  }


make_tab_13(readRDS("../data/table_subcomponents_std.rds"))
