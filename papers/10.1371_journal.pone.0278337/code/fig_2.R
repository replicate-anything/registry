generate_figure <- function(data){

  main_results <-
    list(
      cash = lm_robust(cash_billions ~ trading_importance*risk*others_number_norm*others_giving_norm  + round, fixed_effects = ~id,  se_type = "stata", data = data) %>% tidy,
      doses = lm_robust(doses ~ trading_importance*risk*others_number_norm*others_giving_norm  + round, fixed_effects = ~id,  se_type = "stata", data = data) %>% tidy) %>% bind_rows()


  treatments <- c("trading_importance", "risk", "others_number", "others_giving")
  treatments_norm <- c("trading_importance", "risk", "others_number_norm", "others_giving_norm")
  treatment_labels <- c("Trading importance", "Risk", "Number of others giving (10s)", "Amount given by other countries\n(10s of billions)")


  figure_2 <-

    main_results %>%
    dplyr::filter(term %in% treatments_norm) %>%
    mutate(Treatment = factor(term, treatments_norm, treatment_labels),
           outcome = factor(outcome, c("cash_billions", "doses"), c("Cash (billion Euros)", "Doses (Millions)"))) %>%

    ggplot(aes(estimate, Treatment)) + geom_point()+
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = .1)+
    geom_vline(xintercept=0, linetype="longdash", lwd=0.35, colour = "#B55555") +
    theme_bw() + facet_grid(~outcome, scales = "free_x")+
    ylab("")

  return(figure_2)

}
