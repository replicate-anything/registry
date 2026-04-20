generate_figure <- function(data){

  treatments_norm <- c("trading_importance", "risk", "others_number_norm", "others_giving_norm")
  treatment_labels <- c("Trading importance", "Risk", "Number of others giving (10s)", "Amount given by other countries\n(10s of billions)")

  main_results_by_group <-
    lapply(unique(data$group), function(p)
      list(
        cash = lm_robust(cash_billions ~ trading_importance*risk*others_number_norm*others_giving_norm  + round,
                         fixed_effects = ~id,  se_type = "stata",
                         data = data %>% dplyr::filter(group == p)) %>% tidy,
        doses = lm_robust(doses ~ trading_importance*risk*others_number_norm*others_giving_norm  + round,
                          fixed_effects = ~id,  se_type = "stata",
                          data = data %>% dplyr::filter(group == p)) %>% tidy) %>%
        bind_rows() %>% mutate(group = factor(p))) %>%
    bind_rows()

  figure_1_group <-

    main_results_by_group %>%
    dplyr::filter(term %in% treatments_norm) %>%
    mutate(Treatment = factor(term, treatments_norm, treatment_labels),
           outcome = factor(outcome, c("cash_billions", "doses"), c("Cash (billion Euros)", "Doses (Millions)"))) %>%
    ggplot(aes(Treatment, estimate, group=group, color = group)) +
    geom_point(position = position_dodge(0.3))+
    scale_color_discrete(name = "Sample", labels = c("refreshment", "main"))+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(0.3), width = .1)+
    geom_hline(yintercept=0, linetype="longdash", lwd=0.35, colour = "#B55555") +
    theme_bw() + facet_grid(~outcome, scales = "free_x")+
    xlab("") +
    coord_flip()

  return(figure_1_group)
}
