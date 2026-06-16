# Results Refreshment Sample — Public support for global vaccine sharing in the COVID-19 pandemic: Evidence from Germany
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1371_journal.pone.0278337
# Run from the paper's code/ folder: Rscript fig_8.R

library(dplyr)
library(ggplot2)
library(estimatr)
library(bbmle)
library(egg)

make_fig_8 <- function(data){

  treatments_norm <- c("trading_importance", "risk", "others_number_norm", "others_giving_norm")
  treatment_labels <- c("Trading importance", "Risk", "Number of others giving (10s)", "Amount given by other countries\n(10s of billions)")

  main_results_by_group <-
    lapply(unique(data$group), function(p) {
      dplyr::bind_rows(
        cash = broom::tidy(
          estimatr::lm_robust(
            cash_billions ~ trading_importance * risk * others_number_norm * others_giving_norm + round,
            fixed_effects = ~id,
            se_type = "stata",
            data = dplyr::filter(data, group == p)
          )
        ),
        doses = broom::tidy(
          estimatr::lm_robust(
            doses ~ trading_importance * risk * others_number_norm * others_giving_norm + round,
            fixed_effects = ~id,
            se_type = "stata",
            data = dplyr::filter(data, group == p)
          )
        ),
        .id = "outcome"
      ) |>
        dplyr::mutate(group = factor(p))
    }) |>
    dplyr::bind_rows()

  figure_1_group <-

    main_results_by_group |>
    dplyr::filter(term %in% treatments_norm) |>
    dplyr::mutate(Treatment = factor(term, treatments_norm, treatment_labels),
           outcome = factor(outcome, c("cash_billions", "doses"), c("Cash (billion Euros)", "Doses (Millions)"))) |>
    ggplot2::ggplot(ggplot2::aes(Treatment, estimate, group=group, color = group)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.3))+
    ggplot2::scale_color_discrete(name = "Sample", labels = c("refreshment", "main"))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), position = ggplot2::position_dodge(0.3), width = .1)+
    ggplot2::geom_hline(yintercept = 0, linetype = "longdash", lwd = 0.35, colour = "#B55555") +
    ggplot2::theme_bw() + ggplot2::facet_grid(~outcome, scales = "free_x")+
    ggplot2::xlab("") +
    ggplot2::coord_flip()

  return(figure_1_group)
}


make_fig_8(utils::read.csv("../data/fig_1.csv", stringsAsFactors = FALSE))
