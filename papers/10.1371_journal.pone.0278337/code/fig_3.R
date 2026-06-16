# Effect of video treatment on individual solidarity — Public support for global vaccine sharing in the COVID-19 pandemic: Evidence from Germany
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1371_journal.pone.0278337
# Run from the paper's code/ folder: Rscript fig_3.R

library(dplyr)
library(ggplot2)
library(estimatr)
library(bbmle)
library(egg)

make_fig_3 <- function(data){

  outcomes <- c("solidarity_behaviour", "solidarity_attitude")
  outcome_labels <- c("Solidarity Behavior", "Solidarity Attitude")
  w2_treatments <- c("treatment_video")
  w2_treatment_labels <- c("Treatment")


  models_basic <- lapply(c(outcomes), function(y)
    estimatr::lm_robust(as.formula(paste(y, "~ treatment_video")),
              data = data))

  names(models_basic) <- outcomes

  figure_3 <-
    lapply(models_basic, broom::tidy) |> dplyr::bind_rows(.id = "outcome") |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::mutate(outcome = factor(outcome, outcomes, outcome_labels)) |>
    ggplot2::ggplot(aes(estimate, outcome)) + ggplot2::geom_point()+
    ggplot2::geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = .1)+
    ggplot2::geom_vline(xintercept=0, linetype="longdash", lwd=0.35, colour = "#B55555") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Effect of video treatment") +
    ggplot2::ylab("")

  return(figure_3)
}


make_fig_3(utils::read.csv("../data/fig_3.csv", stringsAsFactors = FALSE))
