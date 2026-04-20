generate_figure <- function(data){

  outcomes <- c("solidarity_behaviour", "solidarity_attitude")
  outcome_labels <- c("Solidarity Behavior", "Solidarity Attitude")
  w2_treatments <- c("treatment_video")
  w2_treatment_labels <- c("Treatment")


  models_basic <- lapply(c(outcomes), function(y)
    lm_robust(as.formula(paste(y, "~ treatment_video")),
              data = data))

  names(models_basic) <- outcomes

  figure_3 <-
    lapply(models_basic, tidy) %>% bind_rows(.id = "outcome") %>%
    dplyr::filter(term != "(Intercept)") %>%
    mutate(outcome = factor(outcome, outcomes, outcome_labels)) %>%
    ggplot(aes(estimate, outcome)) + geom_point()+
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = .1)+
    geom_vline(xintercept=0, linetype="longdash", lwd=0.35, colour = "#B55555") +
    theme_bw() +
    ggtitle("Effect of video treatment") +
    ylab("")

  return(figure_3)
}
