generate_figure <- function(data){

  fig_2 <-
    data %>%
    dplyr::filter(!is.na(n_sub)) %>%
    dplyr::mutate(name = plyr::mapvalues(name, "All", "All LMICs")) %>%
    ggplot(aes(name, estimate, color = tag)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  size = .5, width = .2, position = position_dodge(0.6)) +
    geom_point(shape = 16, position = position_dodge(0.6)) +
    facet_grid(.~tag,  space = "free", labeller = label_wrap_gen(width = 15)) +
    scale_size_discrete(range = c(1,3), name = "Number of observations" ) +
    geom_vline(xintercept = 3.5, color = "darkgrey") +
    geom_vline(xintercept = 2.5, color = "darkgrey") +
    guides(color = FALSE) +
    scale_colour_manual(values = safe_colorblind_palette) +
    coord_flip() + theme_bw() +
    labs(title = "Why would you not take the COVID-19 vaccine?",
         x = "") +
    theme_bw() + ylim(c(-2,100)) +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0), #Default is hjust=1
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          axis.text.y = element_text(hjust = 0))


  return(fig_2)

}
