generate_figure <- function(data){
  
  library(ggplot2)
  
  bound_names <- c(
    "Simple bounds",
    "Unobserved mediators",
    "Monotonic",
    "Two step homogeneous",
    "Infinite step homogeneous",
    "Best (positive) mediator",
    "Best (unobserved) covariate",
    "Best (observed) covariate"
  )
  
  rho_values <- round(c(-.5, 0, .5), 2)
  
  dat <- data
  
  dat$names <- factor(dat$names, levels = rev(bound_names))
  
  dat$rho <- paste("rho ==", dat$rho)
  dat$tau <- paste("tau ==", dat$tau)
  
  dat$rho <- factor(dat$rho, levels = paste0("rho == ", rho_values))
  
  p <- ggplot(data = dat, aes(x = names, ymin = L, ymax = H, color = names)) +
    geom_point(aes(y = L), position = position_dodge(.8)) +
    geom_point(aes(y = H), position = position_dodge(.8)) +
    geom_linerange(position = position_dodge(.8)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_flip(ylim = c(0,1)) +
    theme_bw() +
    guides(col = guide_legend(ncol = 2)) +
    labs(
      y = "Probability of causation",
      x = "",
      shape = "\u03C1",
      colour = "\u03C1"
    ) +
    theme(legend.position = "bottom") +
    facet_grid(rho ~ tau, labeller = label_parsed) +
    theme(legend.position = "none") +
    scale_color_grey(start = .6, end = 0)
  
  return(p)
}