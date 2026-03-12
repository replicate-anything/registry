generate_figure <- function(data){
  
  safe_colorblind_palette <- c(
    "#CC6677","#DDCC77","#117733","#332288",
    "#AA4499","#44AA99","#999933","#882255",
    "#661100","#6699CC","#888888","#88CCEE"
  )
  
  data$estimate <- data$estimate * 100
  
  n_colors <- length(unique(data$tag))
  palette <- grDevices::colorRampPalette(safe_colorblind_palette)(n_colors)
  
  fig_hist_loo <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = estimate, color = tag, fill = tag)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 200,
      alpha = .3,
      linewidth = .3,
      position = "dodge"
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_x_continuous(n.breaks = 8) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1, suffix = "", accuracy = 0.1)
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  return(fig_hist_loo)
  
}