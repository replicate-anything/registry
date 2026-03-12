generate_figure <- function(data){
  
  safe_colorblind_palette <- c(
    "#CC6677","#DDCC77","#117733","#332288",
    "#AA4499","#44AA99","#999933","#882255",
    "#661100","#6699CC","#888888","#88CCEE"
  )
  
  loo_data <- data[data$source == "loo", ]
  ref_data <- data[data$source == "reference", ]
  
  loo_data$estimate <- loo_data$estimate * 100
  
  n_colors <- length(unique(loo_data$tag))
  palette <- grDevices::colorRampPalette(safe_colorblind_palette)(n_colors)
  
  fig_hist_loo <-
    ggplot2::ggplot(
      loo_data,
      ggplot2::aes(x = estimate, color = tag, fill = tag)
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 200,
      position = "dodge",
      alpha = .3,
      linewidth = .3
    ) +
    ggplot2::geom_vline(
      data = ref_data,
      ggplot2::aes(
        xintercept = estimate,
        color = cat,
        linetype = group
      ),
      linewidth = .9
    ) +
    ggplot2::facet_grid(var + tag ~ m) +
    ggplot2::scale_color_manual(
      name = "Subgroups",
      values = palette
    ) +
    ggplot2::scale_fill_manual(
      name = "Subgroups",
      values = palette
    ) +
    ggplot2::scale_linetype_manual(
      name = "Sample Average",
      values = c("solid","11","dashed")
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = 8,
      name = "estimate"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1, suffix = "", accuracy = 0.1)
    ) +
    ggplot2::labs(title = "", x = "") +
    ggplot2::guides(
      color = ggplot2::guide_legend(reverse = TRUE, nrow = 2, keyheight = 1),
      fill = ggplot2::guide_legend(reverse = TRUE, nrow = 2, keyheight = 1),
      linetype = ggplot2::guide_legend(keyheight = 2)
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom"
    )
  
  return(fig_hist_loo)
  
}