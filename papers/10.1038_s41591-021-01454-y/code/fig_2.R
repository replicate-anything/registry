generate_figure <- function(data){

  # Loo estimates for all LMIC respondents
  loo_estimates <-
    plyr::ldply(
      .data = list(1,2),
      .fun = function(x) {
        df %>%
          dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) %>%
          dplyr::filter(!(country %in% c("USA", "Russia"))) %>%
          loo_helper(., "country", loo_n = x) %>%
          dplyr::mutate(m = paste0("Leaving ", x, " out"), tag = "All", var = "All")
      })

  # Loo estimates broken down by category (appended to above)

  loo_estimates <-
    plyr::mdply(
      .data = expand_grid(x = c(1,2), var = c("gender", "educ_binary", "age_groups_three")),
      .fun = function(x, var) {
        data %>%
          dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) %>%
          dplyr::filter(group == "All") %>%
          loo_helper(., "country",
                     loo_n = x,
                     loo_fun = function(dat) grp_analysis(dat, y = "take_vaccine_num", x = var)) %>%
          dplyr::mutate(m = paste0("Leaving ", x, " out"), var = var)
      }) %>%
    dplyr::mutate(tag = dplyr::coalesce(gender, educ_binary, age_groups_three),
                  var = plyr::mapvalues(var,
                                        from = c("gender", "educ_binary", "age_groups_three"),
                                        to = c("By gender", "By education", "By age"))) %>%
    dplyr::bind_rows(loo_estimates, .)

  # Clean up labels
  loo_estimates %<>%
    dplyr::mutate(
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      tag = factor(
        tag, ordered = TRUE,
        levels = rev(c("Female", "Male", "Up to Secondary",
                       "> Secondary", "<25", "25-54", "55+", "All")),
        labels = rev(c("Female", "Male", "Up to Secondary",
                       "More than Secondary", "<25 yr", "25-54 yr", "55+ yr", "All")))
    ) %>%
    dplyr::select(var, m, tag, estimate)

  safe_colorblind_palette <- c("#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC",
                               "#888888", "#88CCEE")

  ans_loo %<>%
    dplyr::filter(group %in% c("All", "USA", "Russia")) %>%
    dplyr::mutate(
      group = plyr::mapvalues(group, from = "All", to = "All LMIC"),
      cat = plyr::mapvalues(cat, from = c("$< 25$", "$25-54$", "$55 +$"),
                            to = c("<25 yr", "25-54 yr", "55+ yr")),
      tag = cat)

  fig_hist_loo <-
    loo_estimates %>%
    dplyr::mutate(estimate = estimate*100) %>%
    ggplot(aes(estimate, color = tag, fill = tag)) +
    geom_histogram(aes(y = ..density..), bins = 200,
                   position = "dodge", alpha = .3, size = .3) +
    geom_vline(data = ans_loo,
               aes(xintercept = estimate, color = cat, linetype = group), size = .9) +
    facet_grid_paginate(var + tag ~ m)  +
    scale_color_manual(
      name = "Subgroups",
      values = safe_colorblind_palette) +
    scale_fill_manual(
      name = "Subgroups",
      values = safe_colorblind_palette) +
    scale_linetype_manual(
      name = "Sample Average",
      values = c("solid", "11", "dashed")) +
    scale_x_continuous(n.breaks = 8, name = "estimate") +
    scale_y_continuous(
      labels = scales::percent_format(scale = 1, suffix = "", accuracy = 0.1)) +
    labs(title = "",
         x = "") +
    guides(color = guide_legend(reverse = TRUE, nrow = 2, keyheight = 1),
           fill = guide_legend(reverse = TRUE, nrow = 2, keyheight = 1),
           linetype = guide_legend(keyheight = 2)) +
    theme_bw(base_size = (base_font_size - 2)) +
    theme(legend.position = "bottom")


  return(fig_hist_loo)


}
