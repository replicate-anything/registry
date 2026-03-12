generate_figure <- function(data){

  
  
  if (!require(pacman)) install.packages("pacman")
  
  pacman::p_load(tidyverse, magrittr, knitr, kableExtra, DT, lazyeval, 
                 labelled, forcats, readxl, googledrive, estimatr, ggforce, fastDummies,
                 stringr, RColorBrewer, readstata13, metaplus, sjlabelled, ggrepel, tikzDevice)
  
  
  # Prep levels

  main_results <-
    data %>%
    dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) %>%
    dplyr::nest_by(group) %>%
    dplyr::summarize(
      lm_helper(data = data,
                formula = take_vaccine_num ~ 1, cluster = cluster,
                weight = weight, se_type = "stata"),
      .groups = "drop")

  # Gender
  acc_by_gender <- grp_analysis(data, y = "take_vaccine_num", x = "gender")

  # Education (all original categories and binary recoding)
  acc_by_educ_binary <- grp_analysis(data, y = "take_vaccine_num", x = "educ_binary")

  # Age (all original categories and binary recoding)
  acc_by_age <-
    grp_analysis(data, y = "take_vaccine_num", x = "age_groups_three") %>%
    dplyr::filter(statistic != Inf) %>%
    dplyr::filter(conf.low > 0)

  acc_by_age_binary <- grp_analysis(data, y = "take_vaccine_num", x = "age_groups_binary")


  # Put them together in a single df. Make estimates "percentages" and round
  ans <-
    dplyr::bind_rows(
      main_results %>% mutate(cat = "All", var = "All"),
      acc_by_gender %>% rename(cat = gender) %>% mutate(var = "By gender"),
      acc_by_educ_binary %>% rename(cat = educ_binary) %>% mutate(var = "By education"),
      acc_by_age %>% rename(cat = age_groups_three) %>% mutate(var = "By age")) %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1)))

  # Join with a tags df, which includes details on the study (national or subnational)
  tags <-
    readxl::read_excel("2_input_data/studies_info.xlsx", sheet = "sample") %>%
    dplyr::select(group = country, tag = "Geographic scope") %>%
    dplyr::left_join(filter(ans, cat == "All"), by = "group") %>%
    dplyr::mutate(tag = paste0(group, " (", tag, ", ", n, ")")) %>%
    dplyr::select(group, tag)

  # Prepare df to plot. Important but ugly relevel of factors, happening all over the code

  ans %<>%
    dplyr::left_join(tags) %>%
    dplyr::mutate(tag = ifelse(group == "All", "All LMICs", tag)) %>%
    # group_by(var) %>%
    # arrange(cat) %>%
    dplyr::mutate(
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      cat = factor(
        cat, ordered = TRUE,
        levels = rev(c("Female", "Male", "Up to Secondary",
                       "> Secondary", "<25", "25-54", "55+", "All")),
        labels = rev(c("Female", "Male", "Up to Secondary",
                       "More than Secondary", "$< 25$", "$25-54$", "$55 +$", "All"))),
      tag = gsub(pattern = " \\(", "\\\n\\(", tag))


  special_cases <-
    sort(unique(ans$tag)[grep(unique(ans$tag), pattern = "All LMICs|Russia|USA")])

  ans %<>%
    dplyr::mutate(
      tag =
        factor(x = tag, ordered = TRUE,
               levels = rev(c(sort(unique(tag)[!(unique(tag) %in% special_cases)]), special_cases))))

  ans_loo <- ans

  #Add a row averaging over only national samples

  nationals <-
    data %>%
    dplyr::filter(country == "Burkina Faso" |
                    country == "Colombia" |
                    country == "Rwanda" |
                    country == "Sierra Leone 1" |
                    country == "Sierra Leone 2")

  main_results_n <-
    nationals %>%
    dplyr::filter(dplyr::if_all(c(take_vaccine_num, cluster, weight), ~ !is.na(.))) %>%
    dplyr::nest_by(group) %>%
    dplyr::summarize(
      lm_helper(data = data,
                formula = take_vaccine_num ~ 1, cluster = cluster,
                weight = weight, se_type = "stata"),
      .groups = "drop")

  # Gender
  acc_by_gender_n <- grp_analysis(nationals, y = "take_vaccine_num", x = "gender")

  # Education (all original categories and binary recoding)
  acc_by_educ_binary_n <- grp_analysis(nationals, y = "take_vaccine_num", x = "educ_binary")

  # Age (all original categories and binary recoding)
  acc_by_age_n <-
    grp_analysis(nationals, y = "take_vaccine_num", x = "age_groups_three") %>%
    dplyr::filter(statistic != Inf) %>%
    dplyr::filter(conf.low > 0)

  ans_n <-
    dplyr::bind_rows(
      main_results_n %>% mutate(cat = "All", var = "All"),
      acc_by_gender_n %>% rename(cat = gender) %>% mutate(var = "By gender"),
      acc_by_educ_binary_n %>% rename(cat = educ_binary) %>% mutate(var = "By education"),
      acc_by_age_n %>% rename(cat = age_groups_three) %>% mutate(var = "By age")) %>%
    dplyr::mutate(across(c(conf.low, conf.high, estimate), ~ round(. * 100, digits = 1))) %>%
    filter(group == "All") %>%
    mutate(group = "All LMICs (National samples)",
           tag = "All LMICs (National samples)")


  # Prepare df to plot. Important but ugly relevel of factors, happening all over the code

  ans_n %<>%
    # group_by(var) %>%
    # arrange(cat) %>%
    dplyr::mutate(
      var = factor(var, levels = c("All", "By gender", "By education", "By age")),
      cat = factor(
        cat, ordered = TRUE,
        levels = rev(c(c("Female", "Male", "Up to Secondary",
                         "> Secondary", "<25", "25-54", "55+", "All"))),
        labels = rev(c("Female", "Male", "Up to Secondary",
                       "More than Secondary", "$< 25$", "$25-54$", "$55 +$", "All"))),
      tag = gsub(pattern = " \\(", "\\\n\\(", tag)) %>%
    bind_rows(ans)

  special_cases <-
    sort(unique(ans_n$tag)[grep(unique(ans_n$tag), pattern = "All LMICs (National samples)|All LMICs|Russia|USA")])

  ans_n %<>%
    dplyr::mutate(
      tag =
        factor(x = tag, ordered = TRUE,
               levels = rev(c(sort(unique(tag)[!(unique(tag) %in% special_cases)]), special_cases))))

  ans_national <-
    ans_n %>%
    dplyr::filter(group == "All LMICs (National samples)" & cat == "All")


  #Colour blind palette (available palletes are smaller so we expand it)
  safe_colorblind_palette <- c("#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888",
                               "#88CCEE")


  fig_1_ages <-
    ans_n %>%
    ggplot(data = ., aes(x = tag, y = estimate, color = cat)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  size = .5, width = .2, position = position_dodge(0.6)) +
    geom_point(position = position_dodge(0.6)) +
    facet_grid(. ~ var, scales = "free_x", space = "free") +
    coord_flip() +
    guides(color = guide_legend(reverse = TRUE, nrow = 2)) +
    geom_vline(xintercept = 4.5, color = "darkgrey") +
    geom_vline(xintercept = 3.5, color = "darkgrey") +
    geom_vline(xintercept = 2.5, color = "darkgrey") +
    scale_colour_manual(values = safe_colorblind_palette) +
    labs(title = "If a COVID-19 vaccine becomes available in [country], would you take it?",
         color = "Subgroups", x = "") +
    theme_bw(base_size = (base_font_size - 2)) + ylim(c(0,100)) +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0), #Default is hjust=1
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          axis.text.y = element_text(hjust = 0))

  return(fig_1_ages)


}
