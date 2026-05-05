generate_figure <- function(data){
  # Support sizes
  amounts_d <- seq(0, 22, .5)
  amounts_v <- seq(0, 200, 1)

  # Figures cumulative distribution
  support <- function(x = data$cash_billions, amounts = amounts_d)
    data.frame(
      amounts = amounts,
      support = sapply(amounts, function(j)
        mean(x >= j, na.rm = TRUE)))

  s1 <- list(
    Low = support(
      data %>% dplyr::filter(risk_factor == "Low" & trading_factor == "Low") %>% pull(cash_billions),
      amounts = amounts_d),
    High  =  support(
      data %>% dplyr::filter(risk_factor == "High" & trading_factor == "High")%>% pull(cash_billions), amounts = amounts_d)) %>%
    bind_rows(.id = "Costs")

  s2 <- list(
    Low = support(
      data %>% dplyr::filter(deal == "No deal") %>% pull(cash_billions)),
    High  =  support(
      data %>% dplyr::filter(deal == "40 give 40 bn") %>% pull(cash_billions))) %>%
    bind_rows(.id = "Multilateralism")


  s3 <- list(
    Low = support(
      data %>% dplyr::filter(risk_factor == "Low" & trading_factor == "Low") %>% pull(doses), amounts_v),
    High  =  support(
      data %>% dplyr::filter(risk_factor == "Low" & trading_factor == "High") %>% pull(doses), amounts_v)) %>%
    bind_rows(.id = "Costs")

  s4 <- list(
    Low = support(
      data %>% dplyr::filter(deal == "No deal") %>% pull(doses), amounts_v),
    High  =  support(
      data %>% dplyr::filter(deal == "40 give 40 bn") %>% pull(doses), amounts_v)) %>%
    bind_rows(.id = "Multilateralism")


  supports <-
    egg::ggarrange(
      s1 %>% ggplot2::ggplot(aes(amounts, support, color = Costs)) + geom_line() + ylim(0, 1) +
        theme_bw() +
        xlab("German contribution (bn Euro)") + ylab("Share supporting")  +
        theme(legend.position = "bottom"),

      s2 %>% ggplot2::ggplot(aes(amounts, support, color = Multilateralism)) + geom_line() + ylim(0, 1) +
        theme_bw() +
        xlab("German contribution (bn  Euro)") +
        ylab("Share supporting")   +
        theme(legend.position = "bottom") + ylab(""),

      s3 %>% ggplot2::ggplot(aes(amounts, support, color = Costs)) + geom_line() + ylim(0, 1) + theme_bw() + xlab("German contribution (mio vaccines)") + ylab("Share supporting")  + theme(legend.position = "bottom"),

      s4 %>% ggplot2::ggplot(aes(amounts, support, color = Multilateralism)) + geom_line() + ylim(0, 1) + theme_bw() + xlab("German contribution (mio vaccines)") + ylab("Share supporting")   + theme(legend.position =  "bottom") + ylab(""),

      nrow = 2,
      ncol = 2
    )
  return(supports)

}
