generate_figure <- function(data){

  amounts_d <- seq(0, 22, .5)

  support <- function(x, amounts = amounts_d)
    data.frame(
      amounts = amounts,
      support = sapply(amounts, function(j) 
        mean(x >= j, na.rm = TRUE)))
  
  sp <- lapply(unique(data$party), function(p)
    support(
      data |> dplyr::filter(party == p) |> dplyr::pull(cash_billions),
      amounts = amounts_d))

  names(sp) <- unique(data$party)

  sp <- sp |> dplyr::bind_rows(.id = "Party")

  supports_by_party <-
    sp |> ggplot2::ggplot(ggplot2::aes(amounts, support, color = Party)) + 
    ggplot2::geom_line() + 
    ggplot2::ylim(0,1) + 
    ggplot2::theme_bw() + 
    ggplot2::xlab("German contribution (bn Euro)") + 
    ggplot2::ylab("Share supporting")  + 
    ggplot2::theme(legend.position="bottom")

  return(supports_by_party)
}
