# Levels of support by party — Public support for global vaccine sharing in the COVID-19 pandemic: Evidence from Germany
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1371_journal.pone.0278337
# Run from the paper's code/ folder: Rscript fig_4.R

library(dplyr)
library(ggplot2)
library(estimatr)
library(bbmle)
library(egg)

make_fig_4 <- function(data){

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


make_fig_4(utils::read.csv("../data/fig_1.csv", stringsAsFactors = FALSE))
