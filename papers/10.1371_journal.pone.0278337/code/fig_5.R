# Levels of support by migration background — Public support for global vaccine sharing in the COVID-19 pandemic: Evidence from Germany
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1371_journal.pone.0278337
# Run from the paper's code/ folder: Rscript fig_5.R

library(dplyr)
library(ggplot2)
library(estimatr)
library(bbmle)
library(egg)

make_fig_5 <- function(data){

  amounts_d <- seq(0, 22, .5)
  
  support <- function(x, amounts = amounts_d)
    data.frame(
      amounts = amounts,
      support = sapply(amounts, function(j) 
        mean(x >= j, na.rm = TRUE)))
  
  sm <- lapply(0:1, function(m)
    support(
      data |> dplyr::filter(migration_background == m) |> dplyr::pull(cash_billions),
      amounts = amounts_d))

  names(sm) <- c("Non migrant", "Migrant")
  sm <- sm |> dplyr::bind_rows(.id = "Migration_background")

  supports_by_migration <-
    sm |> ggplot2::ggplot(ggplot2::aes(amounts, support, color = Migration_background)) + 
    ggplot2::geom_line() + 
    ggplot2::ylim(0,1) + 
    ggplot2::theme_bw() + 
    ggplot2::xlab("German contribution (bn Euro)") + 
    ggplot2::ylab("Share supporting")  + 
    ggplot2::theme(legend.position="bottom")

  return(supports_by_migration)
}


make_fig_5(utils::read.csv("../data/fig_1.csv", stringsAsFactors = FALSE))
