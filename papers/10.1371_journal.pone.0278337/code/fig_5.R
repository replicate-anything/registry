generate_figure <- function(data){

  amounts_d <- seq(0, 22, .5)
  
  support <- function(x, amounts = amounts_d)
    data.frame(
      amounts = amounts,
      support = sapply(amounts, function(j) 
        mean(x >= j, na.rm = TRUE)))
  
  sm <- lapply(0:1, function(m)
    support(
      data |> dplyr::filter(migration_background == m) |> pull(cash_billions),
      amounts = amounts_d))

  names(sm) <- c("Non migrant", "Migrant")
  sm <- sm |> bind_rows(.id = "Migration_background")

  supports_by_migration <-
    sm |> ggplot(aes(amounts, support, color = Migration_background)) + geom_line() + ylim(0,1) + theme_bw() + xlab("German contribution (bn Euro)") + ylab("Share supporting")  + theme(legend.position="bottom")

  return(supports_by_migration)
}
