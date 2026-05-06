generate_figure <- function(data){
  
  vs <- c("y_effort_2", "n_reports_2_std", "n_animals_2_std", "findable_2_std")
  
  fig_2 <- 
    data |> select(all_of(vs)) |> 
    gather(var, val) |>
    filter(var %in% vs) |> 
    mutate(
      var = factor(var, vs, 
                   c("CAHW performance index", 
                     "Reports submitted", 
                     "Animals examined", 
                     "Findable CAHW"))) |>
    ggplot(aes(val)) + 
    geom_histogram(boundary = 0, closed = "left", bins = 25)  + 
    facet_wrap(~var, scales = "free") + 
    theme_bw() + 
    labs(x = NULL)
  
  return(fig_2)
}