generate_figure <- function(data){
  
  vs <- c("y_effort_2", "n_reports_2_std", "n_animals_2_std", "findable_2_std")
  
  fig_2 <- 
    data |> dplyr::select(dplyr::all_of(vs)) |>
    tidyr::gather(var, val) |>
    dplyr::filter(var %in% vs) |> 
    dplyr::mutate(
      var = factor(var, vs, 
                   c("CAHW performance index", 
                     "Reports submitted", 
                     "Animals examined", 
                     "Findable CAHW"))) |>
    ggplot2::ggplot(ggplot2::aes(val)) +
    ggplot2::geom_histogram(boundary = 0, closed = "left", bins = 25)  + 
    ggplot2::facet_wrap(~var, scales = "free") + 
    ggplot2::theme_bw() + 
    ggplot2::labs(x = NULL)
  
  return(fig_2)
}