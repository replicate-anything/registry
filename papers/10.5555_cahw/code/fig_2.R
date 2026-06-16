generate_figure <- function(data){
  
  quality_labels <- 
    c("final_score_r2" = "Quality index",
      "as_final_score_r2" = "Animal knowledge exam",
      "fr_final_score_r2"= "Form review",
      "sao_final_score_r2" = "Sick animal observation",
      "mfu_final_score_r2" = "Form follow-up")
  
  fig_squal <- 
    data |> 
      dplyr::select(dplyr::all_of(c("final_score_r2",
                      "as_final_score_r2", 
                      "fr_final_score_r2", 
                      "sao_final_score_r2",  
                      "mfu_final_score_r2"))) |> 
      tidyr::gather(var, val) |>
      dplyr::mutate(
        var = factor(
        var, 
        names(quality_labels),
        quality_labels)) |>
      ggplot2::ggplot(ggplot2::aes(val)) +
      ggplot2::geom_histogram(
        boundary = 0,
        closed = "left",
        bins = 25)  +
      ggplot2::facet_wrap(~var, scales = "free") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = NULL)
  
  return(fig_squal)

}