generate_figure <- function(data){
  
  quality_labels <- 
    c("final_score_r2" = "Quality index",
      "as_final_score_r2" = "Animal knowledge exam",
      "fr_final_score_r2"= "Form review",
      "sao_final_score_r2" = "Sick animal observation",
      "mfu_final_score_r2" = "Form follow-up")
  
  fig_squal <- 
    data |> 
      select(all_of(c("final_score_r2", 
                      "as_final_score_r2", 
                      "fr_final_score_r2", 
                      "sao_final_score_r2",  
                      "mfu_final_score_r2"))) |> 
      gather(var, val) |>
      mutate(
        var = factor(
        var, 
        names(quality_labels),
        quality_labels)) |>
      ggplot(aes(val)) + 
      geom_histogram(
        boundary = 0, 
        closed = "left", 
        bins = 25)  + 
      facet_wrap(~var, scales = "free") + 
      theme_bw() + 
      labs(x = NULL)
  
  return(fig_squal)

}