generate_figure <- function(data){

  library(ggplot2)

  ggplot(data, aes(x,y)) +
    geom_line() +
    theme_minimal()

}
