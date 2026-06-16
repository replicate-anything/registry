# Appendix: Timeline — Selection and Incentives in Local Service Provision: Theory and Evidence from Sierra Leone
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.5555_cahw
# Run from the paper's code/ folder: Rscript tab_10.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(patchwork)
library(scales)

make_tab_10 <- function(data){
  tab <- kableExtra::kable(data)
  return(tab)
}


make_tab_10(utils::read.csv("../data/timeline.csv", stringsAsFactors = FALSE))
