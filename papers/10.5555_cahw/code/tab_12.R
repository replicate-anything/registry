# Appendix: Balance table — Selection and Incentives in Local Service Provision: Theory and Evidence from Sierra Leone
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.5555_cahw
# Run from the paper's code/ folder: Rscript tab_12.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(texreg)
library(patchwork)
library(scales)

make_tab_12 <- function(data){


}

make_tab_12(list(
  lm_attrition_hh = readRDS("../data/lm_attrition_hh.rds"),
  lm_attrition_main = readRDS("../data/lm_attrition_main.rds"),
  lm_attrition = readRDS("../data/lm_attrition.rds")
))
