# Structural Parameter Estimates — Public support for global vaccine sharing in the COVID-19 pandemic: Evidence from Germany
# Paper folder: https://github.com/replicate-anything/registry/tree/main/papers/10.1371_journal.pone.0278337
# Run from the paper's code/ folder: Rscript tab_1.R

library(dplyr)
library(ggplot2)
library(estimatr)
library(bbmle)
library(egg)
library(knitr)
library(kableExtra)

make_tab_1 <- function(data){
  maxx <- function(a,b,c,g,k,ZT,ZR,ZY,Zy) {
    AA = -2*(1+g)
    BB = -2*((1+g)*ZY - g*k*Zy)
    CC = 2*g*k*Zy*ZY + a+b*ZT+c*ZR
    (-BB - (BB^2 - 4*AA*CC)^.5)/(2*AA)
  }
  
  lik_x <- function(x, sigma, a,b,c,g,k,ZT,ZR,ZY,Zy) 
    dnorm(x, 
          maxx(a, b, c, g, k, ZT, ZR, ZY, Zy), 
          sd = sigma)
  
  df_4 <- data |> dplyr::mutate(x = cash_billions)
  
  LL  <- function(a=1,b=1,c=1,g=1,k=1, sigma=1) {
    
    R <- with(df_4, 
              lik_x(x, 
                    sigma, 
                    a, b, c, g, k,
                    ZT = trading_importance,
                    ZR = risk,
                    ZY = others_giving,
                    Zy = others_average))
    
    -sum(log(R))
  }
  
  # .a    .b    .c    .g    .k  .sigma
  # 243.   -10  36.7 0.667     5     16
  
  M <- bbmle::mle2(
    LL,
    # method = "L-BFGS-B",
    optimizer = "nlminb",
    start = list(a = 240, b = -11, c = 41, g = .8,  k = 4, sigma = 16),
    lower = list(a = 0, b = -20, c = -60, g = .01, k = -10, sigma = .02),
    upper = list(a = 400, b = 20, c = 60, g = 5, k = 10, sigma = 30))
  
  # Format output from estimation
  out <- bbmle::coef(bbmle::summary(M)) %>% data.frame()
  
  names(out) <- c("estimate", "std.error", "statistic", "p.value")
  
  # Flag: Should be able to do better ci's than this
  #out %>% mutate(conf.low = estimate - 1.96*std.error, 
   #              conf.high = estimate + 1.96*std.error) %>% kable(format = "pipe", digits = 2)
  
  
  tbl <- out %>%
    dplyr::mutate(parameter = c("α","β","δ","γ","κ","σ")) %>%
    dplyr::relocate(parameter) %>%
    dplyr::mutate(
      conf.low  = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )

  # If the Shiny app uses renderTable()/renderPrint(), returning a data.frame
  # displays an actual table, whereas HTML would appear as raw tags.
  tbl <- tbl %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

  tbl
} 


make_tab_1(utils::read.csv("../data/fig_1.csv", stringsAsFactors = FALSE))
