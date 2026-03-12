generate_table <- function(data){

  knitr::kable(
    data,
    caption = "Summary statistics for gender, age, education",
    col.names = c("Study", "% Women",
                  "% Age in [18,30)", "% Age in [30,45)", "% Age in [45,60)", "% Age 60+",
                  "% Up to Secondary", "% More than Secondary"),
    booktabs = T, linesep = "", align = c("l", rep("c", 7)), digits = 1) %>%
    kableExtra::kable_styling(full_width = FALSE) %>%
    kableExtra::footnote(
      general = "This table presents summarys statistics for our data and compares it with estimates from other sources of data. Data for Russia comes from census data from the Statistical Agency. For the USA, we use data from the 2019 American Community Survey. For all other countries, the Wellcome Global Monitor 2018 was used. Statistics for our surveys are not weighted, while estimates from benchmark sources are obtained using sampling weights.") %>%
    kableExtra::column_spec(1:8, width = "5em")
}
