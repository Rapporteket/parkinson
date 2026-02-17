## code to prepare internal data set

cases_year <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv"
)

licorice_gargle <- readr::read_csv2("data-raw/licorice_gargle.csv")

usethis::use_data(cases_year, licorice_gargle, internal = TRUE, overwrite = TRUE)
