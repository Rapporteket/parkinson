

devtools::install("../rapbase", upgrade = FALSE)
devtools::install(".", upgrade = FALSE)
source("dev/renv.R")
run_app(browser = TRUE)

################
# MSSQL-greier #
################

source("dev/renv_mssql.R")
run_app(browser = TRUE)

con <- rapbase::rapOpenDbConnection("autoreport")$con

conf <- rapbase:::getDbConfig("autoreport")

install.packages("odbc")
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "FreeTDS",
  database = conf$name,
  server = conf$host,
  uid = conf$user,
  pwd = conf$pass,
  port = 1433
)


con <- DBI::dbDisconnect(con)
con <- NULL

con_park <- rapbase::rapOpenDbConnection("parkinson")
RegData <- readr::read_csv2(
  paste("C:/Users/pli601/repos/parkinson/data-raw/Mockdatasett_Parkinsonregister.csv")
)
RegData <- RegData |>
  dplyr::filter(
    !dplyr::if_all(dplyr::everything(), ~ is.na(.) | trimws(as.character(.)) == "")
  )
RegData <- as.data.frame(RegData)
DBI::dbWriteTable(con_park$con, "parkinsonMockData", RegData)
DBI::dbDisconnect(con_park$con)
