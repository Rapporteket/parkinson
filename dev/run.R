

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
  paste("C:/Users/pli601/repos/parkinson_datadump/Mockdatasett_Parkinsonregister.csv"), col_types = readr::cols(.default = readr::col_character()),
        trim_ws = TRUE, n_max = 700

)

RegData <- RegData |>
  dplyr::filter(
    !dplyr::if_all(dplyr::everything(), ~ is.na(.) | trimws(as.character(.)) == "")
  )

RegData <- as.data.frame(RegData)
RegDataList <- split(RegData, RegData$Skjematype)
DBI::dbWriteTable(con_park$con, "Bakgrunnskjema", RegDataList$Bakgrunnskjema, overwrite = TRUE)
DBI::dbWriteTable(con_park$con, "Behandlingskjema", RegDataList$Behandlingskjema, overwrite = TRUE)
DBI::dbWriteTable(con_park$con, "Konsultasjonskjema", RegDataList$Konsultasjonskjema, overwrite = TRUE)

DBI::dbDisconnect(con_park$con)


