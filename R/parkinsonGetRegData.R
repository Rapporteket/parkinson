#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

parkGetRegData <- function(datoFra = '2017-01-01', datoTil = '2099-01-01') {

  registryName <- "data"
  dbType <- "mysql"
  if (rapbase::isRapContext()) {
     # nocov start
        query <- "
      SELECT
        AvdRESH AS Avdeling,
        COUNT(*) AS n
      FROM
        parkinson_skjema 
        AvdRESH;
      "
    rapbase::loadRegData(registryName)
  } else {
    dateCols <- c("FormDate", "LastUpdate", "FirstTimeClosed", "Dato",
              "PS_HDATO", "DEB_DATO",
              "DIAG_DATO", "PS_DIAG_CT_DATO", "PS_DIAG_MR_DATO", "PS_DIAG_DAT_DATO", "PS_DIAG_PET_DATO")

    firstRow <- rapbase::loadRegData("data", "SELECT * FROM parkinsonmockdata LIMIT 1")
    boolPattern <- "SANN|USANN"

    boolCols <- names(firstRow)[
      sapply(firstRow, function(x) grepl(boolPattern, as.character(x[1]), ignore.case = TRUE))
    ]
    query <- paste0(
      "SELECT ",
      paste(dateCols, collapse = ", "), ", ",
      paste(boolCols, collapse = ", "), ", ",
      "PatientGender, ",
      "HealthUnitShortName, ",
      "RHF ",
      "FROM parkinsonmockdata;"
    )


    RegData <- rapbase::loadRegData("data", query)

    RegData <- parkPreprosess(RegData)
  }

  
  return(RegData)
}
