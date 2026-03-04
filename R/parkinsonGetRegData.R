#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

parkGetRegData <- function(datoFra = "1999-01-01", datoTil = "2099-01-01") {

  registryName <- "data"
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
    rapbase::loadRegData(registryName, query)
    # nocov end
  } else {
    Bakgrunnskjema <- rapbase::loadRegData("data", "SELECT * FROM Bakgrunnskjema;")
    Behandlingskjema <- rapbase::loadRegData("data", "SELECT * FROM Behandlingskjema;")
    Konsultasjonskjema <- rapbase::loadRegData("data", "SELECT * FROM Konsultasjonskjema;")

    RegData <- rbind(
      Bakgrunnskjema,
      Behandlingskjema,
      Konsultasjonskjema
    )

    RegData <- parkPreprosess(RegData)
  }


  return(RegData)
}
