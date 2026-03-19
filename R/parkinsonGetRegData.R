#' Provides a dataframe containing data from a registry
#'
#' @param datoFra Start date for data retrieval (default: "1999-01-01")
#' @param datoTil End date for data retrieval (default: "2099-01-01")
#' @return regData data frame
#' @export

parkGetRegData <- function(datoFra = "1999-01-01", datoTil = "2099-01-01") {

  if (rapbase::isRapContext()) {
    bakgrunnSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM bakgrunnskjema_1"
    )
    # nolint start
    behandlingSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM behandlingskjema_3 LIMIT 2000"
    )
    konsultasjonSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM konsultasjonskjema_2 LIMIT 2000"
    )
    ePromSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM egenrapport_4 LIMIT 2000"
    )
    # nolint end
  } else {
    bakgrunnSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM bakgrunnskjema_1"
    )
    # nolint start
    behandlingSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM behandlingskjema_3 LIMIT 2000"
    )
    konsultasjonSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM konsultasjonskjema_2 LIMIT 2000"
    )
    ePromSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM egenrapport_4 LIMIT 2000"
    )
    # nolint end
  }

  RegData <- parkPreprosess(bakgrunnSkjema)


  return(RegData)
}
