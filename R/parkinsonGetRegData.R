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

    konsultasjonSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM konsultasjonskjema_2"
    )
    ePromSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM egenrapport_4"
    )
    NPRDataskjema <- rapbase::loadRegData(
      query = "SELECT * FROM nprdataskjema_7"
    )
    # nolint end
  } else {
    bakgrunnSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM bakgrunnskjema_1"
    )
    # nolint start
    konsultasjonSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM konsultasjonskjema_2"
    )
    ePromSkjema <- rapbase::loadRegData(
      query = "SELECT * FROM egenrapport_4"
    )

    NPRDataskjema <- rapbase::loadRegData(
      query = "SELECT * FROM nprdataskjema_7"
    )
    # nolint end
  }
  data <- parkPreprosess(bakgrunnSkjema, konsultasjonSkjema, ePromSkjema, NPRDataskjema)

  return(data)
}
