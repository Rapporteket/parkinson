#' Provides a dataframe containing data from a registry
#'
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
      query = "SELECT * FROM egenrapport_4"
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
      query = "SELECT * FROM egenrapport_4"
    )
    # nolint end
  }
  data <- parkPreprosess(bakgrunnSkjema, ePromSkjema)

  return(data)
}
