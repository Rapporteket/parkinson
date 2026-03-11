#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

parkGetRegData <- function(datoFra = "1999-01-01", datoTil = "2099-01-01") {

  registryName <- "parkinson"
  if (rapbase::isRapContext()) {
    bakgrunnSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM bakgrunnskjema_1 LIMIT 2000")
    behandlingSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM behandlingskjema_3 LIMIT 2000")
    konsultasjonSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM konsultasjonskjema_2 LIMIT 2000")
    ePromSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM egenrapport_4 LIMIT 2000")
  } else {
    bakgrunnSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM bakgrunnskjema_1 LIMIT 2000")
    behandlingSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM behandlingskjema_3 LIMIT 2000")
    konsultasjonSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM konsultasjonskjema_2 LIMIT 2000")
    ePromSkjema <- rapbase::loadRegData(
      registryName,
      "SELECT * FROM egenrapport_4 LIMIT 2000")
  }

  RegData <- parkPreprosess(bakgrunnSkjema)


  return(RegData)
}
