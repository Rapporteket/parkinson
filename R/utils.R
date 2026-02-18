#' @importFrom rlang .data
NULL


filtrerDatoIntervall <- function(data, datoColNavn, datoFra = NULL, datoTil = NULL) {
  stopifnot(is.data.frame(data))
  datoColNavn <- rlang::ensym(datoColNavn)

  filterData <- dplyr::filter(data, !is.na(!!datoColNavn))

  if (!is.null(datoFra)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn >= datoFra)
  }

  if (!is.null(datoTil)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn <= datoTil)
  }

  filterData
}