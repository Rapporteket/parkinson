#' @importFrom rlang .data
NULL


filtrerDatoIntervall <- function(data, datoColNavn, datoFra = NULL, datoTil = NULL) {
  stopifnot(is.data.frame(data))

  datoColNavn <- rlang::ensym(datoColNavn)

  # sørg for Date-type
  data[[rlang::as_string(datoColNavn)]] <- as.Date(data[[rlang::as_string(datoColNavn)]], format = "%d.%m.%Y")

  filterData <- dplyr::filter(data, !is.na(!!datoColNavn))

  if (!is.null(datoFra)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn >= as.Date(datoFra))
  }

  if (!is.null(datoTil)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn <= as.Date(datoTil))
  }

  filterData
}


to_date_ddmmyyyy8 <- function(x) {
  s <- trimws(as.character(x))
  s[s == ""] <- NA

  # behold bare de som er eksakt 8 siffer
  ok <- !is.na(s) & grepl("^\\d{8}$", s)

  out <- as.Date(rep(NA_character_, length(s)))
  out[ok] <- as.Date(s[ok], format = "%d%m%Y")
  out
}