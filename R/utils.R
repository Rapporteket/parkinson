#' @importFrom rlang .data
NULL


filtrerDatoIntervall <- function(data, datoColNavn, datoFra = NULL, datoTil = NULL) {
  stopifnot(is.data.frame(data))

  datoColNavn <- rlang::ensym(datoColNavn)

  # sørg for Date-type
  data[[rlang::as_string(datoColNavn)]] <- as.Date(data[[rlang::as_string(datoColNavn)]], format = "%d-%m-%Y")

  filterData <- dplyr::filter(data, !is.na(!!datoColNavn))

  if (!is.null(datoFra)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn >= as.Date(datoFra, format = "%Y-%m-%d"))
  }

  if (!is.null(datoTil)) {
    filterData <- dplyr::filter(filterData, !!datoColNavn <= as.Date(datoTil, format = "%Y-%m-%d"))
  }

  filterData
}


to_date_ddmmyyyy8 <- function(x) {
  s <- trimws(as.character(x))
  s[s == ""] <- NA

  ok <- !is.na(s) & grepl("^\\d{7,8}(\\.0+)?$", s)
  if (any(ok)) s[ok] <- sprintf("%08d", as.integer(sub("\\.0+$", "", s[ok])))

  out <- rep(as.Date(NA), length(s))

  ok3 <- !is.na(s) & grepl("^\\d{8}$", s)
  out[ok3] <- as.Date(s[ok3], "%d%m%Y")

  ok2 <- is.na(out) & !is.na(s) & grepl("^\\d{2}\\.\\d{2}\\.\\d{4}", s)
  out[ok2] <- as.Date(substr(s[ok2], 1, 10), "%d.%m.%Y")

  ok4 <- is.na(out) & !is.na(s) & grepl("^\\d{4}-\\d{2}-\\d{2}", s)
  out[ok4] <- as.Date(substr(s[ok4], 1, 10), "%Y-%m-%d")

  out
}


tidsDiffDager <- function(start, slutt) {
	diffDager <- slutt - start
	diffDager
}