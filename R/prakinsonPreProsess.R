#' Parkinson's Disease Preprocessing
#'
#' Preprosesserer data for Parkinson
#'
#' @param RegData Data frame containing the registry data
#'
#' @return A preprocessed data frame ready for analysis
#'
#' @export
#'


parkPreprosess <- function(RegData) {
  
	# Dato formatering
  RegData[, c("FormDate", "LastUpdate", "FirstTimeClosed", "Dato",
              "PS_HDATO", "DEB_DATO",
              "DIAG_DATO", "PS_DIAG_CT_DATO", "PS_DIAG_MR_DATO", "PS_DIAG_DAT_DATO", "PS_DIAG_PET_DATO")] <-
    dplyr::mutate_all(RegData[, c("FormDate", "LastUpdate", "FirstTimeClosed",
                                  "Dato", "PS_HDATO",
                                  "DEB_DATO", "DIAG_DATO",
                                  "PS_DIAG_CT_DATO", "PS_DIAG_MR_DATO", "PS_DIAG_DAT_DATO", "PS_DIAG_PET_DATO")],
                      list(~ as.Date(., format="%d.%m.%Y")))
	
	RegData$PatientGender <- factor(RegData$PatientGender, levels = c(1, 2), labels = c("Mann", "Kvinne"))
	
	boolPattern <- "SANN|USANN"

	boolCols <- names(RegData)[
		sapply(RegData, function(x)
			any(grepl(boolPattern, as.character(x), ignore.case = TRUE), na.rm = TRUE))
	]
	RegData <- RegData |>
		dplyr::mutate(
			dplyr::across(
				dplyr::all_of(boolCols),
				~ dplyr::case_when(
					. == "USANN" ~ 0,
					. == "SANN" ~ 1,
					TRUE ~ NA_real_
				)
			)
		)

  

  return(RegData)
}