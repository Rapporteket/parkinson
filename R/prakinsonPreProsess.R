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
  date_cols <- c(
		"FormDate", "LastUpdate", "FirstTimeClosed", "Dato",
		"PS_HDATO", "DEB_DATO",
		"DIAG_DATO", "PS_DIAG_CT_DATO", "PS_DIAG_MR_DATO",
		"PS_DIAG_DAT_DATO", "PS_DIAG_PET_DATO"
	)

	RegData <- RegData |>
		dplyr::mutate(
			dplyr::across(
				dplyr::all_of(date_cols),
				to_date_ddmmyyyy8)
			)
	
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

	RegData$tattBilde <- RegData$PS_DIAG_CT | RegData$PS_DIAG_MR | RegData$PS_DIAG_DAT | RegData$PS_DIAG_PET
	attr(RegData, "kvalIndGrenser") <- list(
		tattBilde = c(0, 75, 90, 100)
	)
  

  return(RegData)
}