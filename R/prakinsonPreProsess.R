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
  
	# -------------- Dato formatering -------------
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
	
	
	RegData$H_TO_DIAG <- tidsDiffDager(RegData$PS_HDATO, RegData$DIAG_DATO)

	# -------- Slutt Dato formatering -------------

	RegData$PatientGender <- factor(RegData$PatientGender, levels = c(0, 1, 2), labels = c("Ukjent", "Mann", "Kvinne"))
	
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


	#------- Kaviltetsindikatorer -------
	# Hvis en form for bilde er tatt, så settes tattBilde til 1, ellers 0
	RegData$tattBilde <- RegData$PS_DIAG_CT | RegData$PS_DIAG_MR | RegData$PS_DIAG_DAT | RegData$PS_DIAG_PET

	# Oppdatert behandling: Sjekker om LastUpdate er innenfor de siste 2 årene
	RegData$oppdatertBehandling <- RegData$LastUpdate >= Sys.Date() - lubridate::years(4) # MÅ ENDRES TIL 2 FOR EKTE DATA
	
	# Mottatt avansert behandling
	RegData <- RegData |> dplyr::mutate(
		mottattAvansertBehandling = dplyr::if_else(
				RegData$ICD_10 == "G20", RegData$PS_APO | RegData$PS_DUO | RegData$PS_DBS | RegData$PS_LEC, NA
			)
	)
	# Definer kvalitetsindikatorgrenser
	attr(RegData, "kvalIndGrenser") <- list(
		tattBilde = c(0, 75, 90, 100),
		oppdatertBehandling = c(0, 75, 90, 100),
		mottattAvansertBehandling = c(0, 5, 15, 100)
	)
	#-------------------Slutt Kvalitetsindikatorer -------------------



  return(RegData)
}