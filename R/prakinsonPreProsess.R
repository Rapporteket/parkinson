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


parkPreprosess <- function(RegData, promData) {
  #-----------------------------------------------#
  #              Bakgunnskjema                    #
  #-----------------------------------------------#
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
        to_date_ddmmyyyy8
      )
    )

  RegData$H_TO_DIAG <- tidsDiffDager(RegData$PS_HDATO, RegData$DIAG_DATO)

  # -------- Slutt Dato formatering -------------

  RegData$PatientGender <- factor(RegData$PatientGender, levels = c(0, 1, 2), labels = c("Ukjent", "Mann", "Kvinne"))

  RegData <- RegData |>
    dplyr::mutate(alive = is.na(.data$DeathDate))
  RegData <- RegData |>
    dplyr::mutate(
      deathAge = .data$PatientAge +
        base::floor(
          lubridate::time_length(
            lubridate::interval(.data$FormDate, .data$DeathDate),
            unit = "years"
          )
        )
    )

  RegData <- RegData |>
    dplyr::mutate(atypiskDiag = .data$ICD_10 %in% c("G231", "G232", "G233", "G238"))

  RegData <- RegData |>
    dplyr::mutate(StandardisertKartlegging = .data$PS_HY != -1) # må legge til MDS-UPDRS-III


  #------- Kaviltetsindikatorer -------
  # Hvis en form for bilde er tatt, så settes tattBilde til 1, ellers 0
  RegData$tattBilde <- RegData$PS_DIAG_CT | RegData$PS_DIAG_MR | RegData$PS_DIAG_DAT | RegData$PS_DIAG_PET

  # Oppdatert behandling: Sjekker om LastUpdate er innenfor de siste 2 årene
  RegData$oppdatertBehandling <- RegData$LastUpdate >= Sys.Date() - lubridate::years(2)

  # Mottatt avansert behandling
  RegData <- RegData |>
    dplyr::mutate(
      mottattAvansertBehandling = dplyr::if_else(
        .data$ICD_10 == "G20",
        .data$PS_APO | .data$PS_PRO | .data$PS_DUO | .data$PS_DBS | .data$PS_LEC,
        NA
      )
    )
  # Definer kvalitetsindikatorgrenser
  # nolint start
  # kvalIndDf <- jsonlite::fromJSON(
  #   "https://prod-api.skde.org/data/parkinson/indicators?unit_name[]=Nasjonalt&year=2024&type=ind"
  # ) |>
  #   dplyr::select(.data$ind_id, .data$level_green, .data$level_yellow, .data$level_direction) |>
  #   dplyr::mutate(
  #     ind_id = dplyr::recode(
  #       .data$ind_id,
  #       "parkinson_bildedia" = "bilde",
  #       "parkinson_eprom" = "eprom",
  #       "parkinson_syst_und" = "sysUnd"
  #     )
  #   )
  # nolint end

  attr(RegData, "kvalIndGrenser") <- list(
    tattBilde = c(0, 75, 90, 100),
    oppdatertBehandling = c(0, 75, 90, 100),
    mottattAvansertBehandling = c(0, 5, 15, 100),
    StandardisertKartlegging = c(0, 75, 90, 100)
  )
  #-------------------Slutt Kvalitetsindikatorer -------------------


  #----------------------------------------------#
  #             ePromSkjema                      #
  #----------------------------------------------#
  promData <- promData |>
    dplyr::mutate(
      tilfredsSpesialist = dplyr::if_else(
        .data$DHT_TILFREDS_SPESIALIST %in% 0:4,
        .data$DHT_TILFREDS_SPESIALIST %in% c(3, 4),
        NA
      )
    )
  attr(promData, "kvalIndGrenser") <- list(
    tilfredsSpesialist = c(0, 40, 60, 100)
  )

  return(list(
    RegData = RegData,
    promData = promData
  ))
}
