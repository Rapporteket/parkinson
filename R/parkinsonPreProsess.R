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


parkPreprosess <- function(bakgrunnSkjema, konsultasjonSkjema, promData) {
  RegData <- bakgrunnSkjema |>
    dplyr::bind_rows(konsultasjonSkjema)
  #-----------------------------------------------#
  #              Regdata                          #
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
    dplyr::mutate(atypiskDiag = .data$ICD_10 %in% c("G231", "G232", "G233", "G238", "G239"))

  


  #------- Kaviltetsindikatorer -------
  
  # Mottatt standardisert kartlegging
  RegData <- RegData |>
    dplyr::mutate(StandardisertKartlegging = .data$PS_HY != -1) # må legge til MDS-UPDRS-III
  
  # Tatt bilde
  RegData$tattBilde <- RegData$PS_DIAG_CT | RegData$PS_DIAG_MR | RegData$PS_DIAG_DAT | RegData$PS_DIAG_PET

  # Oppdatert behandling: Sjekker om LastUpdate er innenfor de siste 2 årene
  RegData <- RegData |>
  dplyr::group_by(PasientGUID) |>
    dplyr::mutate(oppdatertBehandling = any(
      RegData$LastUpdate >= Sys.Date() - lubridate::years(2)
    )) |>
    dplyr::ungroup()

  # Mottatt avansert behandling 
  # Sjekker om pasienten har ICD-10-kode G20, er i live, 
  # og har en pågående avansert behandling (APO, PRO, DUO, DBS eller LEC)
  # Sjekker først alle rader og oppdaterer for unike pasienter,
  # slik at hvis en pasient har mottatt avansert behandling i noen av registreringene, 
  # vil det reflekteres i alle registreringene for den pasienten.
  # NA if død or not G20
  RegData <- RegData |>
    dplyr::mutate(
      mottattAvansertBehandlingReg = dplyr::if_else(
        ICD_10 == "G20" & alive,
        (
          (dplyr::coalesce(PS_APO, FALSE) & is.na(PS_APO_SLUTT_DATO)) |
          (dplyr::coalesce(PS_PRO, FALSE) & is.na(PS_PRO_SLUTT_DATO)) |
          (dplyr::coalesce(PS_DUO, FALSE) & is.na(PS_DUO_SLUTT_DATO)) |
          (dplyr::coalesce(PS_DBS, FALSE) & is.na(PS_DBS_SLUTT_DATO)) |
          (dplyr::coalesce(PS_LEC, FALSE) & is.na(PS_LEC_SLUTT_DATO))
        ),
        NA
      )
    ) |>
    dplyr::group_by(PasientGUID) |>
    dplyr::mutate(
      mottattAvansertBehandlingPasient = any(
        mottattAvansertBehandlingReg,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup()

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
  #------------ Slutt ePromSkjema ---------------#

  return(list(
    RegData = RegData,
    promData = promData
  ))
}
