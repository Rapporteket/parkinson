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
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::mutate(oppdatertBehandling = any(
      RegData$LastUpdate >= Sys.Date() - lubridate::years(2)
    )) |>
    dplyr::ungroup()

  # Mottatt avansert behandling
  # Sjekker om pasienten har ICD-10-kode G20 og er i live.
  # For hver behandlingstype (APO, PRO, DUO, DBS, LEC) opprettes først en
  # registreringsvariabel (mottatt*Reg) som er TRUE/FALSE for kvalifiserte
  # pasienter og NA ellers.
  # Deretter rulles verdiene opp til pasientnivå (group_by PasientGUID).
  # En behandling regnes som aktiv (aktiv*) dersom siste startdato er senere
  # enn siste sluttdato (eller ingen sluttdato finnes).
  # Til slutt lages en samlevariabel (mottattAvansertBehandlingPasient) som er
  # TRUE hvis pasienten har minst én aktiv avansert behandling.
  RegData <- RegData |>
    dplyr::mutate(
      mottattAPOReg = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_APO, FALSE),
        NA
      ),
      mottattPROReg = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_PRO, FALSE),
        NA
      ),
      mottattDUOReg = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_DUO, FALSE),
        NA
      ),
      mottattDBSReg = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_DBS, FALSE),
        NA
      ),
      mottattLECReg = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_LEC, FALSE),
        NA
      )
    ) |>
    # Rull opp til pasientnivå:
    # For hver behandling sjekkes det om siste startdato er senere enn siste
    # sluttdato (eller om ingen sluttdato finnes i det hele tatt). I så fall er
    # behandlingen pågående (TRUE).
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::mutate(
      aktivAPO = dplyr::if_else(
        any(.data$mottattAPOReg, na.rm = TRUE),
        max(.data$PS_APO_STDATO[.data$mottattAPOReg %in% TRUE], na.rm = TRUE) >
          dplyr::coalesce(max(.data$PS_APO_SLUTT_DATO[.data$mottattAPOReg %in% TRUE],
                              na.rm = TRUE), as.Date("1900-01-01")),
        FALSE
      ),
      aktivPRO = dplyr::if_else(
        any(.data$mottattPROReg, na.rm = TRUE),
        max(.data$PS_PRO_STDATO[.data$mottattPROReg %in% TRUE], na.rm = TRUE) >
          dplyr::coalesce(max(.data$PS_PRO_SLUTT_DATO[.data$mottattPROReg %in% TRUE],
                              na.rm = TRUE), as.Date("1900-01-01")),
        FALSE
      ),
      aktivDUO = dplyr::if_else(
        any(.data$mottattDUOReg, na.rm = TRUE),
        max(.data$PS_DUO_STDATO[.data$mottattDUOReg %in% TRUE], na.rm = TRUE) >
          dplyr::coalesce(max(.data$PS_DUO_SLUTT_DATO[.data$mottattDUOReg %in% TRUE],
                              na.rm = TRUE), as.Date("1900-01-01")),
        FALSE
      ),
      aktivDBS = dplyr::if_else(
        any(.data$mottattDBSReg, na.rm = TRUE),
        max(.data$PS_DBS_STDATO[.data$mottattDBSReg %in% TRUE], na.rm = TRUE) >
          dplyr::coalesce(max(.data$PS_DBS_SLUTT_DATO[.data$mottattDBSReg %in% TRUE],
                              na.rm = TRUE), as.Date("1900-01-01")),
        FALSE
      ),
      aktivLEC = dplyr::if_else(
        any(.data$mottattLECReg, na.rm = TRUE),
        max(.data$PS_LEC_STDATO[.data$mottattLECReg %in% TRUE], na.rm = TRUE) >
          dplyr::coalesce(max(.data$PS_LEC_SLUTT_DATO[.data$mottattLECReg %in% TRUE],
                              na.rm = TRUE), as.Date("1900-01-01")),
        FALSE
      ),
      mottattAvansertBehandlingPasient = .data$aktivAPO |
        .data$aktivPRO | .data$aktivDUO |
        .data$aktivDBS | .data$aktivLEC
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
