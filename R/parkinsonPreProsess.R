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


parkPreprosess <- function(bakgrunnSkjema, konsultasjonSkjema, promData, NPRDataskjema) {
  RegData <- bakgrunnSkjema |>
    dplyr::bind_rows(konsultasjonSkjema) |>
    dplyr::bind_rows(promData) |>
    dplyr::bind_rows(NPRDataskjema)
  
  # Fjern pasienter som ikke har bakgrunnskjema
  # Fjern pasienter med unitID == 0, 99999
  # Fjern pasienter med ugyldig ICD_10

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
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::mutate(alive = all(is.na(.data$DeathDate))) |>
    dplyr::ungroup()


  RegData <- RegData |> #Grupper per pasient
    dplyr::mutate(
      deathAge = .data$PatientAge +
        base::floor(
          lubridate::time_length(
            lubridate::interval(.data$FormDate, .data$DeathDate),
            unit = "years"
          )
        )
    )

  RegData <- RegData |> #Grupper per pasient
    dplyr::mutate(
      updatedPatientAge = .data$PatientAge +
        base::floor(
          lubridate::time_length(
            lubridate::interval(.data$FormDate, Sys.Date()),
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
    #PS_MDSUPDRS_III = TRUE hvis tall (Ikke tom)

  # Tatt bilde
  RegData <- RegData |>
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::mutate(
      tattBilde = dplyr::if_else(
        .data$alive,
        any(
          .data$PS_DIAG_CT,
          .data$PS_DIAG_MR,
          .data$PS_DIAG_DAT,
          .data$PS_DIAG_PET,
          na.rm = TRUE
        ),
        NA
      )
    ) |>
    dplyr::ungroup()


  # Oppdatert manuell registrering siste to år på bakgrunn og konsultasjon
  # Per nå bare oppdatert status, ikke behandling
  # Gjør kun for Konsultasjon og Bakgrunn -> gir oppdatert manuell registrering
  cutoff <- Sys.Date() - lubridate::years(2)
  oppdatertFlag <- RegData |>
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::summarise(
      oppdatertStatus = any(.data$LastUpdate >= cutoff, na.rm = TRUE),
      .groups = "drop"
    )
  RegData <- RegData |>
    dplyr::left_join(oppdatertFlag, by = "PasientGUID")

  # Mottatt avansert behandling
  # Sjekker om pasienten har ICD-10-kode G20 og er i live.
  # For hver behandlingstype (APO, PRO, DUO, DBS, LEC) opprettes først en
  # registreringsvariabel (mottatt*) som er TRUE/FALSE for kvalifiserte
  # pasienter og NA ellers.
  # Deretter rulles verdiene opp til pasientnivå (group_by PasientGUID).
  # En behandling regnes som aktiv (aktiv*) dersom siste startdato er senere
  # enn siste sluttdato (eller ingen sluttdato finnes).
  # Til slutt lages en samlevariabel (mottattAvansertBehandlingPasient) som er
  # TRUE hvis pasienten har minst én aktiv avansert behandling.
  RegData <- RegData |>
    dplyr::mutate(
      mottattAPO = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_APO, FALSE),
        NA
      ),
      mottattPRO = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_PRO, FALSE),
        NA
      ),
      mottattDUO = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_DUO, FALSE),
        NA
      ),
      # Mottatt DBS kan utfylles via NPR-skjema, mapping kommer fra Johannes
      mottattDBS = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_DBS, FALSE),
        NA
      ),
      mottattLEC = dplyr::if_else(
        .data$ICD_10 == "G20" & .data$alive,
        dplyr::coalesce(.data$PS_LEC, FALSE),
        NA
      )
    )
  # Rull opp til pasientnivå:
  # For hver behandling sjekkes det om siste startdato er senere enn siste
  # sluttdato (eller om ingen sluttdato finnes i det hele tatt). I så fall er
  # behandlingen pågående (TRUE).
  # Hjelpefunksjon for å sjekke om en behandling er aktiv på pasientnivå
  # Vektorisert hjelper: for hver behandling, behold kun rader der mottatt er TRUE,

  # Finn siste start- og sluttdato per pasient, og sjekk om behandlingen er aktiv.
  .aktivVec <- function(df, mottatt, start, slutt) {
    # Pasienter der mottatt er TRUE: sjekk om behandlingen er aktiv
    aktivBehandling <- df |>
      dplyr::filter(.data[[mottatt]] %in% TRUE) |>
      dplyr::group_by(.data$PasientGUID) |>
      dplyr::summarise(
        sisteStart = if (all(is.na(.data[[start]]))) as.Date(NA) else max(.data[[start]], na.rm = TRUE),
        sisteSlutt = if (all(is.na(.data[[slutt]]))) as.Date(NA) else max(.data[[slutt]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        aktiv = dplyr::if_else(
          !is.na(.data$sisteStart),
          dplyr::if_else(
            !is.na(.data$sisteSlutt),
            .data$sisteStart > .data$sisteSlutt,
            TRUE
          ),
          FALSE
        )
      ) |>
      dplyr::select("PasientGUID", "aktiv")

    # Pasienter der mottatt er FALSE (men ikke NA): sett aktiv eksplisitt til FALSE
    inaktivBehandling <- df |>
      dplyr::filter(.data[[mottatt]] %in% FALSE) |>
      dplyr::distinct(.data$PasientGUID) |>
      dplyr::anti_join(aktivBehandling, by = "PasientGUID") |>
      dplyr::mutate(aktiv = FALSE)

    dplyr::bind_rows(aktivBehandling, inaktivBehandling)
  }

  # Beregn behandlingsflagg for hver type i én vektorisert operasjon

  aktivApo <- .aktivVec(RegData, "mottattAPO", "PS_APO_STDATO", "PS_APO_SLUTT_DATO")
  aktivPro <- .aktivVec(RegData, "mottattPRO", "PS_PRO_STDATO", "PS_PRO_SLUTT_DATO")
  aktivDuo <- .aktivVec(RegData, "mottattDUO", "PS_DUO_STDATO", "PS_DUO_SLUTT_DATO")
  aktivDbs <- .aktivVec(RegData, "mottattDBS", "PS_DBS_STDATO", "PS_DBS_SLUTT_DATO")
  aktivLec <- .aktivVec(RegData, "mottattLEC", "PS_LEC_STDATO", "PS_LEC_SLUTT_DATO")

  # Hent unike pasienter, koble på alle flagg,
  # og opprett samlevariabel for avansert behandling
  aktivFlags <- RegData |>
    dplyr::distinct(.data$PasientGUID) |>
    dplyr::left_join(
      aktivApo |>
        dplyr::rename(aktivAPO = "aktiv"),
      by = "PasientGUID"
    ) |>
    dplyr::left_join(
      aktivPro |>
        dplyr::rename(aktivPRO = "aktiv"),
      by = "PasientGUID"
    ) |>
    dplyr::left_join(
      aktivDuo |>
        dplyr::rename(aktivDUO = "aktiv"),
      by = "PasientGUID"
    ) |>
    dplyr::left_join(
      aktivDbs |>
        dplyr::rename(aktivDBS = "aktiv"),
      by = "PasientGUID"
    ) |>
    dplyr::left_join(
      aktivLec |>
        dplyr::rename(aktivLEC = "aktiv"),
      by = "PasientGUID"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("aktiv"), ~ dplyr::coalesce(.x, FALSE)),
      aktivAvansertBehandling = .data$aktivAPO | .data$aktivPRO | .data$aktivDUO | .data$aktivDBS | .data$aktivLEC
    )

  RegData <- RegData |>
    dplyr::left_join(aktivFlags, by = "PasientGUID")

  # Sett grenseverdier for kvalitetsindikatorer
  attr(RegData, "kvalIndGrenser") <- list(
    tattBilde = c(0, 75, 90, 100),
    oppdatertStatus = c(0, 75, 90, 100),
    aktivAvansertBehandling = c(0, 5, 15, 100),
    StandardisertKartlegging = c(0, 75, 90, 100)
  )
  #-------------------Slutt Kvalitetsindikatorer -------------------


  #----------------------------------------------#
  #             ePromSkjema                      #
  #----------------------------------------------#
  # Må nyanseres per pasient
  # Se på siste svar fra hver pasient
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
