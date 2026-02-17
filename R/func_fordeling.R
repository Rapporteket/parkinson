# Funksjoner til modulen "mod_fordeling"

# Standard filtreringsfunksjon



#' Preprosessering
#' @param data datasett "licorice gargle"
#' @return datasett med norske nivåer og verdier
#' @export

forbered_data_fordeling <- function(data) {

  # Endre nivåene
  data <- data |>
    dplyr::mutate(
      preOp_gender = dplyr::case_match(
        .data$preOp_gender,
        0 ~ "mann",
        1 ~ "kvinne"
      ),
      preOp_smoking = dplyr::case_match(
        .data$preOp_smoking,
        1 ~ "Naa",
        2 ~ "Foer",
        3 ~ "Aldri"
      ),
      preOp_pain = dplyr::case_match(
        .data$preOp_pain,
        0 ~ "Nei",
        1 ~ "Ja"
      ),
      treat = dplyr::case_match(
        .data$treat,
        0 ~ "Sukker",
        1 ~ "Lakris"
      ),
      extubation_cough = dplyr::case_match(
        .data$extubation_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pacu30min_cough = dplyr::case_match(
        .data$pacu30min_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pacu90min_cough = dplyr::case_match(
        .data$pacu90min_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      postOp4hour_cough = dplyr::case_match(
        .data$postOp4hour_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      pod1am_cough = dplyr::case_match(
        .data$pod1am_cough,
        0 ~ "ingen hoste",
        1 ~ "mild hoste",
        2 ~ "moderat hoste",
        3 ~ "alvorlig hoste"
      ),
      intraOp_surgerySize = dplyr::case_match(
        .data$intraOp_surgerySize,
        1 ~ "liten",
        2 ~ "medium",
        3 ~ "stor"
      )
    )

  data$preOp_calcBMI_cat <- cut(
    data$preOp_calcBMI,
    breaks = c(0, 18.5, 24.9, 29.9, 34.4, 39.9, 100),
    labels = c("undervektig", "normal", "overvektig", "fedme", "moderat fedme", "alvorlig fedme")
  )

  return(data)
}


#' Funksjon som gjør utvalg basert på  ui-valg
#' @param data datafil som utgangspunkt
#' @param alder1 minste alder
#' @param alder2 høyeste alder
#' @param roek_1_2_3 røyking 1 = nå, 2 = før, 3 = aldri
#' @return datafil der utvalg er gjort
#' @export


utvalg_fordeling <- function(data, alder1, alder2, roek) {

  data <- data |>
    dplyr::filter(dplyr::between(.data$preOp_age, {{alder1}}, {{alder2}}))


  data <- data |>
    dplyr::filter(.data$preOp_smoking == dplyr::case_when(
      {{roek}} == "Naa" ~ "Naa",
      {{roek}} == "Foer" ~ "Foer",
      {{roek}} == "Aldri" ~ "Aldri",
      {{roek}} == "alle_valg" ~ preOp_smoking
    ))
  return(data)
}


#' Tabell som viser fordeling, med villkår basert på ui-valg
#' @param data datafil som har vært gjennom forbered_data_fordeling() og utvalg_fordeling()
#' @param var variabel valgt av bruker i ui-delen
#' @param valg_sammenligne_grupper "ja" eller "nei" valg av bruker i ui-delen
#' @param var_sammenligne variabel for sammenligning valg av bruker i ui-delen
#' @return datasett med antall i hver gruppe
#' @export

lag_fordeling_tabell <- function(data, var, valg_sammenligne_grupper, var_sammenligne) {

  if (valg_sammenligne_grupper == "Ja") {
    tabell <- data |>
      dplyr::group_by(.data[[var_sammenligne]]) |>
      dplyr::count(.data[[var]]) |>
      dplyr::rename("antall" = "n")
  } else {
    tabell <- data |>
      dplyr::count(.data[[var]]) |>
      dplyr::rename("antall" = "n")
  }
  return(tabell)
}


#' Plot: fordeling
#' @param data datafil som har vært gjennom forbered_data_fordeling() og utvalg_fordeling()
#' @param var variabel valgt av bruker i ui-delen
#' @param valg_sammenligne_grupper "ja" eller "nei" valg av bruker i ui-delen
#' @param var_sammenligne variabel for sammenligning valg av bruker i ui-delen
#' @return ggplot2-object som viser fordeling pr. gruppe
#' @export

lag_fordeling_plot <- function(data, var, valg_sammenligne_grupper, var_sammenligne) {

  fordeling_plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_bar(fill = "#6CACE4", alpha = .7) +
    ggplot2::ylab("Antall") +
    ggplot2::xlab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = 3, size = 15, face = "bold"))

  if (valg_sammenligne_grupper == "Ja") {
    fordeling_plot <- fordeling_plot +
      ggplot2::facet_wrap(~ .data[[var_sammenligne]])
  }

  return(fordeling_plot)

}
