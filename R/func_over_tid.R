#' Lager en tabell som viser antall registreringer per år
#' @param data RegData-datasettet
#' @return DT::datatable-objekt
#' @export

makeYearCountTable <- function(data, varChoice) {
  choices <- c(
    "registreringer" = "Nye registreringer per år",
    "pasienter_i_live" = "Antall pasienter i live ved slutten av året",
    "alder_nye_pasienter" = "Gjennomsnittsalder for nye pasienter per år",
    "antallDodsfall" = "Antall dødsfall blant pasienter per år",
    "alder_pasienter_i_live" = "Gjennomsnittsalder
    for pasienter i live ved slutten av året"
  )
  title <- choices[[varChoice]]

  tabellData <- lagSummaryTable(data, varChoice)
  DT::datatable(
    tabellData,
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-size: 140%; color: #2C3E50;",
      title
    ),
    options = list(
      dom = "t",
      paging = TRUE
    )
  )
}
#' Lager et søylediagram med stablet fyll for G20 og atypisk per år
#' @param data RegData-datasettet
#' @param varChoice Valgt variabel for oppsummering
#' @return ggplot-objekt
#' @export

makeYearCountStackedPlot <- function(data, varChoice) {
  choices <- c(
    "registreringer" = "Nye registreringer per år",
    "pasienter_i_live" = "Antall pasienter i live ved slutten av året",
    "alder_nye_pasienter" = "Gjennomsnittsalder for nye pasienter per år",
    "antallDodsfall" = "Antall dødsfall blant pasienter per år",
    "alder_pasienter_i_live" = "Gjennomsnittsalder
    for pasienter i live ved slutten av året"
  )
  title <- choices[[varChoice]]

  tabellData <- lagSummaryTable(data, varChoice)
  if (nrow(tabellData) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = "Ingen data tilgjengelig med denne filtreringen"
        ) +
        ggplot2::theme_void()
    )
  }

  tabellData <- tabellData |>
    dplyr::filter(.data$gruppe != "alle") |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of("gruppe"),
      names_to = "year",
      values_to = "n"
    ) |>
    dplyr::mutate(
      gruppe = factor(.data$gruppe, levels = c("Atypisk", "G20")),
      year = factor(.data$year)
    )

  position <- if (varChoice %in% c(
    "alder_nye_pasienter",
    "alder_pasienter_i_live"
  )) {
    ggplot2::position_dodge(width = 0.8)
  } else {
    ggplot2::position_stack()
  }

  value_label <- if (varChoice %in% c(
    "alder_nye_pasienter",
    "alder_pasienter_i_live"
  )) "Alder" else "Antall"

  ggplot2::ggplot(
    tabellData,
    ggplot2::aes(
      x = .data$year,
      y = .data$n,
      fill = .data$gruppe,
      text = paste0(
        "Gruppe: ", .data$gruppe,
        "<br>", value_label, ": ", .data$n
      )
    )
  ) +
    ggplot2::geom_col(width = 0.8, position = position) +
    ggplot2::scale_fill_manual(
      values = c("G20" = "#2171b5", "Atypisk" = "#4292c6"),
      name = "Diagnose"
    ) +
    ggplot2::labs(x = "\u00c5r", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 16, colour = "#2C3E50"
      ),
      axis.line.x = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      ),
      axis.line.y = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      ),
      legend.position = "top right",
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15)
    )
}
lagSummaryTable <- function(data, varChoice = "registreringer") {
  years <- 2022:2026

  summarise_groups <- function(df, agg_fn) {
    df |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        alle = agg_fn(.data$ICD_10 == "G20" | .data$atypiskDiag),
        G20 = agg_fn(.data$ICD_10 == "G20"),
        Atypisk = agg_fn(.data$atypiskDiag),
        .groups = "drop"
      )
  }

  reshape_output <- function(df) {
    df |>
      tidyr::pivot_longer(c("alle", "G20", "Atypisk"),
        names_to = "gruppe", values_to = "n"
      ) |>
      tidyr::pivot_wider(names_from = "year", values_from = "n") |>
      dplyr::mutate(
        gruppe = factor(.data$gruppe, levels = c("alle", "G20", "Atypisk"))
      ) |>
      dplyr::arrange(.data$gruppe)
  }

  if (varChoice == "registreringer") {
    data |>
      dplyr::mutate(year = as.integer(format(.data[["FormDate"]], "%Y"))) |>
      dplyr::filter(.data$year %in% years) |>
      summarise_groups(\(mask) sum(mask, na.rm = TRUE)) |>
      reshape_output()

  } else if (varChoice == "pasienter_i_live") {
    data |>
      dplyr::select("PasientGUID", "ICD_10", "atypiskDiag", "FormDate", "DeathDate") |>
      dplyr::distinct() |>
      tidyr::crossing(year = years) |>
      dplyr::filter(
        .data$FormDate <= as.Date(paste0(.data$year, "-12-31")),
        is.na(.data$DeathDate) | .data$DeathDate >= as.Date(paste0(.data$year, "-12-31"))
      ) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        alle = dplyr::n_distinct(
          .data$PasientGUID[.data$ICD_10 == "G20" | .data$atypiskDiag]
        ),
        G20 = dplyr::n_distinct(.data$PasientGUID[.data$ICD_10 == "G20"]),
        Atypisk = dplyr::n_distinct(.data$PasientGUID[.data$atypiskDiag]),
        .groups = "drop"
      ) |>
      reshape_output()

  } else if (varChoice == "alder_nye_pasienter") {
    data |>
      dplyr::mutate(year = as.integer(format(.data[["FormDate"]], "%Y"))) |>
      dplyr::filter(.data$year %in% years) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        alle = mean(
          .data$PatientAge[.data$ICD_10 == "G20" | .data$atypiskDiag], na.rm = TRUE
        ),
        G20 = mean(.data$PatientAge[.data$ICD_10 == "G20"], na.rm = TRUE),
        Atypisk = mean(.data$PatientAge[.data$atypiskDiag], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(dplyr::across(c("alle", "G20", "Atypisk"), \(x) round(x, 1))) |>
      reshape_output()

  } else if (varChoice == "alder_pasienter_i_live") {
    data |>
      dplyr::select("PasientGUID", "ICD_10", "atypiskDiag", "FormDate", "DeathDate", "PatientAge") |>
      dplyr::distinct() |>
      tidyr::crossing(year = years) |>
      dplyr::filter(
        .data$FormDate <= as.Date(paste0(.data$year, "-12-31")),
        is.na(.data$DeathDate) | .data$DeathDate >= as.Date(paste0(.data$year, "-12-31"))
      ) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        alle = mean(
          .data$PatientAge[.data$ICD_10 == "G20" | .data$atypiskDiag], na.rm = TRUE
        ),
        G20 = mean(.data$PatientAge[.data$ICD_10 == "G20"], na.rm = TRUE),
        Atypisk = mean(.data$PatientAge[.data$atypiskDiag], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(dplyr::across(c("alle", "G20", "Atypisk"), \(x) round(x, 1))) |>
      reshape_output()

  } else if (varChoice == "antallDodsfall") {
    data |>
      dplyr::filter(!is.na(.data$DeathDate)) |>
      dplyr::mutate(year = as.integer(format(.data[["DeathDate"]], "%Y"))) |>
      dplyr::filter(.data$year %in% years) |>
      dplyr::select("PasientGUID", "ICD_10", "atypiskDiag", "year") |>
      dplyr::distinct() |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(
        alle = dplyr::n_distinct(
          .data$PasientGUID[.data$ICD_10 == "G20" | .data$atypiskDiag]
        ),
        G20 = dplyr::n_distinct(.data$PasientGUID[.data$ICD_10 == "G20"]),
        Atypisk = dplyr::n_distinct(.data$PasientGUID[.data$atypiskDiag]),
        .groups = "drop"
      ) |>
      reshape_output()
  }
}
