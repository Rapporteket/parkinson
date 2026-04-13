# Funksjoner til modulen "mod_fordeling"

# Standard filtreringsfunksjon


#' Preprosessering
#' @param data Data frame med data for plottene
#' @param fordelingsVariabel Variabelen som skal brukes for å gruppere dataene i plottene
#' @return datasett med norske nivåer og verdier
#' @export

plotMedFordeling <- function(data, fordelingsVariabel) {
  medKolonner <- c("PS_APO", "PS_DUO", "PS_DBS", "PS_LEC", "PS_PRO")

  medNavn <- c(
    PS_APO = "Apomorfin",
    PS_DUO = "Duodopa",
    PS_DBS = "DBS",
    PS_LEC = "LEC",
    PS_PRO = "Produodopa"
  )

  colors <- c(
    Apomorfin = "#c6dbef",
    Duodopa = "#6baed6",
    DBS = "#4292c6",
    LEC = "#2171b5",
    Produodopa = "#084594"
  )

  data_long <- data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(medKolonner),
      names_to = "medicine",
      values_to = "used"
    ) |>
    dplyr::filter(.data$used == TRUE) |>
    dplyr::mutate(
      group = as.character(.data[[fordelingsVariabel]]),
      medicine = factor(
        .data$medicine,
        levels = names(medNavn),
        labels = unname(medNavn)
      )
    )

  total_row <- data_long |>
    dplyr::mutate(group = "Hele landet")

  data_plot <- dplyr::bind_rows(data_long, total_row) |>
    dplyr::count(.data$group, .data$medicine, name = "n") |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(
      prop = .data$n / sum(.data$n),
      tooltip = paste0(
        "<br>", .data$medicine,
        "<br>Antall: ", .data$n,
        "<br>Andel: ", scales::percent(.data$prop, accuracy = 0.1)
      )
    ) |>
    dplyr::ungroup()

  ggplot2::ggplot(
    data_plot,
    ggplot2::aes(
      x = .data$group,
      y = .data$prop,
      fill = .data$medicine,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_col(
      width = 0.9
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::percent
    ) +
    ggplot2::scale_fill_manual(
      values = colors,
      drop = FALSE
    ) +
    ggplot2::labs(
      y = "Andel av total medisinsk bruk",
      fill = "Medisin:"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "black"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 11),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 15,
        r = 70,
        b = 15,
        l = 20
      )
    )
}

makeAgeDistributionPlot <- function(data, ageCol, genderCol) {
  fillColors <- c(
    "#4292c6",
    "#2171b5"
  )

  plotData <- data |>
    dplyr::filter(
      !is.na(.data[[ageCol]]),
      !is.na(.data[[genderCol]])
    ) |>
    dplyr::count(
      age = .data[[ageCol]],
      gender = .data[[genderCol]],
      name = "n"
    ) |>
    dplyr::mutate(
      age = as.character(.data$age),
      n_plot = dplyr::case_when(
        as.character(.data$gender) == "Mann" ~ -n,
        as.character(.data$gender) == "Kvinne" ~ n,
        TRUE ~ 0
      ),
      tooltip = paste0("n: ", .data$n, "\nAlder: ", .data$age)
    ) |>
    dplyr::arrange(as.numeric(.data$age)) |>
    dplyr::mutate(
      age = factor(.data$age, levels = unique(.data$age))
    )

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$age,
      y = .data$n_plot,
      fill = .data$gender,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_col(
      width = 0.95,
      position = "identity"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = base::abs,
      breaks = scales::pretty_breaks(n = 5)
    ) +
    ggplot2::scale_x_discrete(
      breaks = seq(0, 110, by = 5)
    ) +
    ggplot2::scale_fill_manual(
      values = fillColors
    ) +
    ggplot2::labs(
      x = "Alder",
      y = "Antall",
      fill = "Kjønn"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
}
