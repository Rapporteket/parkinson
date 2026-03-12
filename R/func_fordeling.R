# Funksjoner til modulen "mod_fordeling"

# Standard filtreringsfunksjon



#' Preprosessering
#' @param data datasett "licorice gargle"
#' @return datasett med norske nivåer og verdier
#' @export

plotMedFordeling <- function(data, fordelingsVariabel) {

  medKolonner <- c("PS_APO", "PS_DUO", "PS_DBS", "PS_LEC", "PS_PRO")

  data |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(medKolonner),
      names_to = "medicine",
      values_to = "used"
    ) |>
    dplyr::filter(.data$used == TRUE) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = !!rlang::sym(fordelingsVariabel),
        fill = .data$medicine
      )
    ) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = paste("Fordeling av medisinsk bruk etter", fordelingsVariabel),
      x = fordelingsVariabel,
      y = "Andel av total medisinsk bruk",
      fill = "Medisin"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(
        t = 15,
        r = 20,
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
