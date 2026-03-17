makeMonthlyColPlot <- function(data, dateCol, fillCol, yearsBack = 1) {
  dataFiltered <- data |>
    dplyr::select(
      dplyr::all_of(c(dateCol, fillCol, "PasientGUID"))
    ) |>
    dplyr::filter(
      !is.na(.data[[dateCol]]),
      !is.na(.data[[fillCol]])
    ) |>
    dplyr::filter(
      .data[[dateCol]] >= lubridate::today() - lubridate::years(yearsBack)
    )

  antallRegistreringer <- nrow(dataFiltered)

  antallUnikePasienter <- dataFiltered |>
    dplyr::summarise(
      n = dplyr::n_distinct(.data$PasientGUID)
    ) |>
    dplyr::pull(.data$n)

  dataMonthly <- dataFiltered |>
    dplyr::mutate(
      month = lubridate::floor_date(.data[[dateCol]], unit = "month"),
      fillValue = as.character(.data[[fillCol]])
    ) |>
    dplyr::count(.data$month, .data$fillValue, name = "n")

  fillValues <- c(
    "#c6dbef",
    "#6baed6",
    "#4292c6",
    "#2171b5",
    "#084594",
    "#000059"
  )

  ggplot2::ggplot(
    data = dataMonthly,
    mapping = ggplot2::aes(
      x = .data$month,
      y = .data$n,
      fill = .data$fillValue,
      text = paste0(
        format(.data$month, "%b %y"),
        "<br>Antall registreringer: ", .data$n,
        "<br>", .data$fillValue
      )
    )
  ) +
    ggplot2::geom_col(width = 25) +
    ggplot2::scale_fill_manual(
      values = rep(fillValues, length.out = dplyr::n_distinct(dataMonthly$fillValue))
    ) +
    ggplot2::labs(
      fill = fillCol,
      title = paste0(
        "Antall registreringer: ", antallRegistreringer,
        " | Antall unike pasienter: ", antallUnikePasienter
      )
    ) +
    ggplot2::scale_x_date(
      labels = scales::label_date(format = "%b %y", locale = "nb"),
      date_breaks = "1 month",
      expand = ggplot2::expansion(add = 0)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      ),
      axis.line.y = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      ),
      plot.title = ggplot2::element_text(size = 11),
      plot.margin = ggplot2::margin(
        t = 15,
        r = 15,
        b = 15,
        l = 15
      )
    )
}
