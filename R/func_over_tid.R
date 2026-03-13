
#' Lager et søylediagram som viser antall registreringer per år
#' @param data RegData-datasettet"
#' @return ggplot-objekt
#' @export

makeYearCountPlot <- function(data) {
  ggplot2::ggplot(
    data |>
      dplyr::count(year = lubridate::year(.data$FormDate)),
    ggplot2::aes(
      x = factor(.data$year),
      y = .data$n,
      text = paste0("År: ", .data$year, "<br>Antall registreringer: ", .data$n)
    )
  ) +
    ggplot2::geom_col(width = 0.8, fill = "#4292c6") +
    ggplot2::labs(x = "År", y = "Antall registreringer") +
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
      )
    )
}
