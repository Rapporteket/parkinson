# Funksjoner til modulen "mod_fordeling"

# Standard filtreringsfunksjon



#' Preprosessering
#' @param data datasett "licorice gargle"
#' @return datasett med norske nivåer og verdier
#' @export

plotMedFordeling <- function(data, fordelingsVariabel) {

  medKolonner <- c("PS_APO", "PS_DUO", "PS_DBS", "PS_LEC")

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
