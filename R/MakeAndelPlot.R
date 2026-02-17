

#' @title PlotAndelerGrVar
#' @description This function generates a plot for group-specific proportions or distributions
#'              based on the provided registry data.
#' @param RegData A data frame containing the registry data to be used for plotting.
#' @return A plot object visualizing the group-specific proportions or distributions.
#' @details This function is designed to work with registry data and create visualizations
#'          that highlight group-specific metrics. The exact details of the plot depend on
#'          the structure of the input data and the specific implementation of the function.
#' @export
PlotAndelerGrVar <- function(RegData,
  Variabel,
  grVar = "ShNavn",
  hovedgrTxt = "Total andel",
  kvalIndGrenser = NA,
  tittel = "tittel",
  utvalgTxt = "",
  Ngrense = 10,
  bestKvalInd = "lav", # "høy" for omvendt rekkfølge på indikatorfarger
  fargepalett = "BlaaOff",
  grtxt = "",
  titleSize = 20,
  subtitleSize = 15,
  legendSize = 12,
  axisTextSize = 12,
  nTicks = 5
) {
  offAlleFarger <- c("#c6dbef", "#6baed6", "#4292c6", "#2171b5", "#084594", "#000059",
                     "#FF7260", "#4D4D4D", "#737373", "#A6A6A6", "#DADADA")
  farger <- switch(
    fargepalett,
    BlaaOff = offAlleFarger[rev(c(1, 2, 4, 5))],
    BlaaOffAlle = offAlleFarger[6:1],
    StotteOff = offAlleFarger[7:11],
    offAlleFarger = offAlleFarger
  )

  dummy0 <- NA  # -0.001

  N <- nrow(RegData)
  Variabel <- as.numeric(Variabel)
  grVar <- as.character(grVar)[1]

  # Gruppestørrelser og summer per gruppe
  if (N > 0) {
    Ngr  <- table(RegData[, grVar])
    Nvar <- tapply(Variabel, RegData[, grVar], sum, na.rm = TRUE)
  } else {
    Ngr  <- table(factor(character(0)))
    Nvar <- numeric(0)
  }
  # Andeler per gruppe (i %), og hvilke grupper som er under grense
  AndelerGr <- round(100 * Nvar / Ngr, 2)

  indGrUt <- which(Ngr < Ngrense)
  AntGr   <- sum(Ngr >= Ngrense)

  # Sett under-grense til dummy0 (NA)
  if (length(indGrUt) > 0) {
    AndelerGr[indGrUt] <- dummy0
  }

  # Sorter synkende ( NA havner sist)
  sortInd <- order(AndelerGr, decreasing = TRUE, na.last = TRUE)

  # Tekst for N per gruppe (med <Ngrense for små grupper)
  Ngrtxt <- as.character(Ngr)
  if (length(indGrUt) > 0) {
    Ngrtxt[indGrUt] <- paste0("<", Ngrense)
  }

  # Aggregerte verdier
  AggVerdier <- list(Hoved = NULL, Tot = NULL)
  AggVerdier$Hoved <- AndelerGr[sortInd]
  AggVerdier$Tot   <- if (N > 0) round(100 * sum(Variabel, na.rm = TRUE) / N, 2) else NA_real_
  # Sorterte gruppenavn med N-tekst
  GrNavnSort <- paste0(names(Ngr)[sortInd], " (", Ngrtxt[sortInd], ")")

  # Andeltekst (blank for under-grense)
  andeltxt <- paste0(sprintf("%.1f", AggVerdier$Hoved), "%")
  if (length(indGrUt) > 0) {
    andeltxt[(AntGr + 1):(AntGr + length(indGrUt))] <- ""
  }


  if (all(is.na(Ngr))) {

    tekst <- "Ingen registrerte data for dette utvalget"

    p <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = tittel) +
      ggplot2::annotate("text", x = 0, y = 0, label = tekst, size = 5) +
      ggplot2::annotate("text", x = 0, y = -0.2, label = paste(utvalgTxt, collapse = "\n"),
                        size = 3.5, color = farger[1])


  } else if (max(Ngr, na.rm = TRUE) < Ngrense) {
    tekst <- paste0("Færre enn ", Ngrense, " registreringer i alle grupper for dette utvalget")

    p <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::labs(title = tittel) +
      ggplot2::annotate("text", x = 0, y = 0, label = tekst, size = 5) +
      ggplot2::annotate("text", x = 0, y = -0.2, label = paste(utvalgTxt, collapse = "\n"),
                        size = 3.5)


  } else {
    # 1) AndelerPlot (NA → 0 kun for plotting, ggplot fjerner NA verdier)
    andeler <- as.numeric(AggVerdier$Hoved)
    andelerPlot <- replace(andeler, is.na(andeler), 0)

    # 2) Datasett til ggplot
    ggDataFrame <- data.frame(
      andelProsent = andeler,      # ekte verdi (kan være NA)
      andelerPlot  = andelerPlot,  # brukt til stolpehøyde
      gruppeNavn   = as.character(GrNavnSort),
      andelTekst   = as.character(andeltxt)
    )

    ggDataFrame <- ggDataFrame[order(-ifelse(is.na(ggDataFrame$andelProsent), -Inf, ggDataFrame$andelProsent)), ]


    # ---- Lås rekkefølge ----
    ggDataFrame$gruppeNavn <- factor(
      ggDataFrame$gruppeNavn,
      levels = ggDataFrame$gruppeNavn
    )
    nLevels <- length(levels(ggDataFrame$gruppeNavn))

    # 3) Gjennomsnittslinje
    gjennomsnittY <- AggVerdier$Tot[1]

    gjennomsnittEtikett <- paste0(
      hovedgrTxt[1], " (",
      sprintf("%.1f", gjennomsnittY), "%), N=", N
    )

    # 4) Dynamisk øvre grense på prosentaksen
    maksAndel <- min(max(ggDataFrame$andelProsent, na.rm = TRUE) * 1.15, 100)
    prettyVals <- pretty(c(0, maksAndel), n = nTicks) # Funksjon som finner "pent" fordelte verdier for aksen
    # Sørg for at både maks andel og gjennomsnittslinje får plass
    ovreGrense <- max(prettyVals, gjennomsnittY, na.rm = TRUE)

    # 5) Kvalitetsindikator: Bakgrunnsbånd basert på kvalitetsgrenser
    visKvalIndGrenser <- any(kvalIndGrenser > 0, na.rm = TRUE)

    kvalIndLegend <- switch(
      bestKvalInd,
      "høy" = c("Lav", "Middels", "Høy"),
      "lav" = c("Høy", "Middels", "Lav")
    )
    kvalIndFarger <- switch(bestKvalInd,
      "lav" = c("#3baa34", "#fd9c00", "#e30713"), # Grønn, gul, rød
      "høy" = c("#e30713", "#fd9c00", "#3baa34") # Rød, gul, grønn
    )
    names(kvalIndFarger) <- kvalIndLegend

    if (visKvalIndGrenser) {

      # Sikre at grensene er numeriske, sorterte og gyldige
      kvalBreaks <- sort(as.numeric(kvalIndGrenser))
      stopifnot(length(kvalBreaks) == 4, all(diff(kvalBreaks) > 0))

      # Lag kvalitetsindikator-bånd på prosentaksen (bånd etter verdi-aksen; blir "vertikale" etter coord_flip)
      indikatorBand <- data.frame(
        ymin = kvalBreaks[-length(kvalBreaks)],
        ymax = kvalBreaks[-1],
        indLevels = factor(kvalIndLegend, levels = kvalIndLegend)
      )

      # Hvis aksen ikke går til 100, klipp båndene til ovreGrense
      indikatorBand$ymin <- pmax(indikatorBand$ymin, 0)
      indikatorBand$ymax <- pmin(indikatorBand$ymax, ovreGrense)

      # fjern kvalitetsindikator-bånd for (N)-label
      indikatorBand$xmin <- 0.5
      indikatorBand$xmax <- nLevels + 0.5

      # Fjern bånd som ender opp tomme
      indikatorBand <- indikatorBand[indikatorBand$ymax > indikatorBand$ymin, ]
    }

    # 6) Plot
    p <- ggplot2::ggplot(ggDataFrame, ggplot2::aes(x = .data$gruppeNavn, y = andelerPlot))

    # Legg til bakgrunnsbånd først slik at de ligger bak stolpene
    if (visKvalIndGrenser) {
      p <- p +
        ggplot2::geom_rect(
          data = indikatorBand,
          ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                       ymin = .data$ymin, ymax = .data$ymax, fill = .data$indLevels),
          inherit.aes = FALSE,
          alpha = 0.20
        ) +
        ggplot2::scale_fill_manual(
          name = "Kvalitetsnivå",
          values = kvalIndFarger,
          drop = FALSE
        )
    }

    # Stolper, linjer og tekst
    p <- p +
      ggplot2::geom_col(fill = farger[3], width = 0.65) +

      ggplot2::geom_segment(
        data = data.frame(
          x = 0.5,
          xend = nLevels + 0.5,
          y = gjennomsnittY,
          yend = gjennomsnittY,
          lab = gjennomsnittEtikett
        ),
        ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend, linetype = .data$lab),
        color = farger[2],
        linewidth = 1,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_linetype_manual(
        values = setNames("solid", gjennomsnittEtikett),
        name = NULL
      ) +

      # Verdietiketter til høyre for stolpene
      ggplot2::geom_text(
        ggplot2::aes(label = .data$andelTekst),
        color = farger[1],
        hjust = -0.2
      ) +

      # Plot stolper horisontalt
      ggplot2::coord_flip(clip = "off") +

      # Prosentakse
      ggplot2::scale_y_continuous(
        breaks = prettyVals,
        limits = c(0, ovreGrense),
        expand = ggplot2::expansion(mult = c(0, 0.0))
      ) +

      # Tittel og undertittel
      ggplot2::labs(
        title = tittel,
        subtitle = paste(utvalgTxt, collapse = "\n"),
        x = NULL,
        y = "Andel (%)"
      ) +

      # Layout
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        plot.margin = ggplot2::margin(r = 30),
        axis.ticks.x = ggplot2::element_line(color = "black"),
        axis.line.x  = ggplot2::element_line(color = "black"),
        axis.line.y  = ggplot2::element_line(color = "black"),
        legend.position = "top",
        legend.justification = "center",
        legend.text = ggplot2::element_text(size = legendSize),
        plot.subtitle = ggplot2::element_text(size = subtitleSize, color = farger[1]),
        plot.title = ggplot2::element_text(size = titleSize),
        axis.text.y = ggplot2::element_text(size = axisTextSize)
      )
  }
  return(p)

}
