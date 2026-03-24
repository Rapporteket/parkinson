#' Shiny module providing GUI and server logic for the intro tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::mainPanel(
    width = 12,
    shiny::uiOutput(ns("generalInfo")),
    shiny::hr(),
    shiny::uiOutput(ns("info")),
    shiny::h3("Registreringer per måned siste år"),
    plotly::plotlyOutput(ns("infoPlot"))
  )
}

#' Server logic
#' @param id Character string module namespace
#' @param user Shiny user object containing information about the logged-in user
#' @param data Data frame with the data to be used in the Rmarkdown rendering and plot
#' @return A shiny app server object
#' @export

info_server <- function(id, user, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      promData <- data$promData
      RegData <- data$RegData

      # Info
      output$info <- shiny::renderText({
        nProm <- nrow(promData)
        nBakgrunn <- nrow(RegData |> dplyr::filter(FormTypeId == 1))
        nKonsultasjon <- nrow(RegData |> dplyr::filter(FormTypeId == 2))
        paste0(
          "<h3>Antall skjema</h3>",
          "<p>Oversikt over antall skjema som er registrert i systemet. ",
          "Denne informasjonen kan brukes til å holde oversikt over registreringsstatus ",
          "og fullstendighet i datainnsamlingen.</p>",
          "<ul>",
          "<li><b>Antall PROM-skjema:</b> ", nProm, "</li>",
          "<li><b>Antall bakgrunnsskjema:</b> ", nBakgrunn, "</li>",
          "<li><b>Antall konsultasjonsskjema:</b> ", nKonsultasjon, "</li>",
          "</ul>"
        )
      })

      output$generalInfo <- shiny::renderText({
        paste0(
          "<h3>Om Parkinsonregisteret</h3>",
          "<p>Parkinsonregisteret er et nasjonalt kvalitetsregister som samler inn data ",
          "om pasienter med Parkinsons sykdom og andre parkinsonistiske tilstander i Norge. ",
          "Registeret brukes til å:</p>",
          "<ul>",
          "<li>Overvåke og forbedre kvaliteten på diagnostikk og behandling av pasienter med parkinsonisme</li>",
          "<li>Kartlegge variasjon i utredning, behandling og oppfølging mellom sykehus og helseregioner</li>",
          "<li>Bidra til forskning og kunnskapsutvikling innen Parkinsons sykdom</li>",
          "<li>Gi grunnlag for pasientrapporterte utfallsmål (PROM) for å måle pasientenes egen opplevelse av sykdom og behandling</li>",
          "<li>Sikre likeverdig helsetilbud uavhengig av bosted</li>",
          "</ul>",
          "<p>Rapporteket gir tilgang til oppdaterte oversikter, figurer og tabeller basert på data fra registeret, ",
          "og er et viktig verktøy for kvalitetsforbedringsarbeid lokalt og nasjonalt.</p>"
        )
      })

      output$infoPlot <- plotly::renderPlotly({
        p <- makeMonthlyColPlot(
          data = RegData,
          dateCol = "FormDate",
          fillCol = "RHF",
          yearsBack = 1
        )
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::config(displayModeBar = FALSE) |>
          plotly::layout(
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            )
          )
      })
    }
  )
}
