

.private <- new.env(parent = emptyenv())
.private$andVarChoices <- c(
  "Tatt CT"        = "PS_DIAG_CT",
  "Tatt MR"        = "PS_DIAG_MR",
  "Tatt DAT"       = "PS_DIAG_DAT",
  "Tatt PET"       = "PS_DIAG_PET",
  "Tatt bilde"     =  "tattBilde",
  "Oppdatert behandling" = "oppdatertBehandling",
  "APO"            = "mottattAPOPasient",
  "DUO"            = "mottattDUOPasient",
  "DBS"            = "mottattDBSPasient",
  "Lec"            = "mottattLECPasient",
  "PRO"            = "mottattPROPasient",
  "Mottatt avansert behandling" = "mottattAvansertBehandlingPasient",
  "Standarsisert kartlegging" = "StandardisertKartlegging",
  "Fornøyd med tilbud fra spesialhelsetjenesten" = "tilfredsSpesialist"
)


.private$andBinChoices <- c(
  "Sykehus"    = "HealthUnitName",
  "Kjønn" = "PatientGender",
  "RHF" = "RHF"
)

.private$datoColmMap <- c(
  "PS_DIAG_CT"    = "PS_DIAG_CT_DATO",
  "PS_DIAG_MR"    = "PS_DIAG_MR_DATO",
  "PS_DIAG_DAT"   = "PS_DIAG_DAT_DATO",
  "PS_DIAG_PET"   = "PS_DIAG_PET_DATO"
)

#' Shiny module providing GUI and server logic for the Andeler tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

mod_andeler_ui <- function(id) {
  ns <- shiny::NS(id)



  shiny::tagList(

    "Andel basert på variabel og grenser",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          inputId = ns("varS"),
          label = "Variabel:",
          choices = .private$andVarChoices
        ),
        shiny::selectInput(
          inputId = ns("binsS"),
          label = "Sortert etter:",
          choices = .private$andBinChoices
        ),
        shiny::dateRangeInput(
          inputId = ns("datoRange"),
          label = "Velg datoperiode:",
          start = "2022-01-01",
          end = Sys.Date(),
          min = "1990-01-01",
          max = Sys.Date(),
          format = "yyyy-mm-dd",
          language = "no"
        ),
        shiny::selectInput(
          inputId = ns("bildeformatAndel"),
          label = "Velg format for nedlasting av figur",
          choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
        ),
        shiny::downloadButton(
          outputId = ns("downloadandelPlot"),
          label = "Last ned!"
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput(outputId = shiny::NS(id, "andelPlot"))
      )
    )
  )
}

#' Server logic for andel plot
#' @param id Character string module namespace
#' @param data Data frame with the data to be plotted
#' @return A Shiny app server object
#' @export

mod_andeler_server <- function(id, inputData) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      data_reactive <- shiny::reactive({
        inputData$RegData
      })
      e_prom_reactive <- shiny::reactive({
        inputData$promData
      })



      plotReactive <- shiny::reactive({
        shiny::req(input$datoRange, input$varS, input$binsS)
        var <- input$varS
        bins <- input$binsS
        if (var == "tilfredsSpesialist") {
          data <- as.data.frame(e_prom_reactive())
        } else {
          data <- as.data.frame(data_reactive())
        }

        data <- filtrerDatoIntervall(
          data = data,
          datoColNavn = "FormDate",
          datoFra = input$datoRange[1],
          datoTil = input$datoRange[2]
        )

        var_label  <- names(.private$andVarChoices)[.private$andVarChoices == input$varS]
        bins_label <- names(.private$andBinChoices)[.private$andBinChoices == input$binsS]

        tittel <- paste(
          "Andel", var_label,
          "etter", bins_label,
          "med mer enn", 10, "registreringer"
        )
        PlotAndelerGrVar(
          RegData = data,
          Variabel = data[[var]],
          grVar = bins,
          Ngrense = 10,
          tittel = tittel,
          kvalIndGrenser = attr(data, "kvalIndGrenser")[[var]],
          bestKvalInd = "høy"
        )
      })

      output$andelPlot <- shiny::renderPlot({
        plotReactive()
      })

      output$downloadandelPlot <- shiny::downloadHandler(
        filename = function() {
          format <- shiny::req(input$bildeformatAndel)
          paste0("plot_andeler_", Sys.Date(), ".", format)
        },
        content = function(file) {
          format <- shiny::req(input$bildeformatAndel)
          p <- shiny::req(plotReactive())

          ggplot2::ggsave(
            filename = file,
            plot = p,
            device = format,
            width = 15,
            height = 9,
            units = "in",
            dpi = 150
          )
        }
      )
    }
  )
}
