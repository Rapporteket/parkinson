#' Shiny module providing GUI and server logic for the Andeler tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

.private <- new.env(parent = emptyenv())
.private$andVarChoices <- c(
  "Tatt CT"        = "PS_DIAG_CT",
  "Tatt MR"        = "PS_DIAG_MR",
  "Tatt bilde" =  "PS_DIAG_IM",
  "Tatt PET"       = "PS_DIAG_PET",
  "Klinkompleks"   = "PS_KLINKOMPEKSPL",
  "Klinkomp"       = "PS_KLINKOMP",
  "Skade hode"     = "PS_FALL_SKADE_HODE",
  "Skade trunkus"  = "PS_FALL_SKADE_TRUNKUS",
  "Skade ekstremitet" = "PS_FALL_SKADE_EKS",
  "Ikke motekspl"  = "PS_IKKEMOTEKSPL",
  "Ikke motekspl ingen" = "PS_IKKEMOTEKSPLIngen",
  "Autekspl"       = "PS_AUTEKSPL",
  "Autdysf"        = "PS_AUTDYSF",
  "Sovnekspl"      = "PS_SOVNEKSPL",
  "Sovn"           = "PS_SOVN",
  "Psykekspl"      = "PS_PSYKEKSPL",
  "Psyk"           = "PS_PSYK",
  "Systunders"     = "PS_SYSTUNDERS",
  "Blodbvit"       = "PS_BLODBVIT",
  "CSF amyl"       = "PS_CSFAMYL",
  "APO"            = "PS_APO",
  "DUO"            = "PS_DUO",
  "DBS"            = "PS_DBS",
  "Antidep"        = "PS_ANTIDEP",
  "Antianx"        = "PS_ANTIANX",
  "Antipsy"        = "PS_ANTIPSY",
  "Sovemed"        = "PS_SOVEMED",
  "Analg"          = "PS_ANALG",
  "Antidem"        = "PS_ANTIDEM",
  "B12 folat"      = "PS_B12FOL",
  "Lec"            = "PS_LEC"
  )
              

.private$andBinChoices <- c(
  "Sykehus"    = "HealthUnitShortName",
  "Kjønn" = "PatientGender",
  "RHF" = "RHF"
)


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
        shiny::sliderInput(
          inputId = ns("limitS"),
          label = "Inklusjonskriterie (>n):",
          min = 0,
          max = 100,
          value = 1
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
#' @return A Shiny app server object
#' @export

mod_andeler_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      data_reactive <- shiny::reactive({
        data
      })

      plotReactive <- shiny::reactive({
        data <- as.data.frame(data_reactive())
        var <- input$varS
        bins <- input$binsS
        limit <- input$limitS

        var_label  <- names(.private$andVarChoices)[.private$andVarChoices == input$varS]
        bins_label <- names(.private$andBinChoices)[.private$andBinChoices == input$binsS]

        tittel <- paste(
          "Andel", var_label,
          "etter", bins_label,
          "med mer enn", input$limitS, "registreringer"
        )

        PlotAndelerGrVar(
          RegData = data,
          Variabel = data[[var]],
          grVar = bins,
          Ngrense = limit,
          tittel = tittel,
          kvalIndGrenser = attr(data, "kvalIndGrenser")[[var]],
          bestKvalInd = "høy"
        )
      })

      output$andelPlot <- shiny::renderPlot({
        plotReactive()
      })

      output$downloadandelPlot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_andeler", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plotReactive())
          dev.off()
        }
      )


    }
  )
}
