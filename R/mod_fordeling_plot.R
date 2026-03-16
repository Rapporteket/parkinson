#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

mod_fordeling_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::conditionalPanel(
          condition = "input.tab == 'Meds'",
          ns = ns,

          shiny::selectInput(
            inputId = ns("x_var"),
            label = "Variabel:",
            choices = c(
              "Sykehus" = "HealthUnitName",
              "Region"  = "RHF"
            ),
            selected = "HealthUnitName"
          ),

          shiny::sliderInput(
            inputId = ns("alder_var"),
            label = "Aldersintervall:",
            min = 0,
            max = 110,
            value = c(0, 110),
            dragRange = TRUE
          )
        ),
        shiny::conditionalPanel(
          condition = "input.tab == 'Age'",
          ns = ns,
          shiny::tagList(
            shiny::selectInput(
              inputId = ns("diagnose_var"),
              label = "Diagnose:",
              choices = c(
                "Parkinson" = FALSE,
                "Atypisk parkinsonisme" = TRUE
              )
            ),
            shiny::selectInput(
              inputId = ns("age_var"),
              label = "Visning:",
              choices = c(
                "Alder i live" = "PatientAge",
                "Alder ved d\u00f8d" = "deathAge"
              )
            )
          )
        )
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          id = ns("tab"),
          shiny::tabPanel(
            "Medisiner",
            value = "Meds",
            shiny::h3("Fordeling i bruk av medisiner og behandling"),
            plotly::plotlyOutput(
              outputId = ns("fordeling_plot_meds"),
              height = "500px"
            ),
            shiny::downloadButton(
              outputId = ns("nedlastning_fordeling_plot_meds"),
              label = "Last ned figur"
            )
          ),
          shiny::tabPanel(
            "Alder",
            value = "Age",
            shiny::h3("Aldersfordeling for pasienter"),
            plotly::plotlyOutput(
              outputId = ns("fordeling_plot_alder"),
              height = "800px"
            ),
            shiny::downloadButton(
              outputId = ns("nedlastning_fordeling_plot_alder"),
              label = "Last ned figur"
            )
          )
        )
      )
    )
  )
}

#' @title Server fordeling
#'
#' @export

mod_fordeling_plot_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data_reactive <- shiny::reactive({
        plotData <- data |>
          dplyr::distinct(.data$PasientGUID, .keep_all = TRUE)

        if (input$tab == "Meds") {
          plotData <- plotData |>
            dplyr::filter(.data$alive == TRUE) |>
            filtrerAlderIntervall(
              input$alder_var[1],
              input$alder_var[2]
            )
        } else if (input$tab == "Age") {
          shiny::req(input$diagnose_var, input$age_var)
          plotData <- plotData |>
            dplyr::filter(
              .data$atypiskDiag == input$diagnose_var,
              .data$alive == (input$age_var != "deathAge")
            )
        }

        plotData
      })

      plot_reactive_meds <- shiny::reactive({
        shiny::req(c(input$x_var))
        plotMedFordeling(
          data_reactive(),
          input$x_var
        )
      })

      plot_reactive_alder <- shiny::reactive({
        shiny::req(input$diagnose_var, input$age_var)
        makeAgeDistributionPlot(
          data_reactive(),
          input$age_var,
          "PatientGender"
        )
      })

      output$fordeling_plot_meds <- plotly::renderPlotly({
        plot_reactive_meds() |>
          plotly::ggplotly(tooltip = "text") |>
          plotly::config(displayModeBar = FALSE) |>
          plotly::layout(
            legend = list(
              itemclick = FALSE,
              itemdoubleclick = FALSE
            )
          )
      })

      output$fordeling_plot_alder <- plotly::renderPlotly({
        plot_reactive_alder() |>
          plotly::ggplotly(tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })

      # Lag nedlastning plot
      output$nedlastning_fordeling_plot_meds <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_fordeling_meds", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plot_reactive_meds())
          dev.off()
        }
      )
      output$nedlastning_fordeling_plot_alder <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_fordeling_alder", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plot_reactive_alder())
          dev.off()
        }
      )

    }
  )
}
