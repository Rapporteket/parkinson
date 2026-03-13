#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

mod_over_tid_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(

      shiny::sidebarPanel(
        width = 3,

        shiny::tagList(
          shiny::selectInput(
            inputId = ns("sorting"),
            label = "Velg sortering:",
            choices = c("Hele landet", "Sykehus", "Region"),
            selected = "Hele landet"
          ),
          shiny::uiOutput(ns("unit_ui"))
        )
      ),

      shiny::mainPanel(
        shiny::tabsetPanel(
          id = ns("tab"),
          shiny::tabPanel(
            "Figur",
            value = "Fig",
            shiny::h3("Antall registreringer over tid"),
            plotly::plotlyOutput(outputId = ns("over_tid_plot")),
            shiny::downloadButton(
              ns("nedlastning_over_tid_plot"),
              "Last ned figur"
            )
          )
        )
      )
    )
  )
}



#'@title Server fordeling
#'
#'@export

mod_over_tid_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      data_filtered <- shiny::reactive({
        shiny::req(input$sorting)

        if (input$sorting == "Hele landet") {
          data
        } else if (input$sorting == "Sykehus") {
          shiny::req(input$unit)
          data |>
            dplyr::filter(.data$HealthUnitName == input$unit)
        } else if (input$sorting == "Region") {
          shiny::req(input$unit)
          data |>
            dplyr::filter(.data$RHF == input$unit)
        }
      })

      output$unit_ui <- shiny::renderUI({
        shiny::req(input$sorting)

        if (input$sorting == "Hele landet") {
          return(NULL)
        }

        unitChoices <- switch(
          input$sorting,
          "Sykehus" = sort(unique(data$HealthUnitName)),
          "Region" = sort(unique(data$RHF))
        )

        shiny::selectInput(
          inputId = shiny::NS(id, "unit"),
          label = "Velg enhet:",
          choices = unitChoices,
          selected = unitChoices[1]
        )
      })

      plot_over_tid_reactive <- shiny::reactive({
        makeYearCountPlot(data_filtered())
      })

      output$over_tid_plot <- plotly::renderPlotly({
        plot_over_tid_reactive() |>
          plotly::ggplotly(tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })
      # Lag nedlastning
      output$nedlastning_over_tid_plot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_over_tid", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plot_over_tid_reactive())
          dev.off()
        }
      )
    }
  )
}
