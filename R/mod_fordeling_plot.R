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
        width = 4,

        shiny::selectInput(
          inputId = ns("x_var"),
          label = "Variabel:",
          choices = c(
            "Sykehus" = "HealthUnitShortName",
            "Region"  = "RHF"
          ),
          selected = "HealthUnitShortName"
        ),

        shiny::sliderInput(
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(10, 100),
          dragRange = TRUE
        )
      ),

      shiny::mainPanel(
        shiny::plotOutput(outputId = ns("fordeling_plot")),
        shiny::downloadButton(
          outputId = ns("nedlastning_fordeling_plot"),
          label = "Last ned figur"
        )
      )
    )
  )
}

#'@title Server fordeling
#'
#'@export

mod_fordeling_plot_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data_reactive <- shiny::reactive({
        data <- filtrerAlderIntervall(
          data,
          input$alder_var[1],
          input$alder_var[2]
        )
      })



      plot_reactive <- shiny::reactive({
        shiny::req(c(input$x_var))
        plotMedFordeling(
          data_reactive(),
          input$x_var
        )
      })

      output$fordeling_plot <- shiny::renderPlot({
        plot_reactive()
      })

      # Lag nedlastning plot
      output$nedlastning_fordeling_plot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_fordeling", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(plot_reactive())
          dev.off()
        }
      )

    }
  )
}
