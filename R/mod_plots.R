#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object

plots_ui <- function(id) {
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::uiOutput(shiny::NS(id, "select_x")),
      shiny::uiOutput(shiny::NS(id, "select_y"))
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::h3("Plot")
      )
    )
  )
}

plots_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {}
  )
}
