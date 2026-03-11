#' Shiny module providing GUI and server logic for the intro tab
#'
#' @param id Character string module namespace
#' @return An shiny app ui object
#' @export

info_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::mainPanel(
    width = 12,
    shiny::h2("Brukerinformasjon"),
    shiny::uiOutput(ns("user_info"), inline = TRUE),
    shiny::hr(),
    shiny::h2("Rendering av Rmarkdown-fil"),
    shiny::uiOutput(ns("info"), inline = TRUE),
    plotly::plotlyOutput(ns("info_plot"))
  )
}

#' Server logic
#' @return A shiny app server object
#' @export

info_server <- function(id, user, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$user_info <- shiny::renderUI({
        shiny::tagList(
          shiny::p(
            "Informasjon om pålogget bruker fra variabelen ",
            shiny::code("user"),
            ", som defineres av shiny-modulen",
            shiny::code("rapbase::navbarWidgetServer2"),
            "."
          ),
          shiny::p(
            "Brukernavn (user$name(), hentet fra miljøvariabelen SHINYPROXY_USERNAME): ",
            shiny::strong(user$name()),
            shiny::br(),
            "Navn (user$fullName(), hentet fra miljøvariabelen FALK_USER_FULLNAME): ",
            shiny::strong(user$fullName()),
            shiny::br(),
            "Epost (user$email(), hentet fra miljøvariabelen FALK_USER_EMAIL): ",
            shiny::strong(user$email()),
            shiny::br(),
            "Telefonnummer (user$phone(), hentet fra miljøvariabelen FALK_USER_PHONE): ",
            shiny::strong(user$phone()),
            shiny::br(),
            "ReshID (user$org(), valgt i meny): ",
            shiny::strong(user$org()),
            shiny::br(),
            "Enhetsnavn (user$orgName(), hvis mapping mellom ReshID og sykehusnavn er ok): ",
            shiny::strong(user$orgName()),
            shiny::br(),
            "Rolle (user$role(), valgt i meny): ",
            shiny::strong(user$role())
          )
        )
      })

      # Info
      output$info <- shiny::renderUI({
        rapbase::renderRmd(
          system.file("info.Rmd", package = "parkinson"),
          outputType = "html_fragment",
          params = list(data = data)
        )
      })
      output$info_plot <- plotly::renderPlotly({
        dataMonthly <- data |>
          dplyr::select(.data$FormDate, .data$RHF) |>
          dplyr::filter(.data$FormDate >= lubridate::today() - lubridate::years(1)) |>
          dplyr::mutate(month = lubridate::floor_date(.data$FormDate, "month")) |>
          dplyr::count(.data$month, .data$RHF)

        p <- ggplot2::ggplot() +
          ggplot2::geom_col(
            data = dataMonthly,
            ggplot2::aes(x = .data$month, y = .data$n, fill = .data$RHF)
          ) +
          ggplot2::scale_fill_manual(
            values = c(
              "#c6dbef",
              "#6baed6",
              "#4292c6",
              "#2171b5",
              "#084594",
              "#000059"
            )
          ) +
          ggplot2::labs(fill = "RHF") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.background = ggplot2::element_blank()
          )

        plotly::ggplotly(p, tooltip = c("y", "fill")) |>
          plotly::config(displayModeBar = FALSE)
      })
    }
  )
}
