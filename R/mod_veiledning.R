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
    plotly::plotlyOutput(ns("infoPlot"))
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
      output$infoPlot <- plotly::renderPlotly({
        p <- makeMonthlyColPlot(
          data = data,
          dateCol = "FormDate",
          fillCol = "RHF",
          yearsBack = 1
        )
        plotly::ggplotly(p, tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })
    }
  )
}

makeMonthlyColPlot <- function(data, dateCol, fillCol, yearsBack = 1) {
  dataMonthly <- data |>
    dplyr::select(
      dplyr::all_of(c(dateCol, fillCol))
    ) |>
    dplyr::filter(
      !is.na(.data[[dateCol]]),
      !is.na(.data[[fillCol]])
    ) |>
    dplyr::filter(
      .data[[dateCol]] >= lubridate::today() - lubridate::years(yearsBack)
    ) |>
    dplyr::mutate(
      month = lubridate::floor_date(.data[[dateCol]], unit = "month"),
      fillValue = as.character(.data[[fillCol]])
    ) |>
    dplyr::count(.data$month, .data$fillValue, name = "n")

  fillValues <- c(
    "#c6dbef",
    "#6baed6",
    "#4292c6",
    "#2171b5",
    "#084594",
    "#000059"
  )

  ggplot2::ggplot(
    data = dataMonthly,
    mapping = ggplot2::aes(
      x = .data$month,
      y = .data$n,
      fill = .data$fillValue,
      text = paste0(
        format(.data$month, "%b %y"),
        "<br>Antall: ", .data$n,
        "<br>", fillCol, ": ", .data$fillValue
      )
    )
  ) +
    ggplot2::geom_col(width = 25) +
    ggplot2::scale_fill_manual(
      values = rep(fillValues, length.out = dplyr::n_distinct(dataMonthly$fillValue))
    ) +
    ggplot2::labs(fill = fillCol) +
    ggplot2::scale_x_date(
      labels = scales::label_date(format = "%b %y", locale = "nb"),
      date_breaks = "1 month",
      expand = ggplot2::expansion(add = 0)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      ),
      axis.line.y = ggplot2::element_line(
        colour = scales::alpha("black", 0.5)
      )
    )
}