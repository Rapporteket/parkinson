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
          shiny::uiOutput(ns("unit_ui")),
          shiny::selectizeInput(
            inputId = ns("group_choice"),
            label = "Velg filtre:",
            choices = c("Kjønn", "Alder"),
            selected = NULL,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          shiny::uiOutput(ns("age_ui")),
          shiny::uiOutput(ns("gender_ui")),
          shiny::uiOutput(ns("tabell_ui"))
        )
      ),
      shiny::mainPanel(
        shiny::h3("Utvikling over tid"),
        shiny::tabsetPanel(
          id = ns("tab"),
          shiny::tabPanel(
            "Figur",
            value = "Fig",
            plotly::plotlyOutput(outputId = ns("over_tid_plot")),
            shiny::downloadButton(
              ns("nedlastning_over_tid_plot"),
              "Last ned figur"
            )
          ),
          shiny::tabPanel(
            "Tabell",
            value = "Tab",
            DT::DTOutput(outputId = ns("tabell_over_tid"))
          )
        )
      )
    )
  )
}


#' @title Server over tid
#' @param id Character string module namespace
#' @param data Data frame with the data to be used in the plot
#'
#' @export

mod_over_tid_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data_filtered <- shiny::reactive({
        shiny::req(input$sorting)

        # First filter by sorting
        filtered <- if (input$sorting == "Hele landet") {
          data
        } else if (input$sorting == "Sykehus") {
          shiny::req(input$unit)
          data |>
            dplyr::filter(.data$HF == input$unit)
        } else if (input$sorting == "Region") {
          shiny::req(input$unit)
          data |>
            dplyr::filter(.data$RHF == input$unit)
        } else {
          data
        }

        # Then filter by group choices (multiple selections possible)
        if ("Kjønn" %in% input$group_choice) {
          shiny::req(input$gender)
          filtered <- filtered |>
            dplyr::filter(.data$PatientGender == input$gender)
        }

        if ("Alder" %in% input$group_choice) {
          shiny::req(input$age)
          filtered <- filtered |>
            dplyr::mutate(
              age_group = dplyr::case_when(
                .data$updatedPatientAge <= 50 ~ "under_50",
                .data$updatedPatientAge >= 51 & .data$updatedPatientAge <= 60 ~ "51_60",
                .data$updatedPatientAge >= 61 & .data$updatedPatientAge <= 70 ~ "61_70",
                .data$updatedPatientAge >= 71 & .data$updatedPatientAge <= 80 ~ "71_80",
                .data$updatedPatientAge >= 81 & .data$updatedPatientAge <= 90 ~ "81_90",
                .data$updatedPatientAge >= 91 ~ "over_90",
                TRUE ~ NA_character_
              )
            ) |>
            dplyr::filter(.data$age_group == input$age)
        }

        filtered
      })

      output$unit_ui <- shiny::renderUI({
        shiny::req(input$sorting)
        if (input$sorting == "Hele landet") {
          return(NULL)
        }
        unitChoices <- switch(input$sorting,
          "Sykehus" = sort(unique(data$HF)),
          "Region" = sort(unique(data$RHF))
        )

        shiny::selectInput(
          inputId = shiny::NS(id, "unit"),
          label = "Velg enhet:",
          choices = unitChoices,
          selected = unitChoices[1]
        )
      })

      output$gender_ui <- shiny::renderUI({
        if (!("Kjønn" %in% input$group_choice)) {
          return(NULL)
        }
        genderChoices <- unique(data$PatientGender)

        shiny::selectInput(
          inputId = shiny::NS(id, "gender"),
          label = "Velg kjønn:",
          choices = genderChoices,
          selected = genderChoices[1]
        )
      })

      output$age_ui <- shiny::renderUI({
        if (!("Alder" %in% input$group_choice)) {
          return(NULL)
        }
        ageChoices <- c(
          "50 år og yngre" = "under_50",
          "51-60 år" = "51_60",
          "61-70 år" = "61_70",
          "71-80 år" = "71_80",
          "81-90 år" = "81_90",
          "91 år og eldre" = "over_90"
        )

        shiny::selectInput(
          inputId = shiny::NS(id, "age"),
          label = "Velg aldersgruppe:",
          choices = ageChoices,
          selected = ageChoices[1]
        )
      })

      output$tabell_ui <- shiny::renderUI({
        shiny::selectInput(
          inputId = shiny::NS(id, "table_select"),
          label = "Velg informasjon:",
          choices = c(
            "Nye registreringer per år" = "registreringer",
            "Antall pasienter i live ved slutten av året" = "pasienter_i_live",
            "Antall dødsfall blant pasienter per år" = "antallDodsfall",
            "Gjennomsnittsalder for nye pasienter per år" = "alder_nye_pasienter",
            "Gjennomsnittsalder for pasienter i live ved 
            slutten av året" = "alder_pasienter_i_live"
          ),
          selected = "registreringer"
        )
      })
      plot_over_tid_reactive <- shiny::reactive({
        shiny::req(input$table_select, data_filtered())
        makeYearCountStackedPlot(data_filtered(), varChoice = input$table_select)
      })

      output$tabell_over_tid <- DT::renderDT({
        shiny::req(input$table_select, data_filtered())
        makeYearCountTable(data_filtered(), varChoice = input$table_select)
      })

      output$over_tid_plot <- plotly::renderPlotly({
        plot_over_tid_reactive() |>
          plotly::ggplotly(tooltip = "text") |>
          plotly::config(displayModeBar = FALSE)
      })

      # Lag nedlastning
      output$nedlastning_over_tid_plot <- shiny::downloadHandler(
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
