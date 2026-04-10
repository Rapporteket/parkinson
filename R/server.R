#' Server logic for the parkinson app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {
  # data.frame som mapper ReshID og sykehusnavn
  map_orgname <- data.frame(
    UnitId = c(111, 222, 333),
    orgname = c("Sykehus 1", "Sykehus 2", "Sykehus 3")
  )
  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "pilot",
    map_orgname = map_orgname,
    caller = "parkinson"
  )

  data <- parkGetRegData()
  RegData <- data$RegData
  info_server("info", user = user, data = data)
  samlerapport_server("samlerapport")
  pivot_server(RegData, "pivot", user = user)
  mod_fordeling_plot_server("fordeling", data = RegData)
  mod_over_tid_server("over_tid", data = RegData)
  mod_andeler_server("andeler", inputData = data)

  #################
  # Subscriptions #
  #################

  subParamNames <- shiny::reactive(c("reshID"))
  subParamValues <- shiny::reactive(user$org())

  rapbase::autoReportServer(
    id = "subscription",
    registryName = "parkinson",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = list(
      `Automatisk samlerapport1` = list(
        synopsis = "
        Automatisk samlerapport1 som inneholder spennende statistikk
        om pasienter og operasjoner",
        fun = "samlerapport1Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("Alder", 1, 999999)
      ),
      `Automatisk samlerapport2` = list(
        synopsis = "Automatisk samlerapport2 som inneholder
        enda mer spennende statistikk
        om pasienter og operasjoner, i tillegg til BMI",
        fun = "samlerapport2Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("BMI", 1, 999999)
      )
    ),
    freq = "quarter",
    user = user
  )

  #################
  # Dispatchments #
  #################

  shiny::observeEvent(
    shiny::req(user$role()),
    {
      if (user$role() != "SC") {
        message("Removing dispatchment tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Utsending")
        message("Removing export tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Eksport")
      } else {
        message("Adding dispatchment tab for user with role ", user$role())
        shiny::insertTab(
          "tabs",
          shiny::tabPanel(
            "Utsending",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::autoReportFormatInput("dispatchment"),
                rapbase::autoReportOrgInput("dispatchment"),
                rapbase::autoReportInput("dispatchment")
              ),
              shiny::mainPanel(
                rapbase::autoReportUI("dispatchment")
              )
            )
          ),
          target = "Pivot-tabell",
          position = "after"
        )
        message("Adding export tab for user with role ", user$role())
        shiny::appendTab(
          "tabs",
          shiny::tabPanel(
            "Eksport",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::exportUCInput("export")
              ),
              shiny::mainPanel(
                rapbase::exportGuideUI("exportGuide")
              )
            )
          )
        )
      }
    }
  )

  orgs <- list(
    OrgOne = 100082,
    OrgTwo = 102966
  )

  org <- rapbase::autoReportOrgServer("dispatchment", orgs)

  disParamNames <- shiny::reactive(c("reshID"))
  disParamValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "dispatchment",
    registryName = "parkinson",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = list(
      `Automatisk samlerapport1` = list(
        synopsis = "
        Automatisk samlerapport1 som inneholder spennende statistikk
        om pasienter og operasjoner",
        fun = "samlerapport1Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("Alder", 1, 999999)
      ),
      `Automatisk samlerapport2` = list(
        synopsis = "Automatisk samlerapport2 som inneholder
        enda mer spennende statistikk
        om pasienter og operasjoner, i tillegg til BMI",
        fun = "samlerapport2Fun",
        paramNames = c("p1", "p2", "reshID"),
        paramValues = c("BMI", 1, 999999)
      )
    ),
    orgs = orgs,
    user = user
  )

  ###############
  # Export data #
  ###############

  ## brukerkontroller
  rapbase::exportUCServer(
    "export",
    dbName = "data",
    teamName = Sys.getenv("SHINYPROXY_APPID", unset = "unknown")
  )
  ## veiledning
  rapbase::exportGuideServer("exportGuide", dbName = "data")
}
