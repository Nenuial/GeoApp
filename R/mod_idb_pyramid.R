# Module UI

#' @title   mod_idb_pyramid_ui and mod_idb_pyramid_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_idb_pyramid
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_idb_pyramid_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Warning placeholder -----------------------------------------------------
    div(id = "idb_error", style = "position: absolute; top: 0; right: 0;"),

    # Plot parameters ---------------------------------------------------------
    bs4Card(
      title = "Plot Parameters",
      closable = FALSE,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          width = 4,
          shiny::selectInput(
            inputId = ns("country"),
            label = "Country",
            choices = geoapp_ui_country_code("fips")
          )
        ),
        column(
          width = 8,
          sliderInput(
            inputId = ns("year"),
            label = "Year",
            width = "100%",
            min = 1950,
            max = 2050,
            value = 2020
          )
        )
      )
    ),

    # Plot --------------------------------------------------------------------
    bs4Card(
      title = "Plot",
      maximizable = TRUE,
      closable = FALSE,
      status = "primary",
      width = 12,
      shinyjqui::jqui_resizable(
        highcharter::highchartOutput(
          outputId = ns("plot_area"),
          width = "100%",
          height = 500
        ),
        options = list(
          handles = "s",
          create = shinyjqui::JS(
            'function(event, ui){ $(this).css("width", "100%"); }'
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_idb_pyramid
#' @export
#' @keywords internal
#'
#' @importFrom magrittr %>%
mod_idb_pyramid_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # nolint: object_usage_linter

    # Update plot -------------------------------------------------------------
    observe({
      req(input$country, input$year)

      hc <- tryCatch(
        {
          geographer::gph_highcharter_pyramid(
            countrycode::countrycode(input$country, "fips", "country.name"),
            input$year
          )
        },
        error = function(cond) {
          bs4Dash::createAlert(
            id = "idb_error",
            options = list(
              title = "Error",
              closable = TRUE,
              width = 12,
              elevations = 4,
              status = c("danger"),
              content = "No data could be fetched for this country/year !"
            )
          )

          return(NULL)
        },
        warning = function(cond) {
          bs4Dash::createAlert(
            id = "idb_error",
            options = list(
              title = "Error",
              closable = TRUE,
              width = 12,
              elevations = 4,
              status = c("danger"),
              content = "No data could be fetched for this country/year !"
            )
          )
        }
      )

      req(hc)

      output$plot_area <- highcharter::renderHighchart(hc)
    })
  })
}
