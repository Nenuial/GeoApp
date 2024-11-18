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
#' @import bslib
#' @importFrom shiny NS tagList
mod_idb_pyramid_ui <- function(id) {
  ns <- NS(id)

  card(
    title = "IDB Pyramids",
    full_screen = TRUE,
    card_header("IDB Pyramids"),
    layout_sidebar(
      sidebar = sidebar(
        # Sidebar --------------------------------------------------------------
        layout_column_wrap(
          width = "200px",
          fixed_width = FALSE,
          shiny::selectInput(
            inputId = ns("country"),
            label = "Country",
            choices = geoapp_ui_country_code("fips")
          ),
          sliderInput(
            inputId = ns("year"),
            label = "Year",
            width = "100%",
            min = 1950,
            max = 2050,
            value = 2020
          )
        )
      ),
      # Plot --------------------------------------------------------------------
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
          ) |>
            ggeo::hc_dark_web_theme()
        },
        error = function(cond) {
          # TODO: Handle error
        },
        warning = function(cond) {
          # TODO: Handle warning
        }
      )

      req(hc)

      output$plot_area <- highcharter::renderHighchart(hc)
    })
  })
}
