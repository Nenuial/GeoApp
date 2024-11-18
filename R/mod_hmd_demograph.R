# Module UI

#' @title   mod_hmd_demograph_ui and mod_hmd_demograph_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_hmd_demograph
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_hmd_demograph_ui <- function(id) {
  ns <- NS(id)
  codes <- geotools::gtl_hmd_codes() |>
    tibble::deframe()

  card(
    title = "HMD Demography",
    full_screen = TRUE,
    card_header("HMD Demography"),
    layout_sidebar(
      sidebar = sidebar(
        layout_column_wrap(
          width = "200px",
          fixed_width = FALSE,
          shiny::selectInput(
            inputId = ns("country"),
            label = "Country",
            choices = codes
          ),
          shiny::uiOutput(
            outputId = ns("year_ui")
          )
        )
      ),
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

#' @rdname mod_hmd_demograph
#' @export
#' @keywords internal

mod_hmd_demograph_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    full_data <- geodata::gdt_hmd_demography
    data <- reactiveVal()


    # Filter data -------------------------------------------------------------
    observe(priority = 100, {
      data(
        full_data |>
          dplyr::filter(Code == input$country)
      )
    })


    # Update year input -------------------------------------------------------
    observe(priority = 90, {
      req(data())

      output$year_ui <- shiny::renderUI({
        shiny::sliderInput(
          inputId = ns("year"),
          label = "Year",
          width = "100%",
          min = min(data()$Year),
          max = max(data()$Year),
          value = c(min(data()$Year), max(data()$Year)),
          step = 1
        )
      })
    })

    # Update plot -------------------------------------------------------------
    observe({
      req(data(), input$year)

      data() |>
        dplyr::filter(
          Year >= input$year[1],
          Year <= input$year[2]
        ) -> plot_data

      output$plot_area <- highcharter::renderHighchart({
        highcharter::highchart() |>
          highcharter::hc_title(text = glue::glue("Demograph for {get_country_name(input$country)}")) |>
          highcharter::hc_xAxis(title = list(text = "Year")) |>
          highcharter::hc_yAxis_multiples(
            list(min = 0, title = list(text = "Rates")),
            list(min = 0, title = list(text = "Population", style = list(color = "blue")), opposite = TRUE)
          ) |>
          highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) |>
          highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
          highcharter::hc_add_series(
            data = plot_data, "line", yAxis = 1,
            name = "population", color = "blue",
            highcharter::hcaes(x = Year, y = Population)
          ) |>
          highcharter::hc_add_series(
            data = plot_data, "line", yAxis = 0,
            name = "crude birth rate", color = "grey",
            tooltip = list(valueSuffix = " ‰"),
            highcharter::hcaes(x = Year, y = CBR)
          ) |>
          highcharter::hc_add_series(
            data = plot_data, "line", yAxis = 0,
            name = "crude death rate", color = "black",
            tooltip = list(valueSuffix = " ‰"),
            highcharter::hcaes(x = Year, y = CDR)
          ) |>
          ggeo::hc_dark_web_theme()
      })
    })


    # Get country name --------------------------------------------------------
    get_country_name <- function(search_code) {
      hmd_codes |>
        dplyr::filter(code == search_code) |>
        dplyr::pull(name)
    }
  })
}
