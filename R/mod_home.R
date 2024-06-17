# Module UI

#' @title   mod_home_ui and mod_home_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_home
#'
#' @keywords internal
#' @export
#' @import bs4Dash
#' @importFrom shiny NS tagList a
mod_home_ui <- function(id) {
  ns <- NS(id) # nolint: object_usage_linter
  tagList(
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "GeoApp – Welcome",
          width = 12,
          collapsible = FALSE,
          "Click on the icons in the cards below to access the individual visualisation apps."
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Card(
          width = 12,
          title = "Human geography",
          headerBorder = FALSE,
          bs4Ribbon("Human", "info"),
          bs4InfoBox(
            title = "Demo explorer",
            width = 12,
            subtitle = "Explore demography indicators",
            icon = shiny::icon("map"),
            tabName = "demo_explorer"
          ),
          bs4InfoBox(
            title = "IDB Pyramid",
            width = 12,
            subtitle = "Population pyramids",
            icon = shiny::icon("users"),
            tabName = "idb_pyramid"
          ),
          bs4InfoBox(
            title = "HMD Demograph",
            width = 12,
            subtitle = "Demographs with data from mortality.org",
            icon = shiny::icon("chart-line"),
            tabName = "hmd_demograph"
          ),
          bs4InfoBox(
            title = "Swiss Votes",
            width = 12,
            subtitle = "Results of swiss national votes",
            icon = shiny::icon("envelope"),
            tabName = "swiss_votes"
          )
        )
      ),
      column(
        width = 6,
        bs4Card(
          width = 12,
          title = "Physical geography",
          headerBorder = FALSE,
          bs4Ribbon("Physical", "maroon"),
          bs4InfoBox(
            title = "NOAA Climate",
            width = 12,
            subtitle = "Climogram for select places",
            icon = shiny::icon("sun"),
            tabName = "noaa_climate"
          )
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_home
#' @export
#' @keywords internal

mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # nolint: object_usage_linter
  })
}
