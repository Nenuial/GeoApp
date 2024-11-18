# Module UI

#' @title   mod_home_ui and mod_home_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_home
#'
#' @keywords internal
#' @export
#' @import bslib
#' @importFrom shiny NS tagList a
mod_home_ui <- function(id) {
  ns <- NS(id) # nolint: object_usage_linter
  demo_theme <- value_box_theme(
    name = "demo-explorer",
    bg = "#00496f",
    fg = "#ffffff"
  )
  idb_theme <- value_box_theme(
    name = "idb-pyramid",
    bg = "#086b8b",
    fg = "#ffffff"
  )
  hmd_theme <- value_box_theme(
    name = "hmd-demograph",
    bg = "#adbf5f",
    fg = "#232323"
  )
  swiss_votes_theme <- value_box_theme(
    name = "swiss-votes",
    bg = "#edc132",
    fg = "#232323"
  )
  noaa_theme <- value_box_theme(
    name = "noaa-climate",
    bg = "#dd4124",
    fg = "#232323"
  )

  layout_column_wrap(
    width = 1/2,
    fillable = FALSE,
    card(
      card_header("Human geography"),
      card_body(
        value_box(
          title = "",
          fill = FALSE,
          value = "Demo Explorer",
          showcase = bsicons::bs_icon("map-fill"),
          showcase_layout = "top right",
          theme = demo_theme,
          actionButton(inputId = ns("go_to_demo"), label = "Open")
        ),
        value_box(
          title = "",
          fill = FALSE,
          value = "IDB Pyramids",
          showcase = bsicons::bs_icon("boxes"),
          showcase_layout = "top right",
          theme = idb_theme,
          actionButton(inputId = ns("go_to_idb"), label = "Open")
        ),
        value_box(
          title = "",
          fill = FALSE,
          value = "HMD Demograph",
          showcase = bsicons::bs_icon("graph-up"),
          showcase_layout = "top right",
          theme = hmd_theme,
          actionButton(inputId = ns("go_to_hmd"), label = "Open")
        ),
        value_box(
          title = "",
          fill = FALSE,
          value = "Swiss Votes",
          showcase = bsicons::bs_icon("archive-fill"),
          showcase_layout = "top right",
          theme = swiss_votes_theme,
          actionButton(inputId = ns("go_to_swiss_votes"), label = "Open")
        )
      )
    ),
    card(
      card_header("Physical geography"),
      card_body(
        value_box(
          title = "",
          fill = FALSE,
          value = "NOAA Climate",
          showcase = bsicons::bs_icon("sun"),
          showcase_layout = "top right",
          theme = noaa_theme,
          actionButton(inputId = ns("go_to_noaa"), label = "Open")
        )
      )
    )
  )

  # tagList(
  #   fluidRow(
  #     column(
  #       width = 12,
  #       bs4Card(
  #         title = "GeoApp – Welcome",
  #         width = 12,
  #         collapsible = FALSE,
  #         "Click on the icons in the cards below to access the individual visualisation apps."
  #       )
  #     )
  #   ),
  #   fluidRow(
  #     column(
  #       width = 6,
  #       bs4Card(
  #         width = 12,
  #         title = "Human geography",
  #         headerBorder = FALSE,
  #         bs4Ribbon("Human", "info"),
  #         bs4InfoBox(
  #           title = "Demo explorer",
  #           width = 12,
  #           subtitle = "Explore demography indicators",
  #           icon = shiny::icon("map"),
  #           tabName = "demo_explorer"
  #         ),
  #         bs4InfoBox(
  #           title = "IDB Pyramid",
  #           width = 12,
  #           subtitle = "Population pyramids",
  #           icon = shiny::icon("users"),
  #           tabName = "idb_pyramid"
  #         ),
  #         bs4InfoBox(
  #           title = "HMD Demograph",
  #           width = 12,
  #           subtitle = "Demographs with data from mortality.org",
  #           icon = shiny::icon("chart-line"),
  #           tabName = "hmd_demograph"
  #         ),
  #         bs4InfoBox(
  #           title = "Swiss Votes",
  #           width = 12,
  #           subtitle = "Results of swiss national votes",
  #           icon = shiny::icon("envelope"),
  #           tabName = "swiss_votes"
  #         )
  #       )
  #     ),
  #     column(
  #       width = 6,
  #       bs4Card(
  #         width = 12,
  #         title = "Physical geography",
  #         headerBorder = FALSE,
  #         bs4Ribbon("Physical", "maroon"),
  #         bs4InfoBox(
  #           title = "NOAA Climate",
  #           width = 12,
  #           subtitle = "Climogram for select places",
  #           icon = shiny::icon("sun"),
  #           tabName = "noaa_climate"
  #         )
  #       )
  #     )
  #   )
  # )
}

# Module Server

#' @rdname mod_home
#' @export
#' @keywords internal

mod_home_server <- function(id, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # nolint: object_usage_linter

    observeEvent(input$go_to_demo, {
      nav_select("tabs", selected = "Demo Explorer", session = parent)
    })

    observeEvent(input$go_to_idb, {
      nav_select("tabs", selected = "IDB Pyramids", session = parent)
    })

    observeEvent(input$go_to_hmd, {
      nav_select("tabs", selected = "HMD Demography", session = parent)
    })

    observeEvent(input$go_to_swiss_votes, {
      nav_select("tabs", selected = "Swiss Votes", session = parent)
    })

    observeEvent(input$go_to_noaa, {
      nav_select("tabs", selected = "NOAA Climate", session = parent)
    })


  })
}
