#' The application theme
#'
#' @export
app_theme <- function() {
  bslib::bs_theme(
    base_font = "Fira Sans",
    code_font = "Fira Code",
    heading_font = "Fira Sans",
    bootswatch = "darkly"
  )
}

#' @import shinyWidgets
#' @import bslib
#' @importFrom shiny tagList
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    page_navbar(
      window_title = "GeoApp",
      bg = "#2f466a",
      id = "tabs",
      fluid = TRUE,
      fillable = FALSE,
      title = div(
        style = "display:flex; align-items:center; margin-right: 10px;",
        img(
          src = "www/favicon.png",
          height = 30,
          width = 30,
          style = "margin:5px 5px"
        ),
        span("GeoApp", class = "navbar-title")
      ),
      theme = app_theme(),
      nav_panel(
        title = "Home",
        mod_home_ui("home_ui")
      ),
      nav_panel(
        title = "Demo Explorer",
        mod_demo_explorer_ui("demo_explorer_ui")
      ),
      nav_panel(
        title = "IDB Pyramids",
        mod_idb_pyramid_ui("idb_pyramid_ui")
      ),
      nav_panel(
        title = "HMD Demography",
        mod_hmd_demograph_ui("hmd_demograph_ui")
      ),
      # nav_panel(
      #   title = "Swiss Votes",
      #   mod_swiss_votes_ui("swiss_votes_ui")
      # ),
      nav_panel(
        title = "NOAA Climate",
        mod_noaa_climate_ui("noaa_climate_ui")
      ),
      nav_spacer(),
      nav_item(a("GeoDoc", href = "https://df.geoviews.ch"))
    )
  )
}

#' @importFrom shiny addResourcePath
golem_add_external_resources <- function() {
  shiny::addResourcePath(
    "www",
    system.file("app/www", package = "GeoApp")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(ext = "png"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "GeoApp"
    ),

    # nolint start commented_code_linter
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # nolint end
    tags$script(src = "www/gochart.js")
  )
}
