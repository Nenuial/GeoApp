#' @import bs4Dash
#' @import shinyWidgets
#' @importFrom shiny tagList
app_ui <- function() {
  theme <- fresh::create_theme(
    fresh::bs4dash_status(primary = "#5E81AC", danger = "#BF616A")
  )


  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Firt level UI
    bs4DashPage(
      freshTheme = theme,

      # Navbar ------------------------------------------------------------------
      header = bs4DashNavbar(
        status = "white",
        border = TRUE,
        skin = "light",
        "Geography App"
      ),
      title = "GeoApp",

      # Sidebar -----------------------------------------------------------------
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "GeoApp",
        bs4SidebarMenu(
          id = "sidebar",
          bs4SidebarMenuItem(
            text = "Home",
            tabName = "home",
            icon = shiny::icon("home")
          ),
          bs4SidebarHeader("Human"),
          bs4SidebarMenuItem(
            text = "Demo explorer",
            tabName = "demo_explorer",
            icon = shiny::icon("map")
          ),
          bs4SidebarMenuItem(
            text = "IDB Pyramid",
            tabName = "idb_pyramid",
            icon = shiny::icon("users")
          ),
          bs4SidebarMenuItem(
            text = "HMD Demograph",
            tabName = "hmd_demograph",
            icon = shiny::icon("chart-line")
          ),
          bs4SidebarMenuItem(
            text = "Swiss Votes",
            tabName = "swiss_votes",
            icon = shiny::icon("envelope")
          ),
          bs4SidebarHeader("Physical"),
          bs4SidebarMenuItem(
            text = "NOAA Climate",
            tabName = "noaa_climate",
            icon = shiny::icon("sun")
          )
        )
      ),


      # Page body ---------------------------------------------------------------
      body = bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "home",
            mod_home_ui("home_ui")
          ),
          bs4TabItem(
            tabName = "demo_explorer",
            mod_demo_explorer_ui("demo_explorer_ui")
          ),
          bs4TabItem(
            tabName = "idb_pyramid",
            mod_idb_pyramid_ui("idb_pyramid_ui")
          ),
          bs4TabItem(
            tabName = "hmd_demograph",
            mod_hmd_demograph_ui("hmd_demograph_ui")
          ),
          bs4TabItem(
            tabName = "swiss_votes",
            mod_swiss_votes_ui("swiss_votes_ui")
          ),
          bs4TabItem(
            tabName = "noaa_climate",
            mod_noaa_climate_ui("noaa_climate_ui")
          )
        )
      )
    )
  )
}

#' @importFrom shiny addResourcePath
golem_add_external_resources <- function() {
  shiny::addResourcePath(
    "www", system.file("app/www", package = "GeoApp")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(ext = "png"),

    # nolint start commented_code_linter
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # nolint end
    tags$script(src = "www/gochart.js")
  )
}
