app_server <- function(input, output, session) {
  mod_home_server("home_ui")
  mod_demo_explorer_server("demo_explorer_ui")
  mod_hmd_demograph_server("hmd_demograph_ui")
  mod_idb_pyramid_server("idb_pyramid_ui")
  mod_swiss_votes_server("swiss_votes_ui")
  mod_noaa_climate_server("noaa_climate_ui")
}
