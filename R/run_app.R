#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  cachedir <- rappdirs::user_cache_dir("geoapp")
  if (!file.exists(cachedir)) dir.create(cachedir, recursive = TRUE)
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      options = list(
        host = "0.0.0.0",
        port = 3801
      )
    ),
    golem_opts = list(...)
  )
}
