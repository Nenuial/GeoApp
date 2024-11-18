#' swiss_votes UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_swiss_votes_ui <- function(id) {
  ns <- NS(id)

  card(
    title = "Swiss Votes",
    full_screen = TRUE,
    card_header("Swiss Votes"),
    layout_sidebar(
      sidebar = sidebar(
        # Sidebar --------------------------------------------------------------
        layout_column_wrap(
          width = "200px",
          fixed_width = FALSE,
          shiny::selectInput(
            inputId = ns("vote_date"),
            label = "Date",
            choices = geoapp_ui_swiss_votes_dates()
          ),
          shiny::selectInput(
            inputId = ns("geolevel"),
            label = "Scale",
            choices = c("Canton" = "canton", "District" = "district", "Municipality" = "municipality")
          ),
          shiny::uiOutput(
            outputId = ns("vote_ui")
          )
        )
      ),
      # Plot --------------------------------------------------------------------
      shinyjqui::jqui_resizable(
        highcharter::highchartOutput(
          outputId = ns("map_area"),
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

#' swiss_votes Server Functions
#'
#' @noRd
mod_swiss_votes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update input votes ------------------------------------------------------
    observe({
      req(input$vote_date)

      output$vote_ui <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("vote_id"),
          label = "Vote",
          choices = geoapp_ui_swiss_votes(input$vote_date)
        )
      })
    })

    # Update map -------------------------------------------------------------
    observe({
      req(input$vote_date, input$vote_id, input$geolevel)

      hc <- tryCatch(
        {
          geographer::gph_highcharter_map_swiss_votes(
            geolevel = input$geolevel,
            votedates = input$vote_date,
            id = input$vote_id
          ) |>
            ggeo::hc_dark_web_theme()
        },
        error = function(cond) {
          # TODO: Handle error

          return(NULL)
        },
        warning = function(cond) {
          # TODO: Handle warning
        }
      )

      req(hc)

      output$map_area <- highcharter::renderHighchart(hc)
    })
  })
}
