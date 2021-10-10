#' swiss_votes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_swiss_votes_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Plot parameters ---------------------------------------------------------
    bs4Card(
      title = "Map Parameters",
      closable = FALSE,
      status = "primary",
      width = 12,
      tagList(
        fluidRow(
          column(
            width = 6,
            shiny::selectInput(
              inputId = ns("vote_date"),
              label = "Date",
              choices = geoapp_ui_swiss_votes_dates()
            )
          ),
          column(
            width = 6,
            shiny::selectInput(
              inputId = ns("geolevel"),
              label = "Scale",
              choices = c("Canton" = "canton", "District" = "district", "Municipality" = "municipality")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            shiny::uiOutput(
              outputId = ns("vote_ui")
            )
          )
        )
      )
    ),
    
    # Map --------------------------------------------------------------------
    bs4Card(
      title = "Map",
      maximizable = TRUE,
      closable = FALSE,
      status = "primary",
      width = 12,
      shinyjqui::jqui_resizable(
        highcharter::highchartOutput(
          outputId = ns("map_area"),
          width = "100%",
          height = 500
        ),
        options = list(
          handles = "s",
          create =  shinyjqui::JS(
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
mod_swiss_votes_server <- function(id){
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
      
      hc <- tryCatch({
        geographer::gph_highcharter_map_swiss_votes(
          geolevel = input$geolevel,
          votedates = input$vote_date,
          id = input$vote_id
        )
      },
      error = function(cond) {
        bs4Dash::createAlert(
          id = "swiss_votes_error",
          options = list(
            title = "Error",
            closable = TRUE,
            width = 12,
            elevations = 4,
            status = c("danger"),
            content = "No data could be fetched!"
          )
        )
        
        return(NULL)
      },
      warning = function(cond) {
        bs4Dash::createAlert(
          id = "swiss_votes_error",
          options = list(
            title = "Error",
            closable = TRUE,
            width = 12,
            elevations = 4,
            status = c("danger"),
            content = "No data could be fetched!"
          )
        )
      }
      )
      
      req(hc)
      
      output$map_area <- highcharter::renderHighchart(hc)
    })
  })
}
