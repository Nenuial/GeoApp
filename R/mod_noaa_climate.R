#' noaa_climate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import bs4Dash
#' @importFrom shiny NS tagList 
mod_noaa_climate_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Warning placeholder -----------------------------------------------------
    div(id = "rnoaa_error", style = "position: absolute; top: 0; right: 0;"),
    
    # Map and chart ----------------------------------------------------
    tabBox(
      id = "contentCard",
      maximizable = TRUE,
      closable = FALSE,
      status = "primary",
      type = "tabs",
      side = "left",
      width = 12,
      tabPanel(
        title = "Stations map",
        shinyjqui::jqui_resizable(
          leaflet::leafletOutput(
            outputId = ns("map_area"),
            width = "100%",
            height = 600
          ),
          options = list(
            handles = "s",
            create =  shinyjqui::JS(
              'function(event, ui){ $(this).css("width", "100%"); }'
            )
          )
        )
      ),
      tabPanel(
        title = "Climate plot",
        tagList(
          shiny::fluidRow(
            shinyjqui::jqui_resizable(
              highcharter::highchartOutput(
                outputId = ns("plot_area"),
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
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::tableOutput(ns("climate_table"))
            )
          )
        )
      )
    )
  )
}

#' noaa_climate Server Function
#'
#' @noRd 
mod_noaa_climate_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cities <- geodata::gdt_ncdc_city_list() |> na.omit()
    
    # Setup map ---------------------------------------------------------------
    observe(priority = 100, {
      output$map_area <- leaflet::renderLeaflet({
        leaflet::leaflet(cities) |> 
          leaflet::addTiles() |> 
          leaflet::addMarkers(lat = ~lat,
                              lng = ~long,
                              layerId = ~id,
                              clusterOptions = leaflet::markerClusterOptions())
      })
    })
    
    
    
    # Update plot -------------------------------------------------------------
    observeEvent(input$map_area_marker_click, {
      city_data <- rnoaa::ncdc_locs(locationid = input$map_area_marker_click$id)$data
      
      climate_data <- tryCatch({
        climate_data <- get_climate_data(input$map_area_marker_click$id)
      },
      error = function(cond) {
        bs4Dash::createAlert(
          id = "rnoaa_error",
          options = list(
            title = "Error",
            closable = TRUE,
            width = 12,
            status = c("danger"),
            content = "No data could be fetched for this station !"
          )
        )
        
        return(NULL)
      },
      warning = function(cond) {
        bs4Dash::createAlert(
          id = "rnoaa_error",
          options = list(
            title = "Error",
            closable = TRUE,
            width = 12,
            status = c("danger"),
            content = "No data could be fetched for this station !"
          )
        )
        
        return(NULL)
      })
      
      req(climate_data)
      
      climate_plot <- get_climate_plot(climate_data, city_data)
      climate_table_data <- get_climate_table(climate_data)
      
      output$plot_area <-  highcharter::renderHighchart({ climate_plot })
      
      output$climate_table <- shiny::renderTable({
        climate_table_data
      }, striped=T, rownames=T)
    })
    
    
    # Get climate data --------------------------------------------------------
    get_climate_data <- function(location_id) {
      temp <- geodata::gdt_noaa_climate_data(location_id, data_type = "temperature")
      prec <- geodata::gdt_noaa_climate_data(location_id, data_type = "precipitation")
      
      return(list("temp" = temp,
                  "prec" = prec))
    }
    
    # Get climate plot --------------------------------------------------------
    get_climate_plot <- function(climate_data, city_data) {
      #Setup min and max value for Chart
      max <- ceiling(max(climate_data$prec$value)/20)*20
      min <- floor(min(climate_data$temp$value)/10)*10
      if(min > 0) min <- 0
      if(max(climate_data$temp$value) > max) max <- ceiling(max(climate_data$temp$value)/10)*10
      if((max-min) < 100) tick_interval <- 5 else tick_interval <- 10
      if((max-min) > 200) tick_interval <- 20
      
      # Get City latitude
      cities %>% 
        dplyr::filter(id == city_data$id) %>% 
        dplyr::pull(lat) -> city_latitude
      
      # Get Köppen Climate code
      koppen_climate <- geotools::gtl_koppen_code(temp = climate_data$temp$value,
                                                  prec = climate_data$prec$value,
                                                  lat = city_latitude)
      
      # Prettify the station name
      city_data$name %>% 
        stringr::str_to_title() %>% 
        stringr::str_extract("^[^,]*") -> city_name
      
      # Setup Highcharter plot
      highcharter::highchart() %>% 
        highcharter::hc_title(text = glue::glue("Average Monthly Weather Data for {city_name}")) %>% 
        highcharter::hc_subtitle(text = glue::glue("Average 1990-2015 - K\u00F6ppen Climate: {koppen_climate}")) %>% 
        highcharter::hc_xAxis(title = list(text = "Month"),
                              categories = c("", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                             "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
        highcharter::hc_yAxis_multiples(
          list(title = list(text = "Temperature",
                            style = list(color = 'red')),
               min = min,
               max = max / 2,
               labels = list(format = "{value} \u00B0C"),
               tickInterval = tick_interval),
          list(title = list(text = "Precipitation",
                            style = list(color = 'lightblue')),
               min = min * 2,
               max = max,
               labels = list(format = "{value} mm"),
               tickInterval = tick_interval * 2,
               opposite = TRUE)
        ) %>% 
        highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) %>% 
        highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>% 
        highcharter::hc_add_series(
          data = climate_data$prec,
          "column",
          yAxis = 1,
          name = "Precipitation",
          color = "lightblue",
          tooltip = list(valueSuffix = " mm"),
          highcharter::hcaes(x = month, y = value)
        ) %>% 
        highcharter::hc_add_series(
          data = climate_data$temp,
          "line",
          yAxis = 0,
          name = "Temperature",
          color = "red",
          tooltip = list(valueSuffix = " \u00B0C"),
          highcharter::hcaes(x = month, y = value)
        ) -> hc
      
      return(hc)
    }
    
    
    # Get climate table -------------------------------------------------------
    get_climate_table <- function(climate_data) {
      prec <- climate_data$prec$value
      prec[13] <- sum(prec)
      
      temp <- climate_data$temp$value
      temp[13] <- mean(prec)
      
      table <- data.frame()
      table = rbind(table, prec)
      table = rbind(table, temp)
      
      head <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec", "Tot/Avg")
      
      names(table) <- head
      row.names(table) <- c("P (mm)", "T (\u00B0C)")
      
      return(table)
    }
  })
}

