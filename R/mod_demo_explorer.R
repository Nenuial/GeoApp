# Module UI

#' @title   mod_demo_explorer_ui and mod_demo_explorer_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_demo_explorer
#'
#' @keywords internal
#' @export 
#' @import bs4Dash
#' @importFrom shiny NS tagList 
mod_demo_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Parameters --------------------------------------------------------------
    bs4Card(
      title = "Map Parameters",
      collapsed = TRUE,
      closable = FALSE,
      headerBorder = TRUE,
      status = "primary",
      width = 12,
      fluidRow(
        column(
          width = 6,
          shiny::selectInput(
            inputId = ns("dimension"),
            label = "Dimension",
            choices = list(
              "Population growth" = "growth",
              "Fertility" = "fertility",
              "Life expectancy" = "lex"
            )
          )
        ),
        column(
          width = 6,
          shiny::sliderInput(
            inputId = ns("year"), 
            label = "Year",
            min = 1960,
            max = as.integer(format(Sys.Date(), "%Y")), 
            value = 2015,
            step = 1
          )
        )
      )
    ),
    
    # Map, table and chart ----------------------------------------------------
    tabBox(
      id = "contentCard",
      maximizable = TRUE,
      closable = FALSE,
      status = "primary",
      type = "tabs",
      side = "left",
      width = 12,
      tabPanel(
        title = "Demography map",
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
        title = "Demography tables",
        column(
          12, 
          DT::dataTableOutput(
            outputId = ns("table")
          )
        )
      ),
      tabPanel(
        title = "Demography plots",
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
      )
    )
  )
}

# Module Server

#' @rdname mod_demo_explorer
#' @export
#' @keywords internal
#' @import rlang
#' @importFrom magrittr %>%
mod_demo_explorer_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- reactiveVal()
    dim <- reactiveVal()
    
    # Update dimension --------------------------------------------------------
    observe(priority = 100, {
      req(input$dimension)
      
      dim(
        get_dimension(input$dimension)
      )
    })
    
    
    # Update data -------------------------------------------------------------
    observe(priority = 90, {
      data(
        get_data(dim())
      )
    })
    
    
    # Update map --------------------------------------------------------------
    observe(priority = 50, {
      req(data(), input$year)
      
      data() %>%
        dplyr::filter(date == input$year) %>%
        dplyr::select(iso3c, data = dim()$map$indicator) %>%
        dplyr::mutate(cut = santoku::chop(data, breaks = dim()$map$breaks,
                                          labels = santoku::lbl_dash(),
                                          extend = T, drop = F) %>% 
                        forcats::fct_relabel(geotools::gtl_relabel_dash)) -> map_data
      
      rnaturalearth::ne_countries(scale = 50,
                                  returnclass = "sf") %>% 
        sf::st_transform(4326) %>% 
        dplyr::left_join(map_data, by = c("iso_a3" = "iso3c")) -> map
      
      leaflet::colorFactor(
        palette = as.character(ggeo::ggeopal_center(length(levels(map_data$cut)),
                                                    dim()$map$center,
                                                    dim()$map$palette)),
        domain = map_data$cut
      ) -> pal
      
      output$map_area <- leaflet::renderLeaflet({
        leaflet::leaflet(
          options = leaflet::leafletOptions(
            crs = leaflet::leafletCRS(crsClass = "L.Proj.CRS", code = "ESRI:54030",
                                      proj4def = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                                      resolutions = 1.5^(26:15))
          )
        ) %>% 
          leaflet.extras::setMapWidgetStyle(
            list(background = "white")
          ) %>% 
          leaflet::addPolygons(
            data = geographer::gph_boundbox(),
            fillColor = "#CDE4F2",
            fillOpacity = 1,
            weight = 1
          ) %>% 
          leaflet::addPolygons(
            data = map,
            weight = 1,
            fillOpacity = 1,
            fillColor = ~pal(map$cut),
            layerId = map$iso_a3,
            color = "#CCCCCC",
            popup = glue::glue("<strong>{map$name}</strong><br>
                              <strong>{dim()$map$legend}:</strong> {map$data} {dim()$map$unit}")
          ) %>% 
          # Modified leaflet addLegend to add decresing parameter
          GeoApp::addLegend(
            "bottomleft",
            title = dim()$map$legend,
            pal = pal, 
            values = map$cut,
            labels = levels(map$cut),
            opacity = 1,
            labFormat = leaflet::labelFormat(suffix = glue::glue(" {dim()$map$unit}")),
            na.label = "No data",
            decreasing = TRUE
          )
      })
    })
    
    
    # Update table ------------------------------------------------------------
    observe(priority = 40, {
      req(data())
      
      output$table <- DT::renderDataTable({
        data()%>%
          dplyr::mutate(
            country = countrycode::countrycode(
              sourcevar = iso3c, 
              origin = "iso3c",
              destination = "country.name",
              warn = FALSE
            ),
            action = glue::glue("<a class='go-chart' href='' data-iso='{iso3c}'><i class='fa fa-line-chart'></i></a>")
          ) %>% 
          dplyr::rename(year = date) %>% 
          dplyr::select(-iso3c) %>% 
          dplyr::select(country, dplyr::everything()) -> df
        
        action <- DT::dataTableAjax(session, df)
        
        DT::datatable(
          df, 
          options = list(
            ajax = list(url = action),
            pageLength = 10,
            lengthMenu = c(5, 10, 15)
          ), 
          escape = FALSE
        )
      })
    })
    
    # Update plot -------------------------------------------------------------
    observeEvent(input$map_area_shape_click, {
      req(data(), input$year, input$map_area_shape_click$id)
      
      data() %>% 
        dplyr::filter(iso3c == input$map_area_shape_click$id) -> plot_data
      
      req(nrow(plot_data) > 0)
      
      setup_plot(plot_data, input$map_area_shape_click$id)
    })
    
    
    
    # Update plot from table --------------------------------------------------
    observeEvent(input$goto, {
      req(data(), input$year, input$goto$iso)
      
      data() %>% 
        dplyr::filter(iso3c == input$goto$iso) -> plot_data
      
      req(nrow(plot_data) > 0)
      
      setup_plot(plot_data, input$goto$iso)
    })
    
    
    # Setup plot --------------------------------------------------------------
    setup_plot <- function(data, iso) {
      hc <- highcharter::highchart() %>% 
        highcharter::hc_title(text = glue::glue(dim()$plot$title)) %>% 
        highcharter::hc_xAxis(dim()$plot$axes$x) %>% 
        highcharter::hc_tooltip(shared = TRUE, crosshairs = TRUE) %>% 
        highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE)))
      
      hc$x$hc_opts$yAxis <- dim()$plot$axes$y
      
      for(param in dim()$plot$series) {
        hc %>% 
          highcharter::hc_add_series(
            data = data,
            "line",
            yAxis = param$yaxis,
            name = param$name,
            color = param$color,
            dashStyle = param$dash_style,
            tooltip = list(valueSuffix = param$suffix),
            highcharter::hcaes(x = date, y = !!param$indicator)
          ) -> hc
      }
      
      output$plot_area <- highcharter::renderHighchart({ hc })
    }
    
    # Fetch dimension data ----------------------------------------------------
    get_data_fun <- function(dim) {
      wbstats::wb_data(
        indicator = dim$data$codes,
        start_date = 1960,
        end_date = as.integer(format(Sys.Date(), "%Y")),
      ) %>% dplyr::mutate(!!!dim$data$operations)
    }
    # Create cached version of the get_data function with local cache
    get_data <- memoise::memoise(get_data_fun, 
                                 ~format(Sys.Date(), "%Y"),
                                 cache = memoise::cache_filesystem(paste0(rappdirs::user_cache_dir("geoapp"),
                                                                          "/.Rcache")))
    
    # Dimension definitions ---------------------------------------------------
    get_dimension <- function(key) {
      dim <- list(
        "growth" = list(
          title = "Population growth",
          map = list(
            indicator = "rni",
            legend    = "RNI",
            unit      = "%",
            palette   = list(palette = "pals::ocean.balance",
                             type = "cont", dir = -1),
            breaks    = c(-1, -.5, 0, 0, .5, 1, 1.5, 2, 5, 10),
            center    = 4
          ),
          plot = list(
            title = "Demograph for {get_wb_country_name(iso)}",
            axes = list(
              x = list(
                title = list(text = "Year")
              ),
              y = list(
                list(min = 0, title = list(text = "Rates")),
                list(min= 0, title = list(text = "Population", style = list(color = "blue")), opposite = TRUE)
              )
            ),
            series = list(
              list(
                indicator = expr(pop),
                name = "population",
                color = "blue",
                dash_style = "solid",
                suffix = "",
                yaxis = 1
              ),
              list(
                indicator = expr(cbr),
                name = "crude birth rate",
                color = "grey",
                dash_style = "solid",
                suffix = " ‰",
                yaxis = 0
              ),
              list(
                indicator = expr(cdr),
                name = "crude death rate",
                color = "black",
                dash_style = "solid",
                suffix = " ‰",
                yaxis = 0
              )
            )
          ),
          data = list(
            codes = c(
              pop = "SP.POP.TOTL",
              cbr = "SP.DYN.CBRT.IN",
              cdr = "SP.DYN.CDRT.IN"
            ),
            operations = list(
              rni = expr((cbr - cdr) / 10)
            )
          )
        ),
        
        "fertility" = list(
          title = "Fertility",
          map = list(
            indicator = "tfr",
            legend    = "TFR",
            unit      = "",
            palette   = list(palette = "viridis::viridis", 
                             type = "cont", dir = -1),
            breaks    = c(2,3,4,5,6,7),
            center    = -1
          ),
          plot = list(
            title = "Fertility for {get_wb_country_name(iso)}",
            axes = list(
              x = list(
                title = list(text = "Year")
              ),
              y = list(
                list(min = 0, title = list(text = "Fertility", style = list(color = "red"))),
                list(min= 0, title = list(text = "Mortality rate", style = list(color = "black")), opposite = TRUE)
              )
            ),
            series = list(
              list(
                indicator = expr(tfr),
                name = "fertility rate",
                color = "red",
                dash_style = "solid",
                suffix = "",
                yaxis = 0
              ),
              list(
                indicator = expr(cmr),
                name = "child mortality rate",
                color = "black",
                dash_style = "solid",
                suffix = " ‰",
                yaxis = 1
              )
            )
          ),
          data = list(
            codes = c(
              tfr = "SP.DYN.TFRT.IN",
              cmr = "SH.DYN.MORT"
            ),
            operations = list()
          )
        ),
        
        "lex" = list(
          title = "Life expectancy",
          map = list(
            indicator = "lex",
            legend    = "LEX",
            unit      = "",
            palette   = list(palette = "RColorBrewer::Spectral",
                             type = "dis", dir = 1),
            breaks    = c(40,50,60,65,70,75,80),
            center    = -1
          ),
          plot = list(
            title = "Life expectancy for {get_wb_country_name(iso)}",
            axes = list(
              x = list(
                title = list(text = "Year")
              ),
              y = list(
                list(title = list(text = "Life expectancy (years)"))
              )
            ),
            series = list(
              list(
                indicator = expr(lex),
                name = "life expectancy",
                color = "grey",
                dash_style = "ShortDot",
                suffix = "",
                yaxis = 0
              ),
              list(
                indicator = expr(lexm),
                name = "male life expectancy",
                color = "deepskyblue",
                dash_style = "solid",
                suffix = "",
                yaxis = 0
              ),
              list(
                indicator = expr(lexf),
                name = "female life expectancy",
                color = "fuchsia",
                dash_style = "solid",
                suffix = "",
                yaxis = 0
              )
            )
          ),
          data = list(
            codes = c(
              lex = "SP.DYN.LE00.IN",
              lexm = "SP.DYN.LE00.MA.IN",
              lexf = "SP.DYN.LE00.FE.IN"
            ),
            operations = list()
          )
        )
      )
      
      return(dim[[key]])
    }
  })
}
