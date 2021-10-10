#' Latest swiss vote date
#'
#' @return A string
#' @export
geoapp_swiss_votes_date <- function() {
  swissdd::get_nationalvotes("national", language = "FR") |> 
    dplyr::pull(votedate) |> 
    purrr::pluck(1)
}

#' Get country name for WB ISO3 code
#'
#' @param iso3c A WB ISO3 code
#'
#' @return The country name
#' @export
get_wb_country_name <- function(code) {
  wbstats::wbcountries() %>% 
    dplyr::filter(iso3c == code) %>% 
    dplyr::pull(country)
}

#' Get country name for specific code type
#'
#' @param code Country code
#' @param code_type Code type in the countrycode package
#'
#' @return
#' @export
get_country_name <- function(code, code_type) {
  countrycode::countrycode(code, origin = code_type, destination = "country.name.en")
}


#' Leaflet addLegend
#'
#' @param map 
#' @param position 
#' @param pal 
#' @param values 
#' @param na.label 
#' @param bins 
#' @param colors 
#' @param opacity 
#' @param labels 
#' @param labFormat 
#' @param title 
#' @param className 
#' @param layerId 
#' @param group 
#' @param data 
#' @param decreasing 
#'
#' @return A leaflet legend
#' @export
addLegend <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                         "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                       opacity = 0.5, labels = NULL, labFormat = leaflet::labelFormat(), 
                       title = NULL, className = "info legend", layerId = NULL, 
                       group = NULL, data = leaflet::getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- leaflet::evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && grDevices::col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  leaflet::invokeMethod(map, data, "addLegend", legend)
}