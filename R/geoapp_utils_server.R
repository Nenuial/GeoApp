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
#' @param code A WB ISO3 code
#'
#' @return The country name
#' @export
get_wb_country_name <- function(code) {
  wbstats::wb_countries() |>
    dplyr::filter(iso3c == code) |>
    dplyr::pull(country)
}

#' Get country name for specific code type
#'
#' @param code Country code
#' @param code_type Code type in the countrycode package
#'
#' @return A list of country names
#' @export
get_country_name <- function(code, code_type) {
  countrycode::countrycode(code, origin = code_type, destination = "country.name.en")
}


#' Leaflet add_legend
#'
#' @param map A leaflet map
#' @param position The position
#' @param pal The palettes
#' @param values Palettes values
#' @param na_label Label for na
#' @param bins Number of bins
#' @param colors The color
#' @param opacity The opacity
#' @param labels The labels
#' @param lab_format Format for labels
#' @param title The title
#' @param class_name The class name
#' @param layer_id The layer id
#' @param group The group
#' @param data The data
#' @param decreasing Decreasing?
#'
#' @return A leaflet legend
#' @export
# nolint start cyclocomp_linter
add_legend <- function(map, position = c(
  "topright", "bottomright", "bottomleft",
  "topleft"
), pal, values, na_label = "NA", bins = 7, colors,
opacity = 0.5, labels = NULL, lab_format = leaflet::labelFormat(),
title = NULL, class_name = "info legend", layer_id = NULL,
group = NULL, data = leaflet::getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na_color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) {
      stop("You must provide either 'pal' or 'colors' (not both)")
    }
    if (missing(title) && inherits(values, "formula")) {
      title <- deparse(values[[2]])
    }
    values <- leaflet::evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na_color <- args$na.color
    if (!is.null(na_color) && grDevices::col2rgb(na_color, alpha = TRUE)[[4]] == 0) {
      na_color <- NULL
    }
    if (type != "numeric" && !missing(bins)) {
      warning("'bins' is ignored because the palette type is not numeric")
    }
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) {
        pretty(values, bins)
      } else {
        bins
      }

      if (length(bins) > 2) {
        if (!all(abs(diff(bins, differences = 2)) <= sqrt(.Machine$double.eps))) {
          stop("The vector of breaks 'bins' must be equally spaced")
        }
      }
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1]) / (r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE) {
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(lab_format(type = "numeric", cuts))
      } else {
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(lab_format(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    } else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n]) / 2
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(lab_format(type = "bin", cuts))
      } else {
        colors <- pal(mids)
        labels <- lab_format(type = "bin", cuts)
      }
    } else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- stats::quantile(values, probs = p, na.rm = TRUE)
      mids <- stats::quantile(values,
        probs = (p[-1] + p[-n]) / 2,
        na.rm = TRUE
      )
      if (decreasing == TRUE) {
        colors <- pal(rev(mids))
        labels <- rev(lab_format(type = "quantile", cuts, p))
      } else {
        colors <- pal(mids)
        labels <- lab_format(type = "quantile", cuts, p)
      }
    } else if (type == "factor") {
      v <- sort(unique(stats::na.omit(values)))
      colors <- pal(v)
      labels <- lab_format(type = "factor", v)
      if (decreasing == TRUE) {
        colors <- pal(rev(v))
        labels <- rev(lab_format(type = "factor", v))
      } else {
        colors <- pal(v)
        labels <- lab_format(type = "factor", v)
      }
    } else {
      stop("Palette function not supported")
    }
    if (!any(is.na(values))) {
      na_color <- NULL
    }
  } else {
    if (length(colors) != length(labels)) {
      stop("'colors' and 'labels' must be of the same length")
    }
  }
  legend <- list(
    colors = I(unname(colors)), labels = I(unname(labels)),
    na_color = na_color, na_label = na_label, opacity = opacity,
    position = position, type = type, title = title, extra = extra,
    layerId = layer_id, className = class_name, group = group
  )
  leaflet::invokeMethod(map, data, "add_legend", legend)
}
# nolint end
