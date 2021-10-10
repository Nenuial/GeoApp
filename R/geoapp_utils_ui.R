#' WB Country selector
#'
#' @param id A string to use as input id
#' @param label A label to display for the selector
#'
#' @return An f7Select input object
#' @export
geoapp_ui_wb_country_selector <- function(id, label) {
  wbstats::wbcountries() |> 
    dplyr::select(country, iso3c) |> 
    tibble::deframe() -> country_list
    
  shiny::selectInput(
    inputId = id,
    label = label,
    choices = country_list
  )
}

#' Country codes for selector
#'
#' @param code_type 
#'
#' @return A named character vector
#' @export
#' 
#' @import rlang
geoapp_ui_country_code <- function(code_type) {
  countrycode::codelist |> 
    dplyr::select(country.name.en, {{ code_type }}) |> 
    tibble::deframe()
}

#' Latest swiss votes and their ids
#'
#' @return A named character vector
#' @export
geoapp_ui_swiss_votes <- function(votedate) {
  swissdd::get_nationalvotes("national", votedates = votedate, language = "FR") |> 
    dplyr::select(name, id) |> 
    tibble::deframe()
}

#' Swiss vote dates
#'
#' @return A named character vector
#' @export
geoapp_ui_swiss_votes_dates <- function () {
  tibble::tibble(votedates = swissdd::available_votedates() |> clock::as_date()) |> 
    dplyr::arrange(dplyr::desc(votedates)) |> 
    dplyr::mutate(
      pretty = withr::with_locale(new = c("LC_TIME" = "fr_CH.UTF-8"), format(votedates, "%d %B %Y")),
      .before = "votedates"
    ) |> 
    tibble::deframe()
}
