#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

# Globals
utils::globalVariables(
  c("votedate", "votedates", "iso3c", "country", "country.name.en", "name", "id",
    "pop", "cbr", "cdr", "tfr", "cmr", "lex", "lexm", "lexf", "Year", "Code", "Population",
    "CBR", "CDR", "ns", "lat", "month", "value", "data")
)

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  pals::alphabet
}
