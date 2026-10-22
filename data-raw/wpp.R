# UN World Population Prospects 2024 → data/wpp/ for the Pyramids and the Population explorer.
#
#   Rscript data-raw/wpp.R
#
# Source: United Nations, Department of Economic and Social Affairs, Population Division (2024), World Population
# Prospects 2024, medium variant (CC BY 3.0 IGO), read with geodata::gdt_wpp_single_age() and
# geodata::gdt_wpp_indicators() (downloads cached in ~/Library/Caches/geodata/wpp). Estimates 1950–2023,
# projections 2024–2100.
#
# Writes
#   data/wpp/places.json         the countries and areas (ISO3) plus the World and the continents: code, en, fr, kind
#   data/wpp/pyramids/<code>.json  population by single year of age (0–99, 100+) and sex, 1950–2100, whole persons:
#                                { code, first, last, estimates, m: [[101] × years], f: [[101] × years] }
# Checks: every place has every year × age × sex; the pyramid of each year sums to TPopulation1July.

suppressMessages(library(data.table))

out <- "data/wpp"
dir.create(file.path(out, "pyramids"), recursive = TRUE, showWarnings = FALSE)
years <- 1950:2100
estimates <- 2023

# The World and the continents (WPP geographic regions), with codes that are not ISO3 codes
aggregates <- data.table(
  LocID = c(900, 903, 935, 908, 904, 905, 909),
  code = c("WORLD", "AFRICA", "ASIA", "EUROPE", "LATAM", "NORTHAM", "OCEANIA"),
  fr = c("Monde", "Afrique", "Asie", "Europe", "Amérique latine et Caraïbes", "Amérique du Nord", "Océanie")
)

sa <- as.data.table(geodata::gdt_wpp_single_age(years = years))
sa <- sa[LocTypeName == "Country/Area" | LocID %in% aggregates$LocID]
sa[aggregates, code := i.code, on = "LocID"]
sa[is.na(code), code := ISO3_code]
stopifnot(!anyNA(sa$code), !any(sa$code == ""))

# Places: English names from WPP, French names from countrycode (CLDR), the continents by hand
places <- unique(sa[, .(code, LocID, en = Location, kind = fifelse(LocID %in% aggregates$LocID, "region", "country"))])
fr_names <- setNames(countrycode::codelist$cldr.name.fr, countrycode::codelist$iso3c)
places[kind == "country", fr := fr_names[code]]
places[aggregates, fr := i.fr, on = "code"]
# Not in CLDR under these codes
places[code == "XKX", fr := "Kosovo"]
places[code == "MAF", fr := "Saint-Martin (partie française)"]
places[code == "FSM", fr := "États fédérés de Micronésie"]
missing_fr <- places[is.na(fr), code]
if (length(missing_fr)) message("No French name (English kept): ", paste(missing_fr, collapse = ", "))
places[is.na(fr), fr := en]
setorder(places, kind, en)
places[kind == "region", ord := match(code, aggregates$code)]
places <- rbind(places[kind == "region"][order(ord)], places[kind == "country"])[, ord := NULL]

# Every place: 151 years × 101 ages × 2 sexes
grid <- sa[, .N, by = code]
stopifnot(all(grid$N == length(years) * 101))
stopifnot(setequal(unique(sa$AgeGrpStart), 0:100))

# Totals: the sum of the single ages against TPopulation1July (both in thousands; rounding in the UN files)
ind <- as.data.table(geodata::gdt_wpp_indicators(years = years))
ind[aggregates, code := i.code, on = "LocID"]
ind[is.na(code), code := ISO3_code]
sums <- sa[, .(sum = sum(PopTotal)), by = .(code, Time)]
sums[ind, total := i.TPopulation1July, on = c("code", "Time")]
stopifnot(!anyNA(sums$total))
sums[, gap := abs(sum - total) / pmax(total, 1e-3)]
worst <- sums[order(-gap)][1]
message(sprintf("Largest gap between the pyramid and TPopulation1July: %.3f %% (%s %d)", 100 * worst$gap, worst$code, worst$Time))
stopifnot(sums[total > 10, max(gap)] < 0.005)

# One file per place: whole persons
setorder(sa, code, Time, AgeGrpStart)
for (cd in places$code) {
  p <- sa[code == cd]
  per_year <- function(col) unname(split(as.integer(round(p[[col]] * 1000)), p$Time))
  jsonlite::write_json(list(code = cd, first = min(years), last = max(years), estimates = estimates,
                            m = per_year("PopMale"), f = per_year("PopFemale")),
                       file.path(out, "pyramids", paste0(cd, ".json")), auto_unbox = TRUE)
}
jsonlite::write_json(list(source = "United Nations, World Population Prospects 2024 (CC BY 3.0 IGO)",
                          estimates = estimates, places = places[, .(code, en, fr, kind)]),
                     file.path(out, "places.json"), auto_unbox = TRUE)
message(sprintf("%d places written to %s", nrow(places), out))
