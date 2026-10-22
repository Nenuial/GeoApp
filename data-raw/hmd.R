# Human Mortality Database → data/hmd/demograph.json for the Demograph.
#
#   Rscript data-raw/hmd.R
#
# Source: geodata::gdt_hmd_demography (Code, Year, Population, CBR, CDR), built by geodata/data-raw/gdt_hmd.R from the
# HMD (www.mortality.org, CC BY 4.0): births and deaths of the year over the population on 1 January, per 1,000.
# Writes { source, populations: [{ code, iso3, en, fr, first, last, pop: [], cbr: [], cdr: [] }] }, one value a year
# from first to last (null where a year is missing). iso3: the WPP code of the whole country, so that the Demograph
# can extend it to 2100 (null for the parts of a country: East and West Germany, the nations of the UK, Māori…).

suppressMessages(library(data.table))

d <- as.data.table(geodata::gdt_hmd_demography)
out <- "data/hmd"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Codes that are not ISO3 codes, or not the whole country
SPECIAL <- rbindlist(list(
  list("DEUTNP", "DEU", "Germany", "Allemagne"),
  list("DEUTE", NA, "East Germany", "Allemagne de l'Est"),
  list("DEUTW", NA, "West Germany", "Allemagne de l'Ouest"),
  list("FRATNP", "FRA", "France", "France"),
  list("GBR_NP", "GBR", "United Kingdom", "Royaume-Uni"),
  list("GBRTENW", NA, "England and Wales", "Angleterre et pays de Galles"),
  list("GBRCENW", NA, "England and Wales (civilians)", "Angleterre et pays de Galles (civils)"),
  list("GBR_SCO", NA, "Scotland", "Écosse"),
  list("GBR_NIR", NA, "Northern Ireland", "Irlande du Nord"),
  list("NZL_NP", "NZL", "New Zealand", "Nouvelle-Zélande"),
  list("NZL_MA", NA, "New Zealand (Māori)", "Nouvelle-Zélande (Maoris)"),
  list("NZL_NM", NA, "New Zealand (non-Māori)", "Nouvelle-Zélande (non-Maoris)"),
  list("TWN", "TWN", "Taiwan", "Taïwan"),
  list("HKG", "HKG", "Hong Kong", "Hong Kong")
))
setnames(SPECIAL, c("code", "iso3", "en", "fr"))

codes <- data.table(code = sort(unique(d$Code)))
codes[SPECIAL, `:=`(iso3 = i.iso3, en = i.en, fr = i.fr), on = "code"]
cl <- as.data.table(countrycode::codelist)[, .(iso3c, en = country.name.en, fr = cldr.name.fr)]
codes[is.na(en), `:=`(iso3 = code, en = cl$en[match(code, cl$iso3c)], fr = cl$fr[match(code, cl$iso3c)])]
stopifnot(!anyNA(codes$en), !anyNA(codes$fr))
# The WPP codes must exist in data/wpp (wpp.R)
wpp <- jsonlite::read_json("data/wpp/places.json")$places
wpp_codes <- vapply(wpp, `[[`, "", "code")
stopifnot(all(na.omit(codes$iso3) %in% wpp_codes))

pops <- lapply(seq_len(nrow(codes)), function(i) {
  cd <- codes$code[i]
  x <- d[Code == cd][order(Year)]
  yrs <- seq(min(x$Year), max(x$Year))
  at <- match(yrs, x$Year)
  list(code = cd, iso3 = codes$iso3[i], en = codes$en[i], fr = codes$fr[i], first = min(yrs), last = max(yrs),
       pop = x$Population[at], cbr = x$CBR[at], cdr = x$CDR[at])
})
gaps <- sum(vapply(pops, function(p) sum(is.na(p$pop)), 0))
message(sprintf("%d populations, %d-%d, %d missing years", length(pops), min(d$Year), max(d$Year), gaps))
jsonlite::write_json(list(source = "Human Mortality Database (www.mortality.org), CC BY 4.0", populations = pops),
                     file.path(out, "demograph.json"), auto_unbox = TRUE, na = "null", digits = NA)
