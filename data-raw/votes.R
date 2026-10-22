# Federal votes since 1981 by commune → data/votes/ for the Swiss votes tool.
#
#   Rscript data-raw/votes.R          new vote dates only (the results already fetched are kept in the cache)
#   Rscript data-raw/votes.R all      fetch every date again
#
# After each vote day (4 times a year): run, render, release. After each 1 January (communes merge): download the
# geometries of the new year (see GEO below), run with "all", render, release.
#
# Results: Federal Statistical Office (FSO), through opendata.swiss and the R package swissdd (GitHub, 1.1.6). The
# results of every vote are given for the communes of today, so one set of geometries serves 1981–today.
# Geometries: "Geometries: communes (politics) as of 01.01.2026" (FSO, the geometries of the federal votes, generalised
# level k4, opendata.swiss; open use, source required), the file GeoResources/tools/vote-data.R uses. Same coordinates
# as GeoResources' votes.json (LV95 in km, rounded to 50 m), so that its vote-map can draw them.
#
# Writes
#   data/votes/geo.json            communes [[id, name, canton, district, polygons]], districts [[id, name, canton,
#                                  polygons]], cantons [[id, name, polygons]], lakes [polygons]; polygons: [[ring as flat
#                                  [E, N, …] in km]]
#   data/votes/index.json          the vote dates, newest first: [{ date, proposals: [{ id, fr, yes, no }] }]
#   data/votes/<date>.json         { date, proposals: [{ id, fr, yes, no, eligible, communes: { yes, no, el } (arrays in the
#                                  order of geo.json's communes, null where a commune has no result), abroad: { yes, no }
#                                  (arrays of the cantons 1 to 26: the Swiss abroad count in their canton's result) }] }
# Cache (raw results, geometries): ~/Library/Caches/geoapp-data/votes

suppressMessages({ library(data.table); library(sf); library(jsonlite); library(rmapshaper) })
args <- commandArgs(trailingOnly = TRUE)
cache <- path.expand("~/Library/Caches/geoapp-data/votes")
GEO <- file.path(cache, "voge-2026-01-01.topojson")   # from opendata.swiss ("Stand am 01.01.2026"), or GeoResources raw
out <- "data/votes"
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(GEO))

# Geometries: simplified together (shared borders stay shared), rings in km rounded to 50 m (as vote-data.R)
read <- function(layer) { x <- st_read(GEO, layer = layer, quiet = TRUE); st_crs(x) <- 2056; x }
simp <- function(x, keep) ms_simplify(x, keep = keep, keep_shapes = TRUE)
rings <- function(geom) lapply(st_geometry(geom), function(g) {
  polys <- if (inherits(g, "MULTIPOLYGON")) unclass(g) else list(unclass(g))
  lapply(polys, function(p) lapply(p, function(r) { r <- round(r / 1000 * 20) / 20; as.vector(t(r[, 1:2])) }))
})
com <- simp(read("k4voge_20260101_gf"), 0.35)
dis <- simp(read("k4bezk_20260101_gf"), 0.35)
can <- simp(read("k4kant_20260101_gf_ohne_seen"), 0.35)
lak <- simp(read("K4seen_yyymmdd11"), 0.5)
cr <- rings(com); dr <- rings(dis); kr <- rings(can)
geo <- list(
  source = "Federal Statistical Office, geometries of the federal votes as of 01.01.2026 (opendata.swiss)",
  date = "2026-01-01",
  communes = lapply(seq_len(nrow(com)), function(i) list(com$vogeId[i], com$vogeName[i], com$kantId[i], com$bezkId[i], cr[[i]])),
  districts = lapply(seq_len(nrow(dis)), function(i) list(dis$bezkId[i], dis$bezkName[i], dis$kantId[i], dr[[i]])),
  cantons = lapply(seq_len(nrow(can)), function(i) list(can$kantId[i], can$kantName[i], kr[[i]])),
  lakes = rings(lak)
)
writeLines(toJSON(geo, auto_unbox = TRUE, digits = NA), file.path(out, "geo.json"))
ids <- com$vogeId

# The Swiss abroad, by canton (1 to 26): they count in their canton's result, not in a commune's
abroad <- function(a) {
  k <- as.integer(a$canton_id)
  list(yes = vapply(1:26, function(i) sum(a$jaStimmenAbsolut[k == i], na.rm = TRUE), 0),
       no = vapply(1:26, function(i) sum(a$neinStimmenAbsolut[k == i], na.rm = TRUE), 0))
}

# Results, one vote date at a time (cached as .rds)
dates <- sort(as.character(swissdd::available_votedates()), decreasing = TRUE)
index <- list()
for (d in dates) {
  raw <- file.path(cache, paste0("votes-", d, ".rds"))
  if (!file.exists(raw) || identical(args[1], "all")) {
    x <- as.data.table(swissdd::get_nationalvotes(votedates = d, geolevel = "municipality", language = "FR"))
    saveRDS(x, raw)
  }
  x <- readRDS(raw)
  x[, mun := as.integer(mun_id)]
  proposals <- lapply(split(x, by = "id", sorted = TRUE), function(p) {
    home <- p[mun < 9000]
    m <- match(ids, home$mun)
    v <- function(col) { y <- home[[col]][m]; y[is.na(m)] <- NA; as.integer(y) }
    list(id = p$id[1], fr = p$name[1], yes = sum(p$jaStimmenAbsolut, na.rm = TRUE), no = sum(p$neinStimmenAbsolut, na.rm = TRUE),
         eligible = sum(p$anzahlStimmberechtigte, na.rm = TRUE),
         communes = list(yes = v("jaStimmenAbsolut"), no = v("neinStimmenAbsolut"), el = v("anzahlStimmberechtigte")),
         abroad = abroad(p[mun >= 9000]),
         missing = sum(is.na(m)))
  })
  miss <- vapply(proposals, `[[`, 0, "missing")
  if (any(miss > 0.05 * length(ids))) warning(d, ": more than 5 % of the communes without result")
  proposals <- lapply(proposals, function(p) { p$missing <- NULL; p })
  writeLines(toJSON(list(date = d, proposals = unname(proposals)), auto_unbox = TRUE, na = "null", digits = NA),
             file.path(out, paste0(d, ".json")))
  index[[length(index) + 1]] <- list(date = d, proposals = unname(lapply(proposals, function(p) p[c("id", "fr", "yes", "no")])))
  message(sprintf("%s: %d proposals, communes without result: %s", d, length(proposals), paste(miss, collapse = " ")))
}
writeLines(toJSON(list(source = "Federal Statistical Office (FSO), opendata.swiss", dates = index), auto_unbox = TRUE, digits = NA),
           file.path(out, "index.json"))
message(sprintf("%d vote dates, %d proposals; geometries: %d communes, %d districts, %d cantons, %d lakes",
                length(index), sum(vapply(index, function(i) length(i$proposals), 0)), length(geo$communes),
                length(geo$districts), length(geo$cantons), length(geo$lakes)))
