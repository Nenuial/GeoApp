# GeoApp

The interactive site of the geography classes, https://app.geoviews.ch: GeoResources sims and GeoApp's own data
tools. The session `geoapp` owns this folder and the app.geoviews.ch container (see `Work/Geography/CLAUDE.md` for
the other sessions and the shared folders). Plan: `~/.claude/plans/you-are-geoapp-you-unified-clover.md`.

## Decisions (user, 2026-10-22)

- Quarto website, the look of GeoDoc (flatly/darkly, dark first = default, Fira Sans, navbar `#2f466a`).
- English + French with babelquarto (English main, French under `/fr/`). **OC is French only.**
- Public (robots `Disallow`), no tracking. Deployed as a Docker compose image (caddy) on the VPS; every deploy approved
  by the user.
- Population data: UN WPP 2024 (one-year age pyramids for every country, 1950–2100), via `geodata::gdt_wpp_*`.
- Not Shiny, not Highcharts. World maps in Equal Earth (`GeoRes.maps.frame("equalearth")`), Köppen with
  `GeoRes.koppen.classify` (geotools now uses the same definition).

## Structure

- DF by field: `<field>/index.qmd` (+ `.fr.qmd`), one page per tool `<field>/<slug>.qmd` (+ `.fr.qmd`).
  Fields in course order: demography, cartography, geopolitics, urban | cosmology, geology, meteorology, climatology.
- OC by chapter, French only: `oc/<chapter>/index.fr.qmd` and `oc/<chapter>/<slug>.fr.qmd`. A new chapter:
  `make new-oc-chapter NAME=chine TITLE="Chine"`, then its cover, accent (`assets/styles.css`) and icon
  (`assets/tiles.ejs`). It appears on the OC page and the French home page by itself (listing of `oc/*/index.qmd`).
- A tool lives once; other listings reach it through categories (no duplicate pages).
- **Stable URLs** (other projects link to them): `/<field>/<slug>`, `/fr/<field>/<slug>`, `/fr/oc/<chapter>/<slug>`.
  The slug of a GeoResources sim is its name. Never rename a published page.

## Tool pages

```
::: {.column-page}
{{< geores sim="climogram" presets="station,compare" >}}
:::
```

- `presets`: the options a link may set (`?station=ch-ber`), copied onto `data-*` by `assets/geoapp.js` before
  `GeoRes.mount()`. Only list options that are safe to take from an address.
- `assets/geoapp.js` adds the toolbar (Full screen, Link, Embed) and the modes `?embed` (the tool alone, for iframes)
  and `?present` / key P (projector).
- Page text: description (one line, shown under the title and in listings), an intro that does not repeat it,
  "What to look for" (classroom prompts), "How it works", "Sources". The user reviews the texts.
- House rules for figures (GeoResources): no native `title` tooltips on figures, controls never over chart text,
  blue only for water on maps, a selection is an outline, a non-breaking space before %, pages work on phones.
- GeoApp's own tools (rebuilt old tools) are written in the GeoRes sim format (`GeoRes.sims["app-…"]`) and loaded by
  GeoApp's own shortcode in `_extensions/geoapp/` (to come). Never write into `_extensions/geores/`: sync replaces it.
- Until GeoResources ships `dist/catalog.json`, the list of sim pages is written by hand (geokey's request).

## GeoResources

- `make sync` copies GeoResources (`tools/sync.sh`) into `_extensions/geores/`; commit the copy with its `VERSION`
  (never `-dirty`). `dist/svg/` (print SVGs) is gitignored and excluded from Dropbox: the website does not use it.
- New sims, changes to sims, data in GeoResources (e.g. the WMO climate normals): ask `geokey`. The any-vote data for
  Swiss votes is GeoApp's to build (`data-raw/`, schema of GeoResources `data/cartography/votes.json`).

## Commands

- `make render`: `render.sh` copies the sources to `~/Library/Caches/geoapp-render/GeoApp`, runs
  `babelquarto::render_website(site_url = "")` (whole site, language links root-relative), copies `_site` to
  `_build/site` (excluded from Dropbox).
- `make serve`: the built site at http://localhost:4321 (`tools/serve.mjs`: clean URLs and `.gz` like Caddy).
- `make check`: every English page has its French twin, OC pages are French only (`tools/check-pairs.mjs`).
- `make test`: Playwright (`tests/`) at phone, phone landscape, 1280×800 and 1920×1080, dark and light.
  `node tests/screens.mjs <dir>` takes review screenshots (with `make serve` running).
- Before a commit: `find . -name "*onflit*"`. Git dir: `/Users/pascalburkhard/git/GeoApp.git` (outside Dropbox).
