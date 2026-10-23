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
- Release straight to app.geoviews.ch (no test address), when the user says so.
- Remote: `git@github.com:Nenuial/GeoApp.git` (`origin`), restarted clean on 2026-10-23 (the old Shiny repository was
  erased; its code stays in `Teaching/Z-Archives/OLD-GeoApp`). The user decides about pushing.
- Not Shiny, not Highcharts. World maps in Equal Earth (`GeoRes.maps.frame("equalearth")`), Köppen with
  `GeoRes.koppen.classify` (geotools now uses the same definition).

## Structure

- DF by field: `<field>/index.qmd` (+ `.fr.qmd`), one page per tool `<field>/<slug>.qmd` (+ `.fr.qmd`).
  Fields in course order: demography, cartography, geopolitics, urban | cosmology, geology, meteorology, climatology.
- OC by chapter, French only: `oc/<chapter>/index.fr.qmd` and `oc/<chapter>/<slug>.fr.qmd`. A new chapter:
  `make new-oc-chapter NAME=chine TITLE="Chine"`, then its cover, accent (`assets/styles.css`) and icon
  (`assets/tiles.ejs`). It appears on the OC page and the French home page by itself (listing of `oc/*/index.qmd`).
- Footer (same on every Geo site): "LDDR · Geography" left; right, the licence (CC BY-NC-SA 4.0) and AI label
  (Me & My Machine "cyborg") of the whole site, icons of GeoDoc's lddr-badges in `assets/badges/`, explained on
  `licence.qmd` (+ `.fr.qmd`).
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
- GeoApp's own tools are written in the GeoRes sim format (`GeoRes.sims["app-<tool>"]`) in
  `_extensions/geoapp/tools/<tool>.js` (helpers: `tools/_app.js`, `GeoRes.app`), and placed with
  `{{< app tool="<tool>" bundles="<GeoRes bundles>" presets="…" >}}` (`_extensions/geoapp/geoapp.lua`).
  Never write into `_extensions/geores/`: sync replaces it.
- The tools: pyramids, explorer (demography: WPP 2024), demograph (HMD), climates (GeoResources' WMO stations),
  votes (FSO, cartogram = GeoResources' vote-map with `src`). They draw SVG at the size of their box (text in pixels,
  legible on phones) and keep their state in the address.
- `_extensions/geoapp/french-spaces.lua` is a copy of quarto-keynote's filter (geokey): recopy it when geokey announces
  a change.
- Until GeoResources ships `dist/catalog.json`, the list of sim pages is written by hand (geokey's request).

## GeoResources

- `make sync` copies GeoResources (`tools/sync.sh`) into `_extensions/geores/`; commit the copy with its `VERSION`
  (never `-dirty`). `dist/svg/` (print SVGs) is gitignored and excluded from Dropbox: the website does not use it.
- New sims, changes to sims, data in GeoResources (e.g. the WMO climate normals): ask `geokey`. The any-vote data for
  Swiss votes is GeoApp's to build (`data-raw/`, schema of GeoResources `data/cartography/votes.json`).

## Data (`data-raw/`, outputs in `data/`, committed)

- `Rscript data-raw/wpp.R`: UN WPP 2024 (`geodata::gdt_wpp_*`) → `data/wpp` (places, pyramids, indicators).
  Next WPP release: every two years.
- `Rscript data-raw/hmd.R`: `geodata::gdt_hmd_demography` (geoverse) → `data/hmd/demograph.json`.
- `Rscript data-raw/votes.R`: **after each federal vote day** (4 times a year), then render, test, release.
  After each 1 January, the new commune geometries first (see the script), then `Rscript data-raw/votes.R all`.
- Climate stations: GeoResources' bundles (geokey), nothing to build here.

## Release (Docker on the VPS; every release approved by the user)

- Server `ubuntu@5.196.76.20`, folder `~/GeoApp`: `compose.yml` (service and container `geoapp`, port 3000, network `npm`
  of Nginx Proxy Manager, as GeoExam; the proxy host app.geoviews.ch forwards to `http://geoapp:3000`), `.env` (`GEOAPP_TAG`), `releases/<tag>/` (site + Dockerfile + Caddyfile).
- `tools/release.sh v2.x.y`: checks (committed, rendered, no conflict copies), tags, sends, builds `geoapp:<tag>` on
  the server, starts it. `tools/release.sh --rollback <tag>` goes back to an earlier image.
- After a release: `cd tests && GEOAPP_URL=https://app.geoviews.ch npx playwright test`.
- `docker/Caddyfile`: clean URLs, precompressed data, `no-cache` + ETag, `noindex`, no X-Frame-Options (Moodle).
  Local check: `colima start`, build `docker/` with `_build/site` as `site/`, run on a free port, `GEOAPP_URL=…`.

## Commands

- `make render`: `render.sh` copies the sources to `~/Library/Caches/geoapp-render/GeoApp`, runs
  `babelquarto::render_website(site_url = "")` (whole site, language links root-relative), copies `_site` to
  `_build/site` (excluded from Dropbox).
- `make serve`: the built site at http://localhost:4321 (`tools/serve.mjs`: clean URLs and `.gz` like Caddy).
- `make check`: every English page has its French twin, OC pages are French only (`tools/check-pairs.mjs`).
- `make test`: Playwright (`tests/`) at phone, phone landscape, 1280×800 and 1920×1080, dark and light.
  `node tests/screens.mjs <dir>` takes review screenshots (with `make serve` running).
- `make sweep` (with `make serve` running): every slider of every sim through its range at 1280, 1000, 800 and 390 px;
  the controls must keep one layout (house rule: moving a slider moves nothing else). After each GeoResources sync.
- Before a commit: `find . -name "*onflit*"`. Git dir: `/Users/pascalburkhard/git/GeoApp.git` (outside Dropbox).
