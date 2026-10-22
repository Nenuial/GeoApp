-- Shortcode {{< geores >}}: a GeoResources diagram or sim, in the right form for each format.
--
--   {{< geores svg="cosmology/seasons-orbit" diagram="seasons-orbit" >}}
--   {{< geores svg="cosmology/summer-solstice" sim="season-steps" season="summer" >}}
--   {{< geores sim="earth-year" >}}
--
--   svg      static SVG in dist/svg (without .<lang>.svg): used for Typst, LaTeX, docx...
--   sim      HTML: interactive sim (GeoRes.sims)
--   diagram  HTML: diagram drawn by the page, in its light or dark theme (GeoRes.diagrams)
--   image    picture for non-HTML formats when there is no svg (path as in a Markdown image)
--   width    width of the svg or image in non-HTML formats (e.g. "80%")
--   lang     "en" or "fr" (default: the document's lang)
--   any other key=value is passed to the sim or diagram as data-key="value"
--
-- In HTML (books and revealjs) the shortcode gives the sim, else the diagram, else the SVG;
-- elsewhere the SVG, else the image, else nothing (web-only sim).
-- The files come from dist/, installed with the extension by GeoResources/tools/sync.sh.

local OWN = { svg = true, sim = true, diagram = true, lang = true, image = true, width = true }

local function lang_of(kwargs, meta)
  local l = pandoc.utils.stringify(kwargs["lang"] or "")
  if l == "" and meta and meta.lang then l = pandoc.utils.stringify(meta.lang) end
  return l:sub(1, 2):lower() == "fr" and "fr" or "en"
end

-- Path of dist/ as seen from the document (the extension sits somewhere in _extensions/)
local function dist_path()
  local dist = quarto.utils.resolve_path("dist")
  local project = quarto.project.directory
  if project and dist:sub(1, #project) == project then
    local offset = quarto.project.offset or "."
    return pandoc.path.join({ offset, dist:sub(#project + 2) })
  end
  return dist
end

local function add_dependency()
  quarto.doc.add_html_dependency({
    name = "geores",
    version = "1.0.0",
    scripts = { "dist/textures.js", "dist/world.js", "dist/precip.js", "dist/geores.js" },
    stylesheets = { "dist/geores.css" },
    resources = {
      { name = "bluemarble.jpg", path = "dist/bluemarble.jpg" },
      { name = "blackmarble.jpg", path = "dist/blackmarble.jpg" },
      { name = "atmosphere.jpg", path = "dist/atmosphere.jpg" },
      { name = "helicopter.png", path = "dist/helicopter.png" },
      { name = "cloud.png", path = "dist/cloud.png" },
      { name = "watercycle.jpg", path = "dist/watercycle.jpg" },
      { name = "cloudtypes.jpg", path = "dist/cloudtypes.jpg" },
    },
  })
end

-- The climate data (about 1 MB) only on the pages that use it
local CLIMATE = { climogram = true, ["koppen-key"] = true, ["koppen-map"] = true, ["co2-curve"] = true, ["ice-core"] = true,
  ["global-temperature"] = true, ["warming-stripes"] = true, ["glacier-massbalance"] = true, ["glacier-length"] = true }
local function add_climate()
  quarto.doc.add_html_dependency({ name = "geores-climate", version = "1.0.0", scripts = { "dist/climate.js" } })
end
-- Historical borders (about 0.4 MB) only on the pages with a history map
local HISTORY = { ["russia-territory"] = true, ["ussr-republics"] = true, ["golden-horde"] = true, ["cold-war"] = true }
local function add_history()
  quarto.doc.add_html_dependency({ name = "geores-history", version = "1.0.0", scripts = { "dist/history.js" } })
end
-- Plates, earthquakes, volcanoes and the age of the ocean floor (about 0.6 MB); the coasts of the past (about
-- 1.2 MB): only on the pages with a geology map
local TECTONICS = { ["plate-map"] = true }
local PALEO = { ["continents-drift"] = true }
-- The static geology maps (diagrams) that need more than the sim of the same name: only when used as diagrams, so
-- that the pages of the sims "plate-map" and "hotspot" don't load data they don't use
-- Large images used by one sim only (dist/<image>.js points GeoRes.images at the image next to it)
local OWN_IMAGES = { ["swiss-grid"] = { { "bernmap", "jpg" } },
  ["map-scales"] = { { "nemap17", "jpg" }, { "nemap15", "jpg" }, { "nemap13", "jpg" }, { "nemap11", "jpg" } } }
local function add_own_images(imgs)
  for _, img in ipairs(imgs) do
    quarto.doc.add_html_dependency({ name = "geores-" .. img[1], version = "1.0.0", scripts = { "dist/" .. img[1] .. ".js" },
      resources = { { name = img[1] .. "." .. img[2], path = "dist/" .. img[1] .. "." .. img[2] } } })
  end
end
local DIAGRAM_TECTONICS = { ["ocean-floor-age"] = true, ["volcanoes-map"] = true, ["hotspot"] = true }
local DIAGRAM_COUNTRIES = { ["africa-un-votes"] = true, ["russia-africa"] = true, ["russia-gas"] = true, ["plate-map"] = true, ["ocean-floor-age"] = true, ["hotspot"] = true }
local function add_tectonics()
  quarto.doc.add_html_dependency({ name = "geores-tectonics", version = "1.0.0", scripts = { "dist/tectonics.js" } })
end
local function add_paleo()
  quarto.doc.add_html_dependency({ name = "geores-paleo", version = "1.0.0", scripts = { "dist/paleo.js" } })
end
-- Countries of the world (about 0.5 MB) only on the pages with a country globe
local COUNTRIES = { ["africa-un-votes"] = true, ["russia-africa"] = true, ["russia-gas"] = true, ["country-globe"] = true, ["borders-pov"] = true, ["border-walls"] = true, ["chokepoints"] = true, ["arctic-routes"] = true, ["conflicts"] = true, ["alliances"] = true, ["urban-count"] = true, ["world-cities"] = true, ["global-cities"] = true, ["swiss-network"] = true, ["true-size"] = true, ["megalopolis"] = true }
local function add_countries()
  quarto.doc.add_html_dependency({ name = "geores-countries", version = "1.0.0", scripts = { "dist/countries.js" } })
end
-- Disputed areas, the points of view of 32 countries and the border walls (about 90 kB) for the geopolitics sims
local GEOPOLITICS = { ["borders-pov"] = true, ["border-walls"] = true, ["chokepoints"] = true, ["arctic-routes"] = true, ["conflicts"] = true }
local function add_geopolitics()
  quarto.doc.add_html_dependency({ name = "geores-geopolitics", version = "1.0.0", scripts = { "dist/geopolitics.js" } })
end
-- Sea zones and land (Natural Earth 1:10m) of the Eastern Mediterranean (about 560 kB) for the sea sim
local SEA = { ["emed-sea"] = true }
-- Cities, towns and rural areas of every country 1950–2050 and 2 500 cities 1975–2050 (about 360 kB) for the urban sims
local URBAN = { ["urban-count"] = true, ["world-cities"] = true, ["urban-transition"] = true, ["rank-size"] = true, ["global-cities"] = true, ["swiss-network"] = true, ["built-up"] = true, ["megalopolis"] = true }
local function add_urban()
  quarto.doc.add_html_dependency({ name = "geores-urban", version = "1.0.0", scripts = { "dist/urban.js" } })
end
-- A Swiss vote by commune (about 400 kB) for the cartography sims
local VOTES = { ["vote-map"] = true, ["map-classes"] = true }
local function add_votes()
  quarto.doc.add_html_dependency({ name = "geores-votes", version = "1.0.0", scripts = { "dist/votes.js" } })
end
-- Population of Russia (about 360 kB) for the Russia sims and diagrams
local RUSSIA = { ["russia-cross"] = true, ["russia-abortions"] = true, ["russia-pyramid"] = true, ["russia-lifeexp"] = true, ["russia-population"] = true,
  ["russia-regions"] = true, ["nivat-route"] = true }
local function add_russia()
  quarto.doc.add_html_dependency({ name = "geores-russia", version = "1.0.0", scripts = { "dist/russia.js" } })
end
local function add_sea()
  quarto.doc.add_html_dependency({ name = "geores-sea", version = "1.0.0", scripts = { "dist/sea.js" } })
end
-- The 0.1° classes of Europe (about 0.7 MB) only for the Europe view of the Köppen map
-- The 0.1° classes of Russia (about 0.9 MB) only for the Russia view of the Köppen map
local function add_climate_russia()
  quarto.doc.add_html_dependency({ name = "geores-climate-russia", version = "1.0.0", scripts = { "dist/climate-russia.js" } })
end
local function add_climate_europe()
  quarto.doc.add_html_dependency({ name = "geores-climate-europe", version = "1.0.0", scripts = { "dist/climate-europe.js" } })
end

local function picture(src, width)
  return pandoc.Image({}, src, "", width and pandoc.Attr("", {}, { width = width }) or nil)
end

return {
  ["geores"] = function(args, kwargs, meta)
    local get = function(k) local v = pandoc.utils.stringify(kwargs[k] or ""); return v ~= "" and v or nil end
    local svg, sim, diagram = get("svg"), get("sim"), get("diagram")
    local lang = lang_of(kwargs, meta)

    if quarto.doc.is_format("html") and (sim or diagram) then
      add_dependency()
      if CLIMATE[sim or diagram] then add_climate() end
      if (sim or diagram) == "koppen-map" and get("region") == "europe" then add_climate_europe() end
      if (sim or diagram) == "koppen-map" and get("region") == "russia" then add_climate_russia() end
      if HISTORY[sim or diagram] then add_history() end
      if COUNTRIES[sim or diagram] then add_countries() end
      if GEOPOLITICS[sim or diagram] then add_geopolitics() end
      if SEA[sim or diagram] then add_sea() end
      if URBAN[sim or diagram] then add_urban() end
      if VOTES[sim or diagram] then add_votes() end
      if RUSSIA[sim or diagram] then add_russia() end
      if TECTONICS[sim or diagram] then add_tectonics() end
      if PALEO[sim or diagram] then add_paleo() end
      if sim and OWN_IMAGES[sim] then add_own_images(OWN_IMAGES[sim]) end
      if diagram and DIAGRAM_TECTONICS[diagram] then add_tectonics() end
      if diagram and DIAGRAM_COUNTRIES[diagram] then add_countries() end
      local attrs = { ["data-lang"] = lang }
      if sim then attrs["data-sim"] = sim else attrs["data-diagram"] = diagram end
      for k, v in pairs(kwargs) do
        if not OWN[k] then attrs["data-" .. k] = pandoc.utils.stringify(v) end
      end
      return pandoc.Div({}, pandoc.Attr("", { "geores" }, attrs))
    end

    if svg then return picture(pandoc.path.join({ dist_path(), "svg", svg .. "." .. lang .. ".svg" }), get("width")) end
    if get("image") then return picture(get("image"), get("width")) end
    return pandoc.Null()
  end,
}
