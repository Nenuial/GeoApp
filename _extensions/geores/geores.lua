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
    scripts = { "dist/world.js", "dist/geores.js" },
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

-- The data files a page loads only with the figures that use them (Earth texture, precipitation, climate, history,
-- countries…), and the large pictures of one sim: bundles.json, next to this file (also read by tools/build.mjs for
-- dist/catalog.json)
local BUNDLES = (function()
  local f = io.open(quarto.utils.resolve_path("bundles.json"), "r")
  if not f then return { bundles = {}, images = {} } end
  local b = pandoc.json.decode(f:read("a"), false)
  f:close()
  return b
end)()

local function has(list, name)
  for _, v in ipairs(list or {}) do if v == name then return true end end
  return false
end

-- Every bundle the sim or diagram needs, with these options of the shortcode
local function add_bundles(sim, diagram, get)
  local name = sim or diagram
  for id, b in pairs(BUNDLES.bundles) do
    if has(b.names, name) or (diagram and has(b.diagrams, diagram)) then
      local ok = true
      for k, v in pairs(b.when or {}) do if get(k) ~= v then ok = false end end
      if ok then quarto.doc.add_html_dependency({ name = "geores-" .. id, version = "1.0.0", scripts = { "dist/" .. b.script } }) end
    end
  end
  -- large pictures of one figure (sim or diagram)
  local own = {}
  for _, img in ipairs(sim and BUNDLES.images[sim] or {}) do table.insert(own, img) end
  for _, img in ipairs(diagram and BUNDLES.images[diagram] or {}) do table.insert(own, img) end
  for _, img in ipairs(own) do
    quarto.doc.add_html_dependency({ name = "geores-" .. img[1], version = "1.0.0", scripts = { "dist/" .. img[1] .. ".js" },
      resources = { { name = img[1] .. "." .. img[2], path = "dist/" .. img[1] .. "." .. img[2] } } })
  end
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
      add_bundles(sim, diagram, get)
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
