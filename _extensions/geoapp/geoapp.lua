-- Shortcode {{< app >}}: one of GeoApp's own tools (tools/<tool>.js, written in the GeoRes sim format).
--
--   {{< app tool="pyramids" presets="c,y,rel,cc,cy" >}}
--   {{< app tool="explorer" bundles="countries" >}}
--
--   tool     the file tools/<tool>.js; the sim is GeoRes.sims["app-<tool>"]
--   bundles  GeoResources data bundles the tool needs (ids of _extensions/geores/bundles.json), comma separated
--   lang     "en" or "fr" (default: the document's lang)
--   any other key=value is passed to the tool as data-key="value" (presets: see assets/geoapp.js)
--
-- The GeoResources base files are added under the dependency names of geores.lua, so a page with a GeoRes sim
-- and a GeoApp tool loads them once. Nothing is written into _extensions/geores (tools/sync.sh replaces it).

local OWN = { tool = true, bundles = true, lang = true }
local GEORES = "../geores/"

local function lang_of(kwargs, meta)
  local l = pandoc.utils.stringify(kwargs["lang"] or "")
  if l == "" and meta and meta.lang then l = pandoc.utils.stringify(meta.lang) end
  return l:sub(1, 2):lower() == "fr" and "fr" or "en"
end

local BUNDLES = (function()
  local f = io.open(quarto.utils.resolve_path(GEORES .. "bundles.json"), "r")
  if not f then return { bundles = {} } end
  local b = pandoc.json.decode(f:read("a"), false)
  f:close()
  return b
end)()

local function add_georesources(bundles)
  quarto.doc.add_html_dependency({
    name = "geores",
    version = "1.0.0",
    scripts = { GEORES .. "dist/world.js", GEORES .. "dist/geores.js" },
    stylesheets = { GEORES .. "dist/geores.css" },
  })
  for id in (bundles or ""):gmatch("[^,%s]+") do
    local b = BUNDLES.bundles[id]
    if b then
      quarto.doc.add_html_dependency({ name = "geores-" .. id, version = "1.0.0", scripts = { GEORES .. "dist/" .. b.script } })
    else
      quarto.log.warning("app: unknown GeoResources bundle " .. id)
    end
  end
end

return {
  ["app"] = function(args, kwargs, meta)
    local get = function(k) local v = pandoc.utils.stringify(kwargs[k] or ""); return v ~= "" and v or nil end
    local tool = get("tool")
    if not tool then
      quarto.log.warning("app: no tool")
      return pandoc.Null()
    end
    if not quarto.doc.is_format("html") then return pandoc.Null() end
    add_georesources(get("bundles"))
    quarto.doc.add_html_dependency({ name = "geoapp", version = "1.0.0", scripts = { "tools/_app.js" } })
    quarto.doc.add_html_dependency({ name = "geoapp-" .. tool, version = "1.0.0", scripts = { "tools/" .. tool .. ".js" } })
    local attrs = { ["data-lang"] = lang_of(kwargs, meta), ["data-sim"] = "app-" .. tool }
    for k, v in pairs(kwargs) do
      if not OWN[k] then attrs["data-" .. k] = pandoc.utils.stringify(v) end
    end
    return pandoc.Div({}, pandoc.Attr("", { "geores", "app-tool" }, attrs))
  end,
}
