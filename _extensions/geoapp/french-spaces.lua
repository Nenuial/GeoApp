-- Copy of Visualization/Quarto/quarto-keynote/_extensions/keynote/french-spaces.lua (keynote 3.14.1, owner geokey):
-- update it from there when that file changes. Used by GeoApp on every page; it only acts on French pages.
--
-- French typography in decks whose `lang` is French (fr, fr-CH…): the spaces that must not break become
-- non-breaking spaces (U+00A0, as wide as a space), so that a line never starts with ; : ! ? » or a unit:
--   before ; : ! ? » and after «         « texte » ; vraiment ?
--   « and » with no space are given one  «texte» -> « texte »
--   before % ‰ and the units after a number (15 %, 9 289 km, 20 °C, 26 millions, 1 200 CHF)
--   between the groups of digits of a number (1 568 032, 1:25 000)
-- Only spaces of the text change: code, maths, raw HTML and links' targets are left alone. A colon with no
-- space (12:30, 1:25 000) stays as it is.
local NB = "\u{a0}"

local BEFORE = { ";", ":", "!", "?", "»", "%", "‰" }
local UNITS = {
  ["%"] = true, ["‰"] = true, ["km"] = true, ["km²"] = true, ["km2"] = true, ["m"] = true, ["m²"] = true, ["cm"] = true,
  ["mm"] = true, ["kg"] = true, ["g"] = true, ["t"] = true, ["Mt"] = true, ["h"] = true, ["min"] = true, ["s"] = true,
  ["°C"] = true, ["°"] = true, ["ha"] = true, ["CHF"] = true, ["€"] = true, ["$"] = true, ["USD"] = true,
  ["millions"] = true, ["million"] = true, ["milliards"] = true, ["milliard"] = true, ["Md"] = true, ["Mds"] = true,
  ["ans"] = true, ["an"] = true, ["mois"] = true, ["jours"] = true, ["habitants"] = true, ["hab."] = true,
}

local function starts(s, p) return s:sub(1, #p) == p end
local function ends(s, p) return #s >= #p and s:sub(-#p) == p end

-- the first "word" of a string, without the punctuation that may follow it (km, → km)
local function head(s) return (s:match("^([^%s,;:%.!%?%)]+)") or s) end

local function is_number(s) return s:match("^%d[%d%.,]*$") ~= nil end

local function is_space(el) return el and (el.t == "Space" or el.t == "SoftBreak") end

local function nb_space(prev, nxt)
  if nxt and nxt.t == "Str" then
    for _, p in ipairs(BEFORE) do if starts(nxt.text, p) then return true end end
  end
  if prev and prev.t == "Str" then
    if ends(prev.text, "«") then return true end
    if nxt and nxt.t == "Str" then
      local w = head(nxt.text)
      -- the groups of a number (1 568 032, 1:25 000): a group of 1 to 3 digits, then exactly three digits
      local last = prev.text:match("%f[%d](%d+)$")
      if last and #last <= 3 and w:match("^%d%d%d$") then return true end
      -- a number and its unit
      if is_number(prev.text) and UNITS[w] then return true end
    end
  end
  return false
end

-- « and » attached to a word get their space: a mark after each « and before each », which becomes NB unless a
-- NB is already there or the guillemet ends (or starts) the word (then the space next to it becomes NB, above)
local function fix_str(s)
  if not (s:find("«", 1, true) or s:find("»", 1, true)) then return s end
  s = s:gsub("«", "«\1"):gsub("»", "\2»")
  s = s:gsub("\1" .. NB, NB):gsub("\1$", ""):gsub("\1", NB)
  s = s:gsub(NB .. "\2", NB):gsub("^\2", ""):gsub("\2", NB)
  return s
end

local function french(meta)
  local l = meta.lang and pandoc.utils.stringify(meta.lang) or ""
  return l:sub(1, 2):lower() == "fr"
end

-- the space at the end of bold or italic text, with the sign right after it: **santé **; (searched inside nested
-- containers); found by geodoc (lddr-french 1.0.1)
local function inlines_of(el)
  local ok, c = pcall(function() return el.content end)
  return ok and type(c) == "table" and c or nil
end
local function trailing_space(el)
  local c = inlines_of(el)
  if not c or #c == 0 then return nil end
  local last = c[#c]
  if is_space(last) then return el, #c end
  if inlines_of(last) then return trailing_space(last) end
  return nil
end
local function before_mark(el)
  if not el or el.t ~= "Str" then return false end
  for _, p in ipairs(BEFORE) do if starts(el.text, p) then return true end end
  return false
end

local function fix_inlines(inl)
  for i = 1, #inl do
    local el = inl[i]
    if inlines_of(el) and before_mark(inl[i + 1]) then
      local owner, k = trailing_space(el)
      if owner then owner.content[k] = pandoc.Str(NB) end
    end
    if is_space(el) and nb_space(inl[i - 1], inl[i + 1]) then
      inl[i] = pandoc.Str(NB)
    elseif el.t == "Str" then
      local t = fix_str(el.text)
      if t ~= el.text then inl[i] = pandoc.Str(t) end
    end
  end
  return inl
end

return {
  {
    Pandoc = function(doc)
      if not french(doc.meta) then return nil end
      return doc:walk({ Inlines = fix_inlines })
    end,
  },
}
