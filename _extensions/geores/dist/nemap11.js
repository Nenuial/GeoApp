// The image nemap11.jpg for the pages that use it (see quarto/geores/geores.lua)
(function () {
  const G = globalThis.GeoRes = globalThis.GeoRes || { diagrams: {}, sims: {} }, s = document.currentScript;
  G.images = Object.assign(G.images || {}, { nemap11: (s ? s.src.replace(/[^/]*$/, "") : "") + "nemap11.jpg" });
})();
