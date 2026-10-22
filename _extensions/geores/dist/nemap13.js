// The image nemap13.jpg for the pages that use it (see quarto/geores/geores.lua)
(function () {
  const G = globalThis.GeoRes = globalThis.GeoRes || { diagrams: {}, sims: {} }, s = document.currentScript;
  G.images = Object.assign(G.images || {}, { nemap13: (s ? s.src.replace(/[^/]*$/, "") : "") + "nemap13.jpg" });
})();
