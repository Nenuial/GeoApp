// The image bernmap.jpg for the pages that use it (see quarto/geores/geores.lua)
(function () {
  const G = globalThis.GeoRes = globalThis.GeoRes || { diagrams: {}, sims: {} }, s = document.currentScript;
  G.images = Object.assign(G.images || {}, { bernmap: (s ? s.src.replace(/[^/]*$/, "") : "") + "bernmap.jpg" });
})();
