// GeoApp's own tools: helpers shared by tools/*.js, on top of GeoRes (GeoRes.app).
// Unlike the GeoRes canvas sims, the tools draw SVG at the size of their box, so the text keeps its size in pixels
// (legible on a phone) and the data come from the site's /data/ when needed.
(function (G) {
  const R_ = G.GeoRes;
  const cache = new Map();
  const NS = "http://www.w3.org/2000/svg";

  R_.addStrings({
    en: { appLoading: "Loading…", appError: "The data could not be loaded." },
    fr: { appLoading: "Chargement…", appError: "Les données n'ont pas pu être chargées." },
  });

  // Thousands separated: 8,345,512 / 8 345 512 (narrow no-break space)
  const int = (L, x) => Math.round(x).toLocaleString(R_.lang(L) === "fr" ? "fr-CH" : "en-GB").replace(/[ ’']/g, " ");
  // Large numbers in words: 8.3 million / 8,3 millions
  function big(L, x) {
    const fr = R_.lang(L) === "fr";
    const [d, w] = x >= 1e9 ? [1e9, fr ? "milliard" : "billion"] : x >= 1e6 ? [1e6, "million"] : [0, ""];
    if (!d) return int(L, x);
    const v = x / d, s = R_.num(L, v, v < 10 ? 2 : v < 100 ? 1 : 0);
    return `${s} ${w}${fr && v >= 2 ? "s" : ""}`;
  }

  R_.app = {
    int, big,
    // JSON of the site's data folder, fetched once: GeoRes.app.data("wpp/places.json")
    data(path) {
      if (!cache.has(path)) {
        cache.set(path, fetch(`/data/${path}`).then(r => {
          if (!r.ok) throw new Error(`${path}: ${r.status}`);
          return r.json();
        }));
      }
      return cache.get(path);
    },
    // The state of a tool in the address (?c=CHE&y=1970), without reloading; null or "" removes a key
    setUrl(state) {
      const u = new URL(location.href);
      Object.entries(state).forEach(([k, v]) => (v === null || v === undefined || v === "" || v === false
        ? u.searchParams.delete(k) : u.searchParams.set(k, v === true ? "1" : v)));
      history.replaceState(history.state, "", u);
    },
    // An SVG element: el("rect", { x: 1, y: 2 }, parent)
    el(tag, attrs = {}, parent) {
      const e = document.createElementNS(NS, tag);
      Object.entries(attrs).forEach(([k, v]) => v !== undefined && v !== null && e.setAttribute(k, v));
      if (parent) parent.appendChild(e);
      return e;
    },
    // A tooltip in the GeoRes style (.geores-tip), following the pointer
    tip(container) {
      const box = document.createElement("div");
      box.className = "geores-tip";
      box.hidden = true;
      document.body.appendChild(box);
      return {
        show(html, e) {
          box.classList.toggle("geores-tip-dark", container.classList.contains("geores-dark"));
          box.innerHTML = html;
          box.hidden = false;
          const r = box.getBoundingClientRect(), pad = 14;
          let x = e.clientX + pad, y = e.clientY + pad;
          if (x + r.width > innerWidth - 4) x = e.clientX - r.width - pad;
          if (y + r.height > innerHeight - 4) y = e.clientY - r.height - pad;
          box.style.left = `${Math.max(4, x)}px`;
          box.style.top = `${Math.max(4, y)}px`;
        },
        hide() { box.hidden = true; },
      };
    },
    // A message in place of the tool (loading, error)
    message(host, text) {
      host.innerHTML = "";
      const p = document.createElement("p");
      p.className = "app-message";
      p.textContent = text;
      host.appendChild(p);
    },
    // Redraw when the box changes size (window, Enlarge view), at most once a frame
    onResize(el, fn) {
      let raf = null, last = 0;
      new ResizeObserver(() => {
        const w = el.clientWidth;
        if (w === last) return;
        last = w;
        cancelAnimationFrame(raf);
        raf = requestAnimationFrame(fn);
      }).observe(el);
    },
  };
})(globalThis);
