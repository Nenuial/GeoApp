// Tool "climates": the climates of 4 700 weather stations (WMO normals 1991–2020, plus the curated stations of
// GeoResources) on a world map, coloured by their Köppen climate; a station from the map or the search box gives its
// climogram (GeoRes.climate.draw); a second station to compare; a quiz that hides the names and the codes.
//
//   {{< app tool="climates" bundles="climate,climate-wmo" presets="s,c,quiz" >}}
//   data-s: station id (ch-ber, wmo-06660000…), default ch-ber;  data-c: a second station;  data-quiz="1": quiz
// Data: GeoRes.climate (dist/climate.js, dist/climate-wmo.js); Köppen: GeoRes.koppen; land: GeoRes.world.land.
(function (G) {
  const R_ = G.GeoRes, pg = R_.page, A = () => R_.app;

  R_.addStrings({
    en: {
      clSearch: "Station", clSearchHint: "Search a station or a country", clCompare: "Compare", clQuiz: "Quiz",
      clReveal: "Show the answer", clHide: "Hide", clPick: "Click a station on the map, or search it by name.",
      clCount: "{n} stations", clSecond: "Next click: the second station", clQuizHint: "Which climate? Where could it be?",
      clSource: "WMO Climatological Standard Normals 1991–2020 (NCEI); MeteoSwiss normals 1991–2020",
    },
    fr: {
      clSearch: "Station", clSearchHint: "Chercher une station ou un pays", clCompare: "Comparer", clQuiz: "Quiz",
      clReveal: "Voir la réponse", clHide: "Cacher", clPick: "Cliquez sur une station de la carte, ou cherchez-la par son nom.",
      clCount: "{n} stations", clSecond: "Prochain clic : la seconde station", clQuizHint: "Quel climat ? Où pourrait-ce être ?",
      clSource: "Normales climatologiques standard 1991–2020 de l'OMM (NCEI) ; normales 1991–2020 de MétéoSuisse",
    },
  });

  R_.sims["app-climates"] = function (container, opts = {}) {
    const L = R_.lang(opts.lang), tr = k => R_.t(L, k), { el } = A(), esc = R_.lib.esc, CL = R_.climate, K = R_.koppen;
    container.classList.add("app-climates");
    if (!CL || !CL.all().length) { A().message(container, tr("appError")); return; }
    const stations = CL.all().filter(s => s.lat !== undefined && s.lon !== undefined && s.T && s.P);
    const byId = new Map(stations.map(s => [s.id, s]));
    const code = s => (s._k || (s._k = K.classify(s.T, s.P).code));
    const label = s => `${CL.nameOf(s, L)} (${CL.countryOf ? CL.countryOf(s, L) : ""})`;

    const state = {
      s: byId.has(opts.s) ? opts.s : "ch-ber", c: byId.has(opts.c) ? opts.c : "",
      quiz: opts.quiz === "1", reveal: false, picking: false,
    };

    // Map
    const mapBox = document.createElement("div");
    mapBox.className = "app-svg-host";
    container.appendChild(mapBox);
    const F = R_.maps.frame("equalearth", { x: 0, y: 0, w: 1000, h: 500 }, 10);
    let bb = [1e9, 1e9, -1e9, -1e9];
    for (let lon = -170; lon <= 190; lon += 10) for (const lat of [-56, -20, 0, 20, 80]) {
      const [x, y] = F.xy(lon > 180 ? lon - 360 : lon, lat);
      bb = [Math.min(bb[0], x), Math.min(bb[1], y), Math.max(bb[2], x), Math.max(bb[3], y)];
    }
    const map = el("svg", { viewBox: `${bb[0]} ${bb[1]} ${bb[2] - bb[0]} ${bb[3] - bb[1]}`, class: "app-map", role: "img" }, mapBox);
    el("rect", { x: bb[0], y: bb[1], width: bb[2] - bb[0], height: bb[3] - bb[1], class: "app-sea" }, map);
    let land = "";
    (R_.world.land || []).forEach(poly => poly.forEach(r => {
      for (let i = 0; i < r.length; i += 2) { const [x, y] = F.xy(r[i], r[i + 1]); land += `${i ? "L" : "M"}${x.toFixed(1)},${y.toFixed(1)}`; }
      land += "Z";
    }));
    el("path", { d: land, class: "app-land app-land-plain" }, map);
    const dots = el("g", { class: "app-dots" }, map);
    stations.forEach(s => {
      const [x, y] = F.xy(s.lon, s.lat);
      s._x = x; s._y = y;
      el("circle", { cx: x.toFixed(1), cy: y.toFixed(1), r: 1.6, fill: K.color(code(s)), "data-id": s.id }, dots);
    });
    const ring = el("g", { class: "app-rings" }, map);
    const tip = A().tip(container);
    const nearest = e => {
      const pt = map.createSVGPoint();
      pt.x = e.clientX; pt.y = e.clientY;
      const p = pt.matrixTransform(map.getScreenCTM().inverse());
      const scale = (bb[2] - bb[0]) / map.getBoundingClientRect().width, max = 14 * scale;   // within 14 pixels
      let best = null, d2 = max * max;
      stations.forEach(s => { const d = (s._x - p.x) ** 2 + (s._y - p.y) ** 2; if (d < d2) { d2 = d; best = s; } });
      return best;
    };
    map.addEventListener("pointermove", e => {
      const s = nearest(e);
      if (!s) return tip.hide();
      tip.show(state.quiz && !state.reveal ? `<div class="geores-tip-head">${tr("clQuizHint")}</div>` :
        `<b>${esc(CL.nameOf(s, L))}</b> · ${esc(CL.countryOf(s, L))}<div class="geores-tip-head">${code(s)} · ${esc(K.name(L, code(s)))}</div>`, e);
    });
    map.addEventListener("pointerleave", () => tip.hide());
    map.addEventListener("click", e => {
      const s = nearest(e);
      if (!s) return;
      if (state.picking) { state.c = s.id; state.picking = false; }
      else state.s = s.id;
      state.reveal = false;
      renderAll();
    });

    // Controls
    const all = stations.slice().sort((a, b) => CL.nameOf(a, L).localeCompare(CL.nameOf(b, L), L));
    const listId = `app-cl-${Math.random().toString(36).slice(2, 8)}`;
    const panel = pg.controls(container, `
      <label>${tr("clSearch")} <input type="search" class="find" list="${listId}" placeholder="${tr("clSearchHint")}"></label>
      <datalist id="${listId}">${all.map(s => `<option value="${esc(label(s))}"></option>`).join("")}</datalist>
      <button type="button" class="cmp" aria-pressed="false">${tr("clCompare")}</button>
      <button type="button" class="quiz" aria-pressed="false">${tr("clQuiz")}</button>
      <button type="button" class="reveal" hidden>${tr("clReveal")}</button>
      <span class="readout hint"></span>`);
    const $ = s => panel.querySelector(s);
    const find = $(".find"), cmpBtn = $(".cmp"), quizBtn = $(".quiz"), revealBtn = $(".reveal"), hint = $(".hint");
    const byLabel = new Map(all.map(s => [label(s), s]));
    find.addEventListener("change", () => {
      const s = byLabel.get(find.value);
      if (!s) return;
      if (state.picking) { state.c = s.id; state.picking = false; } else state.s = s.id;
      find.value = "";
      state.reveal = false;
      renderAll();
    });
    cmpBtn.addEventListener("click", () => {
      if (state.c) { state.c = ""; state.picking = false; }
      else state.picking = !state.picking;
      renderAll();
    });
    quizBtn.addEventListener("click", () => { state.quiz = !state.quiz; state.reveal = false; renderAll(); });
    revealBtn.addEventListener("click", () => { state.reveal = !state.reveal; renderAll(); });

    // Climograms
    const chart = document.createElement("div");
    chart.className = "app-climogram";
    container.appendChild(chart);
    const foot = document.createElement("div");
    foot.className = "app-note";
    container.appendChild(foot);

    function render() {
      tip.hide();
      const hidden = state.quiz && !state.reveal;
      cmpBtn.setAttribute("aria-pressed", String(!!state.c || state.picking));
      quizBtn.setAttribute("aria-pressed", String(state.quiz));
      revealBtn.hidden = !state.quiz;
      revealBtn.textContent = state.reveal ? tr("clHide") : tr("clReveal");
      hint.textContent = state.picking ? tr("clSecond") : hidden ? tr("clQuizHint") : tr("clCount").replace("{n}", R_.app.int(L, stations.length));
      // rings on the map around the chosen stations (not in a quiz, which would give the place away)
      ring.replaceChildren();
      if (!hidden) [state.s, state.c].filter(Boolean).forEach(id => {
        const s = byId.get(id);
        el("circle", { cx: s._x, cy: s._y, r: 6, class: "app-ring" }, ring);
      });
      dots.classList.toggle("app-quiet", hidden);
      // the climograms, drawn by GeoResources (the same drawing as the climogram sim)
      const th = R_.theme(container.classList.contains("geores-dark") ? "dark" : "light");
      const ids = [state.s].concat(state.c ? [state.c] : []);
      const list = ids.map((id, i) => {
        const s = CL.input(id, L);
        return hidden ? Object.assign({}, s, { name: String.fromCharCode(65 + i), place: "", el: "", lat: undefined, source: "" }) : s;
      });
      const W = 1280, H = 560, w = W / list.length;
      chart.innerHTML = R_.lib.svgDoc(`0 0 ${W} ${H}`, list.map((s, i) =>
        CL.draw(s, { x: i * w + 10, y: 10, w: w - 24, h: H - 30 }, th, L, { key: !hidden })).join(""), th);
      foot.textContent = `${tr("clSource")}.`;
      A().setUrl({ s: state.s, c: state.c, quiz: state.quiz ? "1" : "" });
    }
    // dots of about 2.5 pixels whatever the width of the map (the radius is in map units)
    const size = () => {
      const scale = (bb[2] - bb[0]) / Math.max(1, map.getBoundingClientRect().width);
      dots.querySelectorAll("circle").forEach(c => c.setAttribute("r", (2.5 * scale).toFixed(2)));
      ring.querySelectorAll("circle").forEach(c => c.setAttribute("r", (8 * scale).toFixed(2)));
    };
    const renderAll = () => { render(); size(); };
    renderAll();
    A().onResize(mapBox, size);
    pg.onThemeChange(renderAll);
  };
})(globalThis);
