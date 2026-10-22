// Tool "explorer": demographic indicators of every country, 1950–2100 (UN World Population Prospects 2024), as the
// old Demo Explorer: a world map (Equal Earth), a table, and the curves of one place; play the years; a place on the
// map or in the table opens its curves, and its pyramid from there.
//
//   {{< app tool="explorer" bundles="countries" presets="ind,y,view,c" >}}
//   data-ind: growth (natural increase), tfr, lex, q5, medage, migr;  data-y: year (2024);
//   data-view: map, table, chart;  data-c: place of the chart (ISO3 or WORLD…)
// Data: /data/wpp/places.json, /data/wpp/ind/<key>.json (data-raw/wpp.R); map: GeoRes.data.countries (countries.js).
(function (G) {
  const R_ = G.GeoRes, pg = R_.page, A = () => R_.app;

  R_.addStrings({
    en: {
      exInd: "Indicator", exYear: "Year", exPlay: "Play", exPause: "Pause", exMap: "Map", exTable: "Table", exChart: "Chart",
      exNoData: "no data", exSearch: "Search", exPlace: "Place", exCsv: "Download (CSV)", exClick: "Click for its curves",
      exPyramid: "Age pyramid of {p} in {y}", exProjection: "projection", exSource: "UN, World Population Prospects 2024",
      exNames: { growth: "Natural increase", tfr: "Fertility", lex: "Life expectancy at birth", q5: "Under-5 mortality",
        medage: "Median age", migr: "Net migration" },
      exUnits: { growth: "% a year", tfr: "children per woman", lex: "years", q5: "‰ (deaths per 1,000 births)",
        medage: "years", migr: "‰ (net migrants per 1,000 inhabitants)" },
      exSeries: { cbr: "Birth rate (‰)", cdr: "Death rate (‰)", pop: "Population", tfr: "Children per woman",
        q5: "Under-5 mortality (‰)", lex: "Both sexes", lexm: "Men", lexf: "Women", medage: "Median age (years)",
        migr: "Net migration (‰)", growth: "Natural increase (% a year)" },
      exMapNote: "Natural Earth borders; French overseas departments are drawn with France.",
    },
    fr: {
      exInd: "Indicateur", exYear: "Année", exPlay: "Lecture", exPause: "Pause", exMap: "Carte", exTable: "Tableau", exChart: "Courbes",
      exNoData: "pas de données", exSearch: "Chercher", exPlace: "Lieu", exCsv: "Télécharger (CSV)", exClick: "Cliquez pour ses courbes",
      exPyramid: "Pyramide des âges : {p} en {y}", exProjection: "projection", exSource: "ONU, World Population Prospects 2024",
      exNames: { growth: "Accroissement naturel", tfr: "Fécondité", lex: "Espérance de vie à la naissance", q5: "Mortalité avant 5 ans",
        medage: "Âge médian", migr: "Solde migratoire" },
      exUnits: { growth: "% par an", tfr: "enfants par femme", lex: "ans", q5: "‰ (décès pour 1000 naissances)",
        medage: "ans", migr: "‰ (migrants nets pour 1000 habitants)" },
      exSeries: { cbr: "Natalité (‰)", cdr: "Mortalité (‰)", pop: "Population", tfr: "Enfants par femme",
        q5: "Mortalité avant 5 ans (‰)", lex: "Ensemble", lexm: "Hommes", lexf: "Femmes", medage: "Âge médian (ans)",
        migr: "Solde migratoire (‰)", growth: "Accroissement naturel (% par an)" },
      exMapNote: "Frontières Natural Earth ; les départements d'outre-mer sont dessinés avec la France.",
    },
  });

  // Classes and colours of the map (ColorBrewer, no blue on land: blue is for water); the curves of each indicator
  const IND = {
    growth: { breaks: [-1, -0.5, 0, 0.5, 1, 2, 3], dec: 2, sign: true,
      colors: ["#542788", "#8073ac", "#b2abd2", "#fee0b6", "#fdb863", "#e08214", "#b35806", "#7f3b08"],
      chart: { left: ["cbr", "cdr"], right: "pop" } },
    tfr: { breaks: [1.5, 2.1, 3, 4, 5, 6], dec: 2,
      colors: ["#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"],
      chart: { left: ["tfr"], right: "q5" } },
    lex: { breaks: [50, 55, 60, 65, 70, 75, 80], dec: 1,
      colors: ["#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#005a32"],
      chart: { left: ["lexf", "lex", "lexm"] } },
    q5: { breaks: [5, 10, 25, 50, 100, 200], dec: 1,
      colors: ["#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#990000"],
      chart: { left: ["q5"], right: "tfr" } },
    medage: { breaks: [20, 25, 30, 35, 40, 45], dec: 1,
      colors: ["#f1eef6", "#d4b9da", "#c994c7", "#df65b0", "#e7298a", "#ce1256", "#91003f"],
      chart: { left: ["medage"] } },
    migr: { breaks: [-5, -2, -0.5, 0.5, 2, 5], dec: 1, sign: true,
      colors: ["#c51b7d", "#e9a3c9", "#fde0ef", "#f7f7f7", "#e6f5d0", "#a1d76a", "#4d9221"],
      chart: { left: ["migr"] } },
  };
  const SERIES_COLOR = { cbr: "#1b9e77", cdr: "#d95f02", tfr: "#d95f02", q5: "#7570b3", medage: "#e7298a", migr: "#66a61e",
    lex: "var(--geores-fg)", lexm: "var(--app-men)", lexf: "var(--app-women)", pop: "var(--geores-muted)" };
  // Natural Earth ids → WPP codes (Åland is counted with Finland, Somaliland with Somalia)
  const ID2CODE = { KOS: "XKX", SDS: "SSD", PSX: "PSE", SAH: "ESH", ALD: "FIN", SOL: "SOM" };
  const VIEWS = ["map", "table", "chart"];

  R_.sims["app-explorer"] = function (container, opts = {}) {
    const L = R_.lang(opts.lang), tr = k => R_.t(L, k), { el, int, big } = A(), esc = R_.lib.esc;
    container.classList.add("app-explorer");
    const state = {
      ind: IND[opts.ind] ? opts.ind : "growth", year: +opts.y || 2024,
      view: VIEWS.includes(opts.view) ? opts.view : "map", c: (opts.c || "").toUpperCase(),
    };
    const head = document.createElement("div");
    head.className = "app-head";
    container.appendChild(head);
    const host = document.createElement("div");
    host.className = "app-svg-host";
    container.appendChild(host);
    const foot = document.createElement("div");
    foot.className = "app-foot";
    container.appendChild(foot);
    A().message(host, tr("appLoading"));
    const tip = A().tip(container);

    const panel = pg.controls(container, `
      <label>${tr("exInd")} <select class="ind">${Object.keys(IND).map(k => `<option value="${k}">${tr("exNames")[k]}</option>`).join("")}</select></label>
      <span class="app-tabs" role="group">${VIEWS.map(v => `<button type="button" data-view="${v}">${tr("ex" + v[0].toUpperCase() + v.slice(1))}</button>`).join("")}</span>
      <label class="app-year">${tr("exYear")} <input type="range" class="year" min="1950" max="2100" step="1"></label>
      <button type="button" class="play" aria-pressed="false">▶ ${tr("exPlay")}</button>
      <span class="readout"><b class="year-out"></b></span>`);
    const $ = s => panel.querySelector(s);
    const indSel = $(".ind"), yearIn = $(".year"), playBtn = $(".play"), yearOut = $(".year-out");

    let places = [], byCode = {}, data = {}, playing = null, mapSvg = null, sort = { col: 1, dir: -1 }, query = "";
    const name = code => (byCode[code] ? byCode[code][L] : code);
    const idx = () => state.year - 1950;
    const load = keys => Promise.all(keys.map(k => (data[k] ? data[k] : A().data(`wpp/ind/${k}.json`).then(d => (data[k] = d)))));
    const fmt = (key, v) => {
      if (v === null || v === undefined) return tr("exNoData");
      if (key === "pop") return big(L, v);
      const d = IND[key] ? IND[key].dec : 1, s = R_.num(L, v, d);
      return (IND[key] && IND[key].sign && v > 0 ? "+" : "") + s;
    };
    const unit = key => (key === "growth" ? " %" : key === "q5" || key === "migr" ? " ‰" : "");
    const classOf = v => { const b = IND[state.ind].breaks; let k = 0; while (k < b.length && v >= b[k]) k++; return k; };

    A().data("wpp/places.json").then(p => {
      places = p.places;
      places.forEach(q => (byCode[q.code] = q));
      if (state.c && !byCode[state.c]) state.c = "";
      update();
    }).catch(err => { console.error(err); A().message(host, tr("appError")); });

    function update() {
      const keys = new Set([state.ind]);
      if (state.view === "chart") { const ch = IND[state.ind].chart; ch.left.forEach(k => keys.add(k)); if (ch.right) keys.add(ch.right); }
      load([...keys]).then(() => { sync(); draw(); }).catch(err => { console.error(err); A().message(host, tr("appError")); });
    }
    function sync() {
      indSel.value = state.ind;
      yearIn.value = state.year;
      yearOut.textContent = state.year;
      panel.querySelectorAll("[data-view]").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.view === state.view)));
      A().setUrl({ ind: state.ind, y: state.year, view: state.view === "map" ? "" : state.view, c: state.c });
    }
    // the files of the current view are loaded (a resize can come before them)
    function ready() {
      const keys = [state.ind];
      if (state.view === "chart") { const ch = IND[state.ind].chart; keys.push(...ch.left); if (ch.right) keys.push(ch.right); }
      return keys.every(k => data[k]);
    }
    function draw() {
      if (!ready()) return;
      tip.hide();
      if (state.view === "map") drawMap();
      else if (state.view === "table") drawTable();
      else drawChart();
    }
    function open(code) { state.c = code; state.view = "chart"; update(); }

    /* ---------- Map ---------- */
    function buildMap() {
      const C = R_.data && R_.data.countries;
      if (!C) return null;
      const F = R_.maps.frame("equalearth", { x: 0, y: 0, w: 1000, h: 500 }, 10);
      let bb = [1e9, 1e9, -1e9, -1e9];
      for (let lon = -170; lon <= 190; lon += 10) for (const lat of [-56, -20, 0, 20, 84]) {
        const [x, y] = F.xy(lon > 180 ? lon - 360 : lon, lat);
        bb = [Math.min(bb[0], x), Math.min(bb[1], y), Math.max(bb[2], x), Math.max(bb[3], y)];
      }
      const svg = el("svg", { viewBox: `${bb[0]} ${bb[1]} ${bb[2] - bb[0]} ${bb[3] - bb[1]}`, class: "app-map", role: "img" });
      el("rect", { x: bb[0], y: bb[1], width: bb[2] - bb[0], height: bb[3] - bb[1], class: "app-sea" }, svg);
      const g = el("g", {}, svg), paths = [];
      C.forEach(c => {
        let d = "";
        c.poly.forEach(poly => poly.forEach(r => {
          for (let i = 0; i < r.length; i += 2) { const [x, y] = F.xy(r[i], r[i + 1]); d += `${i ? "L" : "M"}${x.toFixed(1)},${y.toFixed(1)}`; }
          d += "Z";
        }));
        const code = ID2CODE[c.id] || c.id;
        paths.push({ code, path: el("path", { d, class: "app-land" }, g) });
      });
      const sel = el("path", { class: "app-selected" }, svg);
      svg.addEventListener("pointermove", e => {
        const p = paths.find(q => q.path === e.target);
        if (!p) return tip.hide();
        const v = data[state.ind].values[p.code];
        tip.show(`<div class="geores-tip-head">${esc(name(p.code))} · ${state.year}</div>` +
          `${tr("exNames")[state.ind]} : <b>${fmt(state.ind, v ? v[idx()] : null)}${v && v[idx()] !== null ? unit(state.ind) : ""}</b>` +
          (byCode[p.code] ? `<div class="geores-tip-head">${tr("exClick")}</div>` : ""), e);
      });
      svg.addEventListener("pointerleave", () => tip.hide());
      svg.addEventListener("click", e => { const p = paths.find(q => q.path === e.target); if (p && byCode[p.code]) open(p.code); });
      return { svg, paths, sel };
    }
    function drawMap() {
      if (!mapSvg) mapSvg = buildMap();
      if (!mapSvg) { A().message(host, tr("appError")); return; }
      const I = IND[state.ind], vals = data[state.ind].values;
      mapSvg.paths.forEach(({ code, path }) => {
        const v = vals[code] ? vals[code][idx()] : null;
        // a style, not a fill attribute: the stylesheet's grey of the land would win over an attribute
        if (v === null || v === undefined) { path.style.fill = ""; path.classList.add("app-nodata"); }
        else { path.classList.remove("app-nodata"); path.style.fill = I.colors[classOf(v)]; }
      });
      const s = mapSvg.paths.filter(p => p.code === state.c).map(p => p.path.getAttribute("d")).join("");
      mapSvg.sel.setAttribute("d", s);
      if (host.firstChild !== mapSvg.svg) host.replaceChildren(mapSvg.svg);
      head.innerHTML = `<b>${tr("exNames")[state.ind]}</b> · ${state.year} <span class="app-muted">(${tr("exUnits")[state.ind]})</span>` +
        (state.year > (data[state.ind].estimates) ? ` <span class="app-badge">${tr("exProjection")}</span>` : "");
      // legend
      // class limits with only the decimals they need: 1.5 – 2.1, not 1.50 – 2.10
      const lim = v => (I.sign && v > 0 ? "+" : "") + R_.num(L, v, Number.isInteger(v) ? 0 : Number.isInteger(v * 10) ? 1 : 2);
      const b = I.breaks, lab = k => (k === 0 ? `< ${lim(b[0])}` : k === b.length ? `≥ ${lim(b[k - 1])}` : `${lim(b[k - 1])} – ${lim(b[k])}`);
      foot.innerHTML = `<div class="app-legend">${I.colors.map((c, k) => `<span><i style="background:${c}"></i>${lab(k)}</span>`).join("")}` +
        `<span><i class="app-nodata-swatch"></i>${tr("exNoData")}</span></div>` +
        `<div class="app-note">${tr("exMapNote")} ${tr("exSource")}.</div>`;
    }

    /* ---------- Table ---------- */
    const COLS = () => [state.year, 1950, 2000, 2050, 2100];
    function drawTable() {
      const vals = data[state.ind].values, cols = COLS();
      let rows = places.map(q => ({ q, v: cols.map(y => (vals[q.code] ? vals[q.code][y - 1950] : null)) }));
      if (query) { const s = query.toLowerCase(); rows = rows.filter(r => r.q[L].toLowerCase().includes(s) || r.q.code.toLowerCase() === s); }
      rows.sort((a, b) => {
        if (sort.col === 0) return sort.dir * a.q[L].localeCompare(b.q[L], L);
        const x = a.v[sort.col - 1], y = b.v[sort.col - 1];
        if (x === null) return 1;
        if (y === null) return -1;
        return sort.dir * (x - y);
      });
      const arrow = k => (sort.col === k ? (sort.dir > 0 ? " ▲" : " ▼") : "");
      head.innerHTML = `<b>${tr("exNames")[state.ind]}</b> <span class="app-muted">(${tr("exUnits")[state.ind]})</span>`;
      host.innerHTML = `<div class="app-table-tools"><input type="search" class="app-search" placeholder="${tr("exSearch")}" value="${esc(query)}">` +
        `<button type="button" class="app-csv">${tr("exCsv")}</button></div>` +
        `<div class="app-table-wrap"><table class="app-table"><thead><tr><th data-col="0">${tr("exPlace")}${arrow(0)}</th>` +
        cols.map((y, k) => `<th data-col="${k + 1}" class="num${k ? "" : " app-current"}">${y}${arrow(k + 1)}</th>`).join("") + `</tr></thead><tbody>` +
        rows.map(r => `<tr data-code="${r.q.code}" class="${r.q.kind === "region" ? "app-region" : ""}"><td>${esc(r.q[L])}</td>` +
          r.v.map((v, k) => `<td class="num${k ? "" : " app-current"}">${fmt(state.ind, v)}</td>`).join("") + `</tr>`).join("") +
        `</tbody></table></div>`;
      foot.innerHTML = `<div class="app-note">${tr("exSource")}.</div>`;
      const search = host.querySelector(".app-search");
      search.addEventListener("input", () => { query = search.value; drawTable(); host.querySelector(".app-search").focus(); });
      host.querySelectorAll("th").forEach(th => th.addEventListener("click", () => {
        const c = +th.dataset.col;
        sort = { col: c, dir: sort.col === c ? -sort.dir : (c ? -1 : 1) };
        drawTable();
      }));
      host.querySelectorAll("tbody tr").forEach(tr_ => tr_.addEventListener("click", () => open(tr_.dataset.code)));
      host.querySelector(".app-csv").addEventListener("click", csv);
    }
    function csv() {
      const vals = data[state.ind].values, years = Array.from({ length: 151 }, (_, i) => 1950 + i);
      const lines = [["code", "place", ...years].join(",")].concat(places.map(q =>
        [q.code, `"${q[L].replace(/"/g, '""')}"`, ...(vals[q.code] || []).map(v => (v === null ? "" : v))].join(",")));
      const a = document.createElement("a");
      a.href = URL.createObjectURL(new Blob([lines.join("\n")], { type: "text/csv" }));
      a.download = `wpp2024-${state.ind}.csv`;
      a.click();
      setTimeout(() => URL.revokeObjectURL(a.href), 1000);
    }

    /* ---------- Chart ---------- */
    function nice(min, max, n) {
      const raw = (max - min) / n, p = Math.pow(10, Math.floor(Math.log10(raw || 1)));
      const st = [1, 2, 2.5, 5, 10].map(k => k * p).find(s => s >= raw);
      return { lo: Math.floor(min / st) * st, hi: Math.ceil(max / st) * st, st };
    }
    function drawChart() {
      const code = state.c || "WORLD", ch = IND[state.ind].chart;
      const W = Math.max(300, host.clientWidth), small = W < 560, fs = small ? 12 : 14;
      const expanded = container.classList.contains("geores-expanded");
      const H = Math.round(Math.max(300, Math.min(expanded ? innerHeight - panel.offsetHeight - 200 : innerHeight * 0.6, W * 0.55)));
      const m = { l: small ? 38 : 48, r: ch.right ? (small ? 46 : 60) : 22, t: 14, b: 30 };
      const X = y => m.l + (y - 1950) / 150 * (W - m.l - m.r);
      const series = k => (data[k].values[code] || []);
      const range = keys => {
        const all = keys.flatMap(k => series(k)).filter(v => v !== null);
        return nice(Math.min(0, ...all), Math.max(...all), small ? 4 : 5);
      };
      const ly = range(ch.left), ry = ch.right ? range([ch.right]) : null;
      const Y = (v, r) => m.t + (r.hi - v) / (r.hi - r.lo) * (H - m.t - m.b);
      const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, width: W, height: H, class: "app-svg app-chart", role: "img" });
      svg.style.fontSize = `${fs}px`;
      // projection
      const est = data[state.ind].estimates;
      el("rect", { x: X(est + 0.5), y: m.t, width: X(2100) - X(est + 0.5), height: H - m.t - m.b, class: "app-proj" }, svg);
      el("text", { x: X(est + 0.5) + 4, y: m.t + fs, class: "app-muted app-small" }, svg).textContent = tr("exProjection");
      // grid and axes
      for (let v = ly.lo; v <= ly.hi + 1e-9; v += ly.st) {
        el("line", { x1: m.l, x2: W - m.r, y1: Y(v, ly), y2: Y(v, ly), class: v === 0 ? "app-axis" : "app-grid" }, svg);
        el("text", { x: m.l - 6, y: Y(v, ly) + fs * 0.35, "text-anchor": "end", class: "app-muted" }, svg).textContent = R_.num(L, v, ly.st < 1 ? 1 : 0);
      }
      if (ry) for (let v = ry.lo; v <= ry.hi + 1e-9; v += ry.st) {
        const t = ch.right === "pop" ? (v === 0 ? "0" : v >= 1e6 ? `${R_.num(L, v / 1e6, v % 1e6 ? 1 : 0)} M` : `${R_.num(L, v / 1e3, 0)} k`) : R_.num(L, v, ry.st < 1 ? 1 : 0);
        el("text", { x: W - m.r + 6, y: Y(v, ry) + fs * 0.35, class: "app-muted" }, svg).textContent = t;
      }
      for (let y = 1950; y <= 2100; y += small ? 50 : 25) {
        el("text", { x: X(y), y: H - 8, "text-anchor": "middle", class: "app-muted" }, svg).textContent = y;
      }
      // the selected year
      el("line", { x1: X(state.year), x2: X(state.year), y1: m.t, y2: H - m.b, class: "app-marker" }, svg);
      // curves
      const line = (k, r, dashed) => {
        let d = "";
        series(k).forEach((v, i) => { if (v !== null) d += `${d && series(k)[i - 1] !== null ? "L" : "M"}${X(1950 + i).toFixed(1)},${Y(v, r).toFixed(1)}`; });
        el("path", { d, class: "app-line" + (dashed ? " app-dashed" : ""), style: `stroke:${SERIES_COLOR[k]}` }, svg);
      };
      if (ry) line(ch.right, ry, true);
      ch.left.forEach(k => line(k, ly, false));
      // hover: the values of a year
      const overlay = el("rect", { x: m.l, y: m.t, width: W - m.l - m.r, height: H - m.t - m.b, fill: "transparent" }, svg);
      const keys = ch.left.concat(ch.right ? [ch.right] : []);
      overlay.addEventListener("pointermove", e => {
        const b = svg.getBoundingClientRect(), x = (e.clientX - b.left) * (W / b.width);
        const yr = Math.round(1950 + (x - m.l) / (W - m.l - m.r) * 150);
        if (yr < 1950 || yr > 2100) return tip.hide();
        tip.show(`<div class="geores-tip-head">${esc(name(code))} · ${yr}</div>` + keys.map(k =>
          `<span style="color:${SERIES_COLOR[k]}">■</span> ${tr("exSeries")[k]} ${fmt(k, series(k)[yr - 1950])}`).join("<br>"), e);
      });
      overlay.addEventListener("pointerleave", () => tip.hide());
      overlay.addEventListener("click", e => {
        const b = svg.getBoundingClientRect(), x = (e.clientX - b.left) * (W / b.width);
        state.year = Math.min(2100, Math.max(1950, Math.round(1950 + (x - m.l) / (W - m.l - m.r) * 150)));
        sync(); drawChart();
      });
      host.replaceChildren(svg);
      head.innerHTML = `<b>${esc(name(code))}</b> · ${tr("exNames")[state.ind]}`;
      const pyr = `pyramids.html?c=${code}&y=${state.year}`;
      foot.innerHTML = `<div class="app-legend">${keys.map(k => `<span><i class="app-line-swatch${k === ch.right ? " app-dashed" : ""}" style="border-color:${SERIES_COLOR[k]}"></i>` +
        `${tr("exSeries")[k]} <b>${fmt(k, series(k)[idx()])}</b></span>`).join("")}</div>` +
        `<div class="app-note"><a href="${pyr}">${tr("exPyramid").replace("{p}", esc(name(code))).replace("{y}", state.year)} →</a> · ${tr("exSource")}.</div>`;
    }

    /* ---------- Controls ---------- */
    function stop() {
      clearInterval(playing);
      playing = null;
      playBtn.textContent = `▶ ${tr("exPlay")}`;
      playBtn.setAttribute("aria-pressed", "false");
      sync();
    }
    playBtn.addEventListener("click", () => {
      if (playing) return stop();
      if (state.year >= 2100) state.year = 1950;
      playBtn.textContent = `❚❚ ${tr("exPause")}`;
      playBtn.setAttribute("aria-pressed", "true");
      playing = setInterval(() => {
        if (state.year >= 2100) return stop();
        state.year++;
        yearIn.value = yearOut.textContent = state.year;
        draw();
      }, 180);
    });
    yearIn.addEventListener("input", () => { state.year = +yearIn.value; yearOut.textContent = state.year; draw(); });
    yearIn.addEventListener("change", sync);
    indSel.addEventListener("change", () => { state.ind = indSel.value; update(); });
    panel.querySelectorAll("[data-view]").forEach(b => b.addEventListener("click", () => { state.view = b.dataset.view; update(); }));
    A().onResize(host, () => { if (state.view === "chart") draw(); });
  };
})(globalThis);
