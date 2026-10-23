// Tool "pyramids": the age pyramid of any country, the World or a continent, 1950–2100 (UN World Population
// Prospects 2024; projections from 2024, medium variant). One bar per year of age (0–99, 100+), or 5-year groups in %
// of the population; the years of birth beside the ages; play the years; compare with the outline of another year
// or another place (another place at the same total population, so that the shapes compare). A click on a bar follows
// that generation (its bars in the accent colour) while the years change; a click on it again stops. As GeoResources'
// russia-pyramid (geokey): same mark, its strings rpyClick and rpyCohort.
//
//   {{< app tool="pyramids" presets="c,y,rel,cmp,b" >}}
//   data-c: place (ISO3 code, or WORLD AFRICA ASIA EUROPE LATAM NORTHAM OCEANIA), default CHE;  data-y: year (2024);
//   data-rel="1": 5-year groups in %;  data-cmp: outline of a year ("1950") or of a place ("IND");
//   data-b: the year of birth of a generation followed ("1946")
// Data: /data/wpp/places.json, /data/wpp/pyramids/<code>.json (data-raw/wpp.R).
(function (G) {
  const R_ = G.GeoRes, pg = R_.page, A = () => R_.app;

  R_.addStrings({
    en: {
      pyPlace: "Place", pyYear: "Year", pyPlay: "Play", pyPause: "Pause", pyRel: "5-year groups (%)",
      pyCompare: "Compare with", pyNone: "—", pyYears: "the same place in", pyPlaces: "another place, same year",
      pyRegions: "World and continents", pyCountries: "Countries and areas", pyMen: "Men", pyWomen: "Women",
      pyAge: "age", pyBorn: "born", pyBornIn: "born in", pyMedian: "median age", pyProjection: "projection",
      pyScaled: "outline: {p} {y}, at the same total population", pyOutline: "outline: {p} {y}",
      pyInhab: "inhabitants", pySource: "UN, World Population Prospects 2024", pyOfPop: "of the population",
      pyUnfollow: "click: stop following",
      pyAged: "aged {a}", pyUnborn: "not born yet",
    },
    fr: {
      pyPlace: "Lieu", pyYear: "Année", pyPlay: "Lecture", pyPause: "Pause", pyRel: "Groupes de 5 ans (%)",
      pyCompare: "Comparer avec", pyNone: "—", pyYears: "le même lieu en", pyPlaces: "un autre lieu, même année",
      pyRegions: "Monde et continents", pyCountries: "Pays et territoires", pyMen: "Hommes", pyWomen: "Femmes",
      pyAge: "âge", pyBorn: "nés en", pyBornIn: "nés en", pyMedian: "âge médian", pyProjection: "projection",
      pyScaled: "contour : {p} {y}, à population totale égale", pyOutline: "contour : {p} {y}",
      pyInhab: "habitants", pySource: "ONU, World Population Prospects 2024", pyOfPop: "de la population",
      pyUnfollow: "clic : ne plus la suivre",
      pyAged: "{a} ans", pyUnborn: "pas encore nés",
    },
  });

  const COMPARE_YEARS = [1950, 1975, 2000, 2024, 2050, 2075, 2100];

  R_.sims["app-pyramids"] = function (container, opts = {}) {
    const L = R_.lang(opts.lang), tr = k => R_.t(L, k), { el, int, big } = A();
    container.classList.add("app-pyramids");
    const host = document.createElement("div");
    host.className = "app-svg-host";
    container.appendChild(host);
    A().message(host, tr("appLoading"));

    const state = {
      code: (opts.c || "CHE").toUpperCase(), year: +opts.y || 2024, rel: opts.rel === "1",
      cmp: (opts.cmp || "").toUpperCase(), born: /^\d{4}$/.test(opts.b || "") ? +opts.b : null,
    };
    let places = [], byCode = {}, pyr = null, cmpPyr = null, playing = null, hover = null, meta = null, geom = null;
    const tip = A().tip(container);

    const panel = pg.controls(container, `
      <label>${tr("pyPlace")} <select class="place"></select></label>
      <label class="app-year">${tr("pyYear")} <input type="range" class="year" min="1950" max="2100" step="1"></label>
      <button type="button" class="play" aria-pressed="false">▶ ${tr("pyPlay")}</button>
      <span class="readout"><b class="year-out"></b></span>
      <label><input type="checkbox" class="rel"> ${tr("pyRel")}</label>
      <label>${tr("pyCompare")} <select class="cmp"></select></label>`);
    const $ = s => panel.querySelector(s);
    const placeSel = $(".place"), yearIn = $(".year"), playBtn = $(".play"), yearOut = $(".year-out"),
      relIn = $(".rel"), cmpSel = $(".cmp");

    const name = code => (byCode[code] ? byCode[code][L] : code);

    A().data("wpp/places.json").then(p => {
      meta = p;
      places = p.places;
      places.forEach(q => (byCode[q.code] = q));
      if (!byCode[state.code]) state.code = "CHE";
      const sorted = kind => places.filter(q => q.kind === kind)
        .sort((a, b) => (kind === "region" ? 0 : a[L].localeCompare(b[L], L)));
      const opt = q => `<option value="${q.code}">${R_.lib.esc(q[L])}</option>`;
      const regions = sorted("region").map(opt).join(""), countries = sorted("country").map(opt).join("");
      placeSel.innerHTML = `<optgroup label="${tr("pyRegions")}">${regions}</optgroup>` +
        `<optgroup label="${tr("pyCountries")}">${countries}</optgroup>`;
      cmpSel.innerHTML = `<option value="">${tr("pyNone")}</option>` +
        `<optgroup label="${tr("pyYears")}">${COMPARE_YEARS.map(y => `<option value="${y}">${y}</option>`).join("")}</optgroup>` +
        `<optgroup label="${tr("pyPlaces")} · ${tr("pyRegions")}">${regions}</optgroup>` +
        `<optgroup label="${tr("pyPlaces")} · ${tr("pyCountries")}">${countries}</optgroup>`;
      return load();
    }).catch(err => { console.error(err); A().message(host, tr("appError")); });

    function load() {
      const files = [A().data(`wpp/pyramids/${state.code}.json`)];
      const cmpPlace = state.cmp && !/^\d+$/.test(state.cmp) && byCode[state.cmp] ? state.cmp : null;
      if (!cmpPlace && !/^\d+$/.test(state.cmp)) state.cmp = "";
      files.push(cmpPlace ? A().data(`wpp/pyramids/${cmpPlace}.json`) : Promise.resolve(null));
      return Promise.all(files).then(([a, b]) => {
        pyr = a;
        cmpPyr = b;
        state.year = Math.min(pyr.last, Math.max(pyr.first, state.year));
        sync();
        draw();
      }).catch(err => { console.error(err); A().message(host, tr("appError")); });
    }

    // Controls and address from the state
    function sync() {
      placeSel.value = state.code;
      yearIn.value = state.year;
      yearOut.textContent = state.year;
      relIn.checked = state.rel;
      cmpSel.value = state.cmp;
      A().setUrl({ c: state.code, y: state.year, rel: state.rel ? "1" : "", cmp: state.cmp, b: state.born ?? "" });
    }

    // Men and women of a year: single years (101 values) or 5-year groups in % (21 values: 0–4 … 95–99, 100+)
    function rows(p, year) {
      const i = year - p.first, m = p.m[i], f = p.f[i];
      const total = m.reduce((s, v) => s + v, 0) + f.reduce((s, v) => s + v, 0);
      if (!state.rel) return { m, f, total };
      const g = a => Array.from({ length: 21 }, (_, k) => a.slice(k * 5, k === 20 ? 101 : k * 5 + 5)
        .reduce((s, v) => s + v, 0) / total * 100);
      return { m: g(m), f: g(f), total };
    }
    // The largest bar over every year of a place: the axis stays fixed while the years play
    const maxCache = new Map();
    function maxOf(p) {
      const key = `${p.code}|${state.rel}`;
      if (!maxCache.has(key)) {
        let mx = 0;
        for (let y = p.first; y <= p.last; y++) {
          const r = rows(p, y);
          mx = Math.max(mx, ...r.m, ...r.f);
        }
        maxCache.set(key, mx);
      }
      return maxCache.get(key);
    }
    function medianAge(p, year) {
      const i = year - p.first, all = p.m[i].map((v, a) => v + p.f[i][a]);
      const half = all.reduce((s, v) => s + v, 0) / 2;
      let acc = 0;
      for (let a = 0; a < all.length; a++) {
        if (acc + all[a] >= half) return a + (half - acc) / all[a];
        acc += all[a];
      }
      return 100;
    }
    // Row of the generation followed this year (null before its birth); its age
    function cohortRow(n) {
      if (state.born === null || state.year < state.born) return null;
      const age = state.year - state.born;
      return state.rel ? Math.min(20, Math.floor(age / 5)) : Math.min(100, age);
    }
    // Year of birth followed after a click on row k (5-year groups: the middle of the group)
    const bornOf = k => state.year - (state.rel ? Math.min(100, k * 5 + 2) : k);

    // Round axis steps: 1, 2, 2.5 or 5 × 10^n, about 3 ticks a side (2 on a phone)
    function step(max, ticks) {
      const raw = max / ticks, p = Math.pow(10, Math.floor(Math.log10(raw)));
      return [1, 2, 2.5, 5, 10].map(k => k * p).find(s => s >= raw);
    }

    function draw() {
      if (!pyr) return;
      tip.hide();
      const W = Math.max(300, host.clientWidth);
      const expanded = container.classList.contains("geores-expanded");
      const avail = expanded ? innerHeight - panel.offsetHeight - 110 : Math.min(innerHeight * 0.75, 760);
      const H = Math.round(Math.max(380, Math.min(avail, W * 0.78)));
      const small = W < 560;
      const fs = small ? 12 : 14;                     // text size in pixels, whatever the width
      const top = small ? 62 : 72, bottom = small ? 62 : 44, mid = small ? 30 : 38, right = small ? 40 : 52, left = 6;
      const half = (W - left - right - mid) / 2, x0 = left + half, x1 = x0 + mid;  // men end at x0, women start at x1
      const plotH = H - top - bottom;

      const r = rows(pyr, state.year), n = r.m.length, rowH = plotH / n;
      let max = maxOf(pyr);
      // outline: another year of the same place, or another place scaled to the same total population
      let cr = null, cmpLabel = "";
      if (/^\d+$/.test(state.cmp)) {
        cr = rows(pyr, +state.cmp);
        if (!state.rel) max = Math.max(max, ...cr.m, ...cr.f);
        cmpLabel = tr("pyOutline").replace("{p}", name(state.code)).replace("{y}", state.cmp);
      } else if (cmpPyr) {
        cr = rows(cmpPyr, Math.min(cmpPyr.last, Math.max(cmpPyr.first, state.year)));
        if (!state.rel) {
          const k = r.total / cr.total;
          cr = { m: cr.m.map(v => v * k), f: cr.f.map(v => v * k), total: r.total };
          max = Math.max(max, ...cr.m, ...cr.f);
          cmpLabel = tr("pyScaled").replace("{p}", name(state.cmp)).replace("{y}", state.year);
        } else {
          max = Math.max(max, maxOf(cmpPyr));
          cmpLabel = tr("pyOutline").replace("{p}", name(state.cmp)).replace("{y}", state.year);
        }
      }
      const sx = half / max;
      const yOf = k => top + plotH - (k + 1) * rowH;   // row k from the bottom (age 0 at the bottom)
      const proj = state.year > pyr.estimates;

      const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, width: W, height: H, class: "app-svg", role: "img",
        "aria-label": `${name(state.code)} ${state.year}` });
      svg.style.fontSize = `${fs}px`;

      // Title: place and year; subtitle: population and median age
      el("text", { x: left, y: small ? 22 : 26, class: "app-title" }, svg).textContent = `${name(state.code)} · ${state.year}`;
      const sub = `${big(L, r.total)} ${tr("pyInhab")} · ${tr("pyMedian")} ${R_.num(L, medianAge(pyr, state.year), 1)}`;
      el("text", { x: left, y: small ? 42 : 48, class: "app-muted" }, svg).textContent = sub;
      if (proj) {
        const t = el("text", { x: W - 4, y: small ? 22 : 26, "text-anchor": "end", class: "app-badge" }, svg);
        t.textContent = `${tr("pyProjection")}`;
      }
      // The generation followed: its year of birth and age (wide screens; on phones its row labels say it)
      const ck = cohortRow(n);
      if (state.born !== null && !small) {
        const age = state.year - state.born;
        el("text", { x: W - 4, y: 48, "text-anchor": "end", class: "app-cohort-text" }, svg).textContent =
          `${tr("rpyCohort").replace("{y}", state.born)} · ${age < 0 ? tr("pyUnborn") : tr("pyAged").replace("{a}", age)}`;
      }
      el("text", { x: x0 - 4, y: top - 8, "text-anchor": "end", class: "app-men-text" }, svg).textContent = tr("pyMen");
      el("text", { x: x1 + 4, y: top - 8, class: "app-women-text" }, svg).textContent = tr("pyWomen");

      // Grid and the axis of numbers
      // axis numbers kept short: 1.5 M, 500 k
      const short = v => (v >= 1e6 ? `${R_.num(L, v / 1e6, v % 1e6 ? 1 : 0)} M` : v >= 1e4 ? `${R_.num(L, v / 1e3, 0)} k` : int(L, v));
      const st = step(max, small ? 2 : 3), fmt = v => (state.rel ? `${R_.num(L, v, st < 1 ? 1 : 0)} %` : short(v));
      for (let v = st; v <= max * 1.0001; v += st) {
        [x0 - v * sx, x1 + v * sx].forEach(x => {
          el("line", { x1: x, x2: x, y1: top, y2: top + plotH, class: "app-grid" }, svg);
          el("text", { x, y: top + plotH + fs + 6, "text-anchor": "middle", class: "app-muted" }, svg).textContent = fmt(v);
        });
      }

      // Bars
      const gap = rowH > 4 ? Math.min(1, rowH * 0.15) : 0;
      const bars = el("g", { class: proj ? "app-projected" : null }, svg);
      for (let k = 0; k < n; k++) {
        const y = yOf(k) + gap / 2, h = Math.max(0.5, rowH - gap);
        const c = k === ck ? " app-cohort" : "";
        el("rect", { x: x0 - r.m[k] * sx, y, width: r.m[k] * sx, height: h, class: "app-men" + c }, bars);
        el("rect", { x: x1, y, width: r.f[k] * sx, height: h, class: "app-women" + c }, bars);
      }

      // Outline of the comparison
      if (cr) {
        const path = (vals, side) => {
          let d = "";
          vals.forEach((v, k) => {
            const x = side < 0 ? x0 - v * sx : x1 + v * sx, yb = yOf(k) + rowH, yt = yOf(k);
            d += `${k ? "L" : "M"}${x.toFixed(1)},${yb.toFixed(1)}L${x.toFixed(1)},${yt.toFixed(1)}`;
          });
          return d;
        };
        el("path", { d: path(cr.m, -1), class: "app-outline" }, svg);
        el("path", { d: path(cr.f, 1), class: "app-outline" }, svg);
        el("text", { x: left, y: small ? H - 26 : H - 8, class: "app-muted" }, svg).textContent = `— ${cmpLabel}`;
      }

      // Ages in the middle, years of birth on the right (single years only); the row of the generation followed in
      // the accent colour, in place of the labels it would overlap
      const ageStep = state.rel ? 2 : 10;   // in rows
      const label = (k, cls) => {
        const age = state.rel ? k * 5 : k, y = yOf(k) + rowH / 2 + fs * 0.35;
        el("text", { x: (x0 + x1) / 2, y, "text-anchor": "middle", class: cls }, svg).textContent = age === 100 && k === ck ? "100+" : age;
        if (!state.rel) el("text", { x: W - 2, y, "text-anchor": "end", class: cls }, svg).textContent = k === ck ? state.born : state.year - age;
      };
      for (let k = 0; k < n; k += ageStep) if (ck === null || Math.abs(k - ck) * rowH >= fs * 1.1) label(k, "app-muted");
      if (ck !== null) label(ck, "app-cohort-text");
      el("text", { x: (x0 + x1) / 2, y: top - 8, "text-anchor": "middle", class: "app-muted app-small" }, svg).textContent = tr("pyAge");
      if (!state.rel) el("text", { x: W - 2, y: top - 8, "text-anchor": "end", class: "app-muted app-small" }, svg).textContent = tr("pyBorn");
      el("text", { x: W - 2, y: H - 8, "text-anchor": "end", class: "app-muted app-small" }, svg).textContent = tr("pySource");

      // Hover (or tap): the row under the pointer
      if (hover !== null && hover < n) {
        el("rect", { x: left, y: yOf(hover), width: W - left - right + 4, height: rowH, class: "app-hover" }, svg);
      }
      const overlay = el("rect", { x: left, y: top, width: W - left, height: plotH, fill: "transparent" }, svg);
      const rowAt = e => {
        const b = svg.getBoundingClientRect(), y = (e.clientY - b.top) * (H / b.height);
        const k = Math.floor((top + plotH - y) / rowH);
        return k >= 0 && k < n ? k : null;
      };
      const show = e => {
        const k = rowAt(e);
        if (k === null) { hover = null; tip.hide(); return; }
        if (k !== hover) { hover = k; draw(); }
        const age = state.rel ? (k === 20 ? "100+" : `${k * 5}–${k * 5 + 4}`) : (k === 100 ? "100+" : String(k));
        const born = state.rel ? (k === 20 ? `≤ ${state.year - 100}` : `${state.year - k * 5 - 4}–${state.year - k * 5}`)
          : (k === 100 ? `≤ ${state.year - 100}` : `${state.year - k - 1}–${state.year - k}`);
        const v = x => (state.rel ? `${R_.num(L, x, 2)} %` : int(L, x));
        tip.show(`<div class="geores-tip-head">${tr("pyAge")} ${age} · ${tr("pyBornIn")} ${born}</div>` +
          `<span class="app-men-text">■</span> ${tr("pyMen")} ${v(r.m[k])}<br>` +
          `<span class="app-women-text">■</span> ${tr("pyWomen")} ${v(r.f[k])}<br>` +
          `<span class="app-muted">${tr(k === ck ? "pyUnfollow" : "rpyClick")}</span>`, e);
      };
      overlay.addEventListener("pointermove", show);
      overlay.addEventListener("pointerdown", show);
      overlay.addEventListener("pointerleave", () => { hover = null; tip.hide(); draw(); });

      host.replaceChildren(svg);
      geom = { rowAt, ck };
    }

    // A click on a row follows its generation, a click on the generation followed stops (on the host: a click
    // redraws the svg between pointerdown and click)
    host.addEventListener("click", e => {
      const k = geom && geom.rowAt(e);
      if (k === null || k === undefined) return;
      state.born = k === geom.ck ? null : bornOf(k);
      sync();
      draw();
    });

    function stop() {
      clearInterval(playing);
      playing = null;
      playBtn.textContent = `▶ ${tr("pyPlay")}`;
      playBtn.setAttribute("aria-pressed", "false");
      sync();
    }
    playBtn.addEventListener("click", () => {
      if (playing) return stop();
      if (state.year >= pyr.last) state.year = pyr.first;
      playBtn.textContent = `❚❚ ${tr("pyPause")}`;
      playBtn.setAttribute("aria-pressed", "true");
      playing = setInterval(() => {
        if (state.year >= pyr.last) return stop();
        state.year++;
        yearIn.value = state.year;
        yearOut.textContent = state.year;
        draw();
      }, 140);
    });
    yearIn.addEventListener("input", () => { state.year = +yearIn.value; yearOut.textContent = state.year; draw(); });
    yearIn.addEventListener("change", sync);
    placeSel.addEventListener("change", () => { state.code = placeSel.value; load(); });
    relIn.addEventListener("change", () => { state.rel = relIn.checked; sync(); draw(); });
    cmpSel.addEventListener("change", () => { state.cmp = cmpSel.value; load(); });
    A().onResize(host, draw);
    pg.onThemeChange(draw);
  };
})(globalThis);
