// Tool "demograph": birth and death rates and the population of a country over the long run (Human Mortality
// Database, from 1751 for Sweden), the gap between the two rates shaded (natural increase or decrease); a range of
// years; for a whole country, the UN estimates and projections after the last HMD year, to 2100.
//
//   {{< app tool="demograph" presets="p,from,to,ext" >}}
//   data-p: HMD code (CHE, SWE, FRATNP, DEUTE…), default CHE;  data-from, data-to: years;  data-ext="1": to 2100 (UN)
// Data: /data/hmd/demograph.json (data-raw/hmd.R); /data/wpp/ind/{cbr,cdr,pop}.json for the extension (data-raw/wpp.R).
(function (G) {
  const R_ = G.GeoRes, pg = R_.page, A = () => R_.app;

  R_.addStrings({
    en: {
      dgPop: "Population", dgFrom: "From", dgTo: "to", dgExt: "Continue with the UN (to 2100)", dgCountries: "Countries",
      dgParts: "Parts of countries", dgCbr: "Birth rate (‰)", dgCdr: "Death rate (‰)", dgPopulation: "Population",
      dgInc: "natural increase", dgDec: "natural decrease", dgUn: "UN estimates and projections",
      dgSource: "Human Mortality Database (mortality.org)", dgSourceUn: "after {y}: UN, World Population Prospects 2024",
      dgProjection: "projection",
    },
    fr: {
      dgPop: "Population", dgFrom: "De", dgTo: "à", dgExt: "Prolonger avec l'ONU (jusqu'en 2100)", dgCountries: "Pays",
      dgParts: "Parties de pays", dgCbr: "Natalité (‰)", dgCdr: "Mortalité (‰)", dgPopulation: "Population",
      dgInc: "accroissement naturel", dgDec: "décroissance naturelle", dgUn: "estimations et projections de l'ONU",
      dgSource: "Human Mortality Database (mortality.org)", dgSourceUn: "après {y} : ONU, World Population Prospects 2024",
      dgProjection: "projection",
    },
  });

  const COL = { cbr: "#1b9e77", cdr: "#d95f02", pop: "var(--geores-muted)" };

  R_.sims["app-demograph"] = function (container, opts = {}) {
    const L = R_.lang(opts.lang), tr = k => R_.t(L, k), { el, big } = A(), esc = R_.lib.esc;
    container.classList.add("app-demograph");
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

    const state = { p: (opts.p || "CHE").toUpperCase(), from: +opts.from || null, to: +opts.to || null, ext: opts.ext === "1" };
    const panel = pg.controls(container, `
      <label>${tr("dgPop")} <select class="pop"></select></label>
      <label class="app-year">${tr("dgFrom")} <input type="range" class="from" step="1"></label>
      <label class="app-year">${tr("dgTo")} <input type="range" class="to" step="1"></label>
      <span class="readout"><b class="span-out"></b></span>
      <label><input type="checkbox" class="ext"> ${tr("dgExt")}</label>`);
    const $ = s => panel.querySelector(s);
    const popSel = $(".pop"), fromIn = $(".from"), toIn = $(".to"), spanOut = $(".span-out"), extIn = $(".ext");

    let pops = [], byCode = {}, wpp = null;
    A().data("hmd/demograph.json").then(d => {
      pops = d.populations;
      pops.forEach(p => (byCode[p.code] = p));
      if (!byCode[state.p]) state.p = "CHE";
      const opt = p => `<option value="${p.code}">${esc(p[L])}</option>`;
      const sorted = list => list.slice().sort((a, b) => a[L].localeCompare(b[L], L)).map(opt).join("");
      popSel.innerHTML = `<optgroup label="${tr("dgCountries")}">${sorted(pops.filter(p => p.iso3))}</optgroup>` +
        `<optgroup label="${tr("dgParts")}">${sorted(pops.filter(p => !p.iso3))}</optgroup>`;
      update(true);
    }).catch(err => { console.error(err); A().message(host, tr("appError")); });

    const cur = () => byCode[state.p];
    const lastYear = () => (state.ext && cur().iso3 ? 2100 : cur().last);
    function update(keepRange) {
      const p = cur();
      extIn.disabled = !p.iso3;
      if (!p.iso3) state.ext = false;
      const need = state.ext ? Promise.all(["cbr", "cdr", "pop"].map(k => A().data(`wpp/ind/${k}.json`))) : Promise.resolve(null);
      need.then(w => {
        wpp = w ? { cbr: w[0], cdr: w[1], pop: w[2] } : null;
        const lo = p.first, hi = lastYear();
        [fromIn, toIn].forEach(i => { i.min = lo; i.max = hi; });
        if (!keepRange || !state.from) state.from = lo;
        if (!keepRange || !state.to || state.to > hi) state.to = hi;
        state.from = Math.max(lo, Math.min(state.from, hi - 10));
        state.to = Math.min(hi, Math.max(state.to, state.from + 10));
        sync();
        draw();
      }).catch(err => { console.error(err); A().message(host, tr("appError")); });
    }
    function sync() {
      popSel.value = state.p;
      fromIn.value = state.from;
      toIn.value = state.to;
      extIn.checked = state.ext;
      spanOut.textContent = `${state.from}–${state.to}`;
      A().setUrl({ p: state.p, from: state.from === cur().first ? "" : state.from,
        to: state.to === lastYear() ? "" : state.to, ext: state.ext ? "1" : "" });
    }

    // One series for the chart: HMD years, then the UN after the last HMD year
    function series() {
      const p = cur(), rows = [];
      for (let y = state.from; y <= state.to; y++) {
        if (y <= p.last) {
          const i = y - p.first;
          rows.push({ y, cbr: p.cbr[i], cdr: p.cdr[i], pop: p.pop[i], un: false });
        } else if (wpp) {
          const i = y - 1950, v = k => (wpp[k].values[p.iso3] || [])[i];
          rows.push({ y, cbr: v("cbr"), cdr: v("cdr"), pop: v("pop"), un: true });
        }
      }
      return rows;
    }
    function nice(min, max, n) {
      const raw = (max - min) / n || 1, p = Math.pow(10, Math.floor(Math.log10(raw)));
      const st = [1, 2, 2.5, 5, 10].map(k => k * p).find(s => s >= raw);
      return { lo: Math.floor(min / st) * st, hi: Math.ceil(max / st) * st, st };
    }

    function draw() {
      if (!pops.length) return;
      tip.hide();
      const rows = series().filter(r => r.cbr !== null && r.cdr !== null);
      if (!rows.length) return;
      const W = Math.max(300, host.clientWidth), small = W < 560, fs = small ? 12 : 14;
      const expanded = container.classList.contains("geores-expanded");
      const H = Math.round(Math.max(300, Math.min(expanded ? innerHeight - panel.offsetHeight - 200 : innerHeight * 0.6, W * 0.55)));
      const m = { l: small ? 34 : 44, r: small ? 46 : 60, t: 14, b: 30 };
      const X = y => m.l + (y - state.from) / Math.max(1, state.to - state.from) * (W - m.l - m.r);
      const ly = nice(0, Math.max(...rows.map(r => Math.max(r.cbr, r.cdr))), small ? 4 : 5);
      const ry = nice(0, Math.max(...rows.map(r => r.pop || 0)), small ? 4 : 5);
      const Y = (v, r) => m.t + (r.hi - v) / (r.hi - r.lo) * (H - m.t - m.b);
      const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, width: W, height: H, class: "app-svg app-chart", role: "img",
        "aria-label": cur()[L] });
      svg.style.fontSize = `${fs}px`;

      // UN part: shaded from the first UN year; projections after 2023
      const un = rows.find(r => r.un);
      if (un) {
        el("rect", { x: X(un.y - 0.5), y: m.t, width: X(state.to) - X(un.y - 0.5), height: H - m.t - m.b, class: "app-proj" }, svg);
        el("text", { x: X(Math.max(un.y, 2024)) + 4, y: m.t + fs, class: "app-muted app-small" }, svg).textContent =
          un.y > 2023 ? tr("dgProjection") : tr("dgUn");
      }
      // grid
      for (let v = ly.lo; v <= ly.hi + 1e-9; v += ly.st) {
        el("line", { x1: m.l, x2: W - m.r, y1: Y(v, ly), y2: Y(v, ly), class: v === 0 ? "app-axis" : "app-grid" }, svg);
        el("text", { x: m.l - 6, y: Y(v, ly) + fs * 0.35, "text-anchor": "end", class: "app-muted" }, svg).textContent = R_.num(L, v, 0);
      }
      for (let v = ry.lo; v <= ry.hi + 1e-9; v += ry.st) {
        const t = v === 0 ? "0" : v >= 1e6 ? `${R_.num(L, v / 1e6, v % 1e6 ? 1 : 0)} M` : `${R_.num(L, v / 1e3, 0)} k`;
        el("text", { x: W - m.r + 6, y: Y(v, ry) + fs * 0.35, class: "app-muted" }, svg).textContent = t;
      }
      const span = state.to - state.from, every = [5, 10, 20, 25, 50, 100].find(s => span / s <= (small ? 4 : 8));
      for (let y = Math.ceil(state.from / every) * every; y <= state.to; y += every) {
        el("text", { x: X(y), y: H - 8, "text-anchor": "middle", class: "app-muted" }, svg).textContent = y;
      }

      // natural increase (births above deaths) and decrease: one shape per run of years of the same sign
      // (one shape a year would show hairline seams between the years)
      let run = [];
      const flush = () => {
        if (run.length > 1) {
          const up = run.reduce((t, r) => t + r.cbr - r.cdr, 0) >= 0;
          const d = run.map((r, i) => `${i ? "L" : "M"}${X(r.y).toFixed(1)},${Y(r.cbr, ly).toFixed(1)}`).join("") +
            run.slice().reverse().map(r => `L${X(r.y).toFixed(1)},${Y(r.cdr, ly).toFixed(1)}`).join("") + "Z";
          el("path", { d, class: up ? "app-inc" : "app-dec" }, svg);
        }
      };
      rows.forEach((r, i) => {
        const prev = rows[i - 1];
        const gap = prev && prev.y !== r.y - 1, flip = prev && Math.sign(prev.cbr - prev.cdr) * Math.sign(r.cbr - r.cdr) < 0;
        if (gap || flip) { flush(); run = gap ? [] : [prev]; }
        run.push(r);
      });
      flush();
      // curves: the HMD years, then the UN years (dotted) from the last HMD point
      const pts = (list, k, r) => list.filter(row => row[k] !== null)
        .map((row, i) => `${i ? "L" : "M"}${X(row.y).toFixed(1)},${Y(row[k], r).toFixed(1)}`).join("");
      const hmd = rows.filter(q => !q.un), unRows = rows.filter(q => q.un), lastH = hmd[hmd.length - 1];
      const line = (k, r) => {
        if (hmd.length) el("path", { d: pts(hmd, k, r), class: "app-line" + (k === "pop" ? " app-dashed" : ""), style: `stroke:${COL[k]}` }, svg);
        if (unRows.length) el("path", { d: pts(lastH ? [lastH, ...unRows] : unRows, k, r), class: "app-line app-un", style: `stroke:${COL[k]}` }, svg);
      };
      line("pop", ry);
      line("cdr", ly);
      line("cbr", ly);

      const overlay = el("rect", { x: m.l, y: m.t, width: W - m.l - m.r, height: H - m.t - m.b, fill: "transparent" }, svg);
      overlay.addEventListener("pointermove", e => {
        const b = svg.getBoundingClientRect(), x = (e.clientX - b.left) * (W / b.width);
        const yr = Math.round(state.from + (x - m.l) / (W - m.l - m.r) * (state.to - state.from));
        const r = rows.find(q => q.y === yr);
        if (!r) return tip.hide();
        const inc = r.cbr - r.cdr;
        tip.show(`<div class="geores-tip-head">${esc(cur()[L])} · ${yr}${r.un ? ` · ${tr("dgUn")}` : ""}</div>` +
          `<span style="color:${COL.cbr}">■</span> ${tr("dgCbr")} ${R_.num(L, r.cbr, 1)}<br>` +
          `<span style="color:${COL.cdr}">■</span> ${tr("dgCdr")} ${R_.num(L, r.cdr, 1)}<br>` +
          `${inc >= 0 ? tr("dgInc") : tr("dgDec")} ${inc >= 0 ? "+" : ""}${R_.num(L, inc / 10, 2)} %<br>` +
          `${tr("dgPopulation")} ${big(L, r.pop)}`, e);
      });
      overlay.addEventListener("pointerleave", () => tip.hide());
      host.replaceChildren(svg);

      const p = cur();
      head.innerHTML = `<b>${esc(p[L])}</b> · ${state.from}–${state.to}`;
      foot.innerHTML = `<div class="app-legend">` +
        `<span><i class="app-line-swatch" style="border-color:${COL.cbr}"></i>${tr("dgCbr")}</span>` +
        `<span><i class="app-line-swatch" style="border-color:${COL.cdr}"></i>${tr("dgCdr")}</span>` +
        `<span><i class="app-line-swatch app-dashed" style="border-color:${COL.pop}"></i>${tr("dgPopulation")}</span>` +
        `<span><i class="app-inc-swatch"></i>${tr("dgInc")}</span><span><i class="app-dec-swatch"></i>${tr("dgDec")}</span></div>` +
        `<div class="app-note">${tr("dgSource")}${un ? ` · ${tr("dgSourceUn").replace("{y}", p.last)}` : ""}.</div>`;
    }

    popSel.addEventListener("change", () => { state.p = popSel.value; update(false); });
    extIn.addEventListener("change", () => { state.ext = extIn.checked; if (state.ext) state.to = 2100; update(true); });
    const range = () => {
      let a = +fromIn.value, b = +toIn.value;
      if (b - a < 10) { if (document.activeElement === fromIn) a = b - 10; else b = a + 10; }
      state.from = Math.max(+fromIn.min, a);
      state.to = Math.min(+toIn.max, b);
      fromIn.value = state.from;
      toIn.value = state.to;
      spanOut.textContent = `${state.from}–${state.to}`;
      draw();
    };
    fromIn.addEventListener("input", range);
    toIn.addEventListener("input", range);
    fromIn.addEventListener("change", sync);
    toIn.addEventListener("change", sync);
    A().onResize(host, draw);
  };
})(globalThis);
