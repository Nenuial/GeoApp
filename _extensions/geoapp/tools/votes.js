// Tool "votes": every federal vote since 1981 by canton, district or commune (Federal Statistical Office), on the
// communes of today; the share of yes in classes of 10 %, the lakes on top; the result of the people and of the
// cantons; a cartogram of the communes by number of voters (GeoResources' vote-map, given the vote as a file).
//
//   {{< app tool="votes" presets="d,p,lvl,view" >}}
//   data-d: vote date (YYYY-MM-DD, default the latest);  data-p: proposal number (FSO);
//   data-lvl: cantons, districts, communes (default communes);  data-view: map, cartogram
// Data: /data/votes/index.json, geo.json, <date>.json (data-raw/votes.R).
(function (G) {
  const R_ = G.GeoRes, pg = R_.page, A = () => R_.app;

  R_.addStrings({
    en: {
      voDate: "Vote of", voProposal: "Proposal", voLevel: "By", voCantons: "canton", voDistricts: "district",
      voCommunes: "commune", voMap: "Map", voCartogram: "Cartogram", voYes: "yes", voNo: "no",
      voPeopleYes: "Accepted by the people", voPeopleNo: "Rejected by the people", voCantonsYes: "cantons in favour: {y} of 23",
      voNoData: "no result", voVoters: "{n} eligible voters", voTitle: "official title", voYesShare: "share of yes",
      voSource: "Federal Statistical Office (results; geometries of the communes as of 1 January 2026), opendata.swiss",
      voAbroad: "The Swiss abroad count in the results of Switzerland and of their canton, not in those of the communes.",
    },
    fr: {
      voDate: "Votation du", voProposal: "Objet", voLevel: "Par", voCantons: "canton", voDistricts: "district",
      voCommunes: "commune", voMap: "Carte", voCartogram: "Cartogramme", voYes: "oui", voNo: "non",
      voPeopleYes: "Accepté par le peuple", voPeopleNo: "Rejeté par le peuple", voCantonsYes: "cantons favorables : {y} sur 23",
      voNoData: "pas de résultat", voVoters: "{n} électeurs inscrits", voTitle: "titre officiel", voYesShare: "part de oui",
      voSource: "Office fédéral de la statistique (résultats ; géométries des communes au 1er janvier 2026), opendata.swiss",
      voAbroad: "Les Suisses de l'étranger comptent dans les résultats de la Suisse et de leur canton, pas dans ceux des communes.",
    },
  });

  // No (pink) to yes (green), classes of 10 %; blue stays for the lakes
  const COLORS = ["#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419"];
  const HALF = new Set([6, 7, 12, 13, 15, 16]);   // Obwalden, Nidwalden, Basel-Stadt, Basel-Landschaft, the two Appenzells
  const LEVELS = ["cantons", "districts", "communes"];

  R_.sims["app-votes"] = function (container, opts = {}) {
    const L = R_.lang(opts.lang), tr = k => R_.t(L, k), { el, int } = A(), esc = R_.lib.esc;
    container.classList.add("app-votes");
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

    const state = { d: opts.d || "", p: opts.p || "", lvl: LEVELS.includes(opts.lvl) ? opts.lvl : "communes",
      view: opts.view === "cartogram" ? "cartogram" : "map" };
    const panel = pg.controls(container, `
      <label>${tr("voDate")} <select class="date"></select></label>
      <label class="app-wide">${tr("voProposal")} <select class="prop"></select></label>
      <span class="app-tabs app-levels" role="group">${LEVELS.map(l => `<button type="button" data-lvl="${l}">${tr("vo" + l[0].toUpperCase() + l.slice(1))}</button>`).join("")}</span>
      <span class="app-tabs" role="group"><button type="button" data-view="map">${tr("voMap")}</button><button type="button" data-view="cartogram">${tr("voCartogram")}</button></span>`);
    const $ = s => panel.querySelector(s);
    const dateSel = $(".date"), propSel = $(".prop");

    let index = null, geo = null, vote = null, prop = null, shapes = null, blobUrl = null;
    const longDate = d => new Date(`${d}T12:00:00Z`).toLocaleDateString(L === "fr" ? "fr-CH" : "en-GB", { day: "numeric", month: "long", year: "numeric", timeZone: "UTC" });
    const title = p => R_.typo ? R_.typo("fr", p.fr) : p.fr;
    const pct = (y, n) => (y + n ? 100 * y / (y + n) : null);
    const fmtPct = v => `${R_.num(L, v, 1)} %`;

    Promise.all([A().data("votes/index.json"), A().data("votes/geo.json")]).then(([ix, g]) => {
      index = ix; geo = g;
      if (!index.dates.some(x => x.date === state.d)) state.d = index.dates[0].date;
      dateSel.innerHTML = index.dates.map(x => `<option value="${x.date}">${longDate(x.date)}</option>`).join("");
      shapes = build();
      return load();
    }).catch(err => { console.error(err); A().message(host, tr("appError")); });

    function load() {
      return A().data(`votes/${state.d}.json`).then(v => {
        vote = v;
        if (!v.proposals.some(p => String(p.id) === String(state.p))) state.p = String(v.proposals[0].id);
        prop = v.proposals.find(p => String(p.id) === String(state.p));
        propSel.innerHTML = v.proposals.map(p => `<option value="${p.id}">${esc(title(p))}</option>`).join("");
        sync();
        draw();
      }).catch(err => { console.error(err); A().message(host, tr("appError")); });
    }
    function sync() {
      dateSel.value = state.d;
      propSel.value = state.p;
      panel.querySelectorAll("[data-lvl]").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.lvl === state.lvl)));
      panel.querySelectorAll("[data-view]").forEach(b => b.setAttribute("aria-pressed", String(b.dataset.view === state.view)));
      panel.querySelector(".app-levels").hidden = state.view === "cartogram";
      A().setUrl({ d: state.d, p: state.p, lvl: state.lvl === "communes" ? "" : state.lvl, view: state.view === "map" ? "" : state.view });
    }

    // The map, built once: paths of the communes, districts, cantons and lakes (LV95 km; north up)
    function build() {
      const d = polys => polys.map(poly => poly.map(r => {
        let s = "";
        for (let i = 0; i < r.length; i += 2) s += `${i ? "L" : "M"}${r[i]},${-r[i + 1]}`;
        return s + "Z";
      }).join("")).join("");
      let bb = [1e9, 1e9, -1e9, -1e9];
      geo.cantons.forEach(c => c[2].forEach(poly => poly.forEach(r => {
        for (let i = 0; i < r.length; i += 2) bb = [Math.min(bb[0], r[i]), Math.min(bb[1], -r[i + 1]), Math.max(bb[2], r[i]), Math.max(bb[3], -r[i + 1])];
      })));
      const pad = 2, svg = el("svg", { viewBox: `${bb[0] - pad} ${bb[1] - pad} ${bb[2] - bb[0] + 2 * pad} ${bb[3] - bb[1] + 2 * pad}`, class: "app-map app-swiss", role: "img" });
      const layer = (cls, list, polysAt) => {
        const g = el("g", { class: cls }, svg);
        return list.map(u => ({ u, path: el("path", { d: d(polysAt(u)) }, g) }));
      };
      const out = {
        svg,
        communes: layer("app-units app-lvl-communes", geo.communes, u => u[4]),
        districts: layer("app-units app-lvl-districts", geo.districts, u => u[3]),
        cantons: layer("app-units app-lvl-cantons", geo.cantons, u => u[2]),
      };
      layer("app-lakes", geo.lakes.map(p => [p]), u => u[0]);
      layer("app-canton-lines", geo.cantons, u => u[2]);
      svg.addEventListener("pointermove", e => {
        const hit = out[state.lvl].find(x => x.path === e.target);
        if (!hit) return tip.hide();
        const r = result(state.lvl, hit.u);
        tip.show(`<b>${esc(hit.u[1])}</b><br>` + (r && r.p !== null
          ? `${tr("voYesShare")} <b>${fmtPct(r.p)}</b><div class="geores-tip-head">${tr("voYes")} ${int(L, r.yes)} · ${tr("voNo")} ${int(L, r.no)}</div>`
          : tr("voNoData")), e);
      });
      svg.addEventListener("pointerleave", () => tip.hide());
      return out;
    }

    // Yes and no of a canton, district or commune for the proposal shown
    let cache = null;
    function sums() {
      if (cache && cache.prop === prop) return cache;
      const c = prop.communes, dist = new Map(), cant = new Map();
      geo.communes.forEach((u, i) => {
        if (c.yes[i] === null) return;
        for (const [m, k] of [[dist, u[3]], [cant, u[2]]]) {
          const s = m.get(k) || { yes: 0, no: 0 };
          s.yes += c.yes[i]; s.no += c.no[i];
          m.set(k, s);
        }
      });
      for (let k = 1; k <= 26; k++) {
        const s = cant.get(k) || { yes: 0, no: 0 };
        s.yes += prop.abroad.yes[k - 1] || 0; s.no += prop.abroad.no[k - 1] || 0;
        cant.set(k, s);
      }
      cache = { prop, dist, cant };
      return cache;
    }
    function result(lvl, u) {
      if (lvl === "communes") {
        const i = geo.communes.indexOf(u), y = prop.communes.yes[i], n = prop.communes.no[i];
        return y === null ? null : { yes: y, no: n, p: pct(y, n) };
      }
      const s = (lvl === "districts" ? sums().dist : sums().cant).get(u[0]);
      return s ? { yes: s.yes, no: s.no, p: pct(s.yes, s.no) } : null;
    }

    function draw() {
      if (!prop) return;
      tip.hide();
      const people = pct(prop.yes, prop.no);
      let cantonsYes = 0;
      sums().cant.forEach((s, k) => { if (s.yes > s.no) cantonsYes += HALF.has(k) ? 0.5 : 1; });
      const whole = Math.floor(cantonsYes), cantonsText = cantonsYes % 1 ? `${whole || ""}½` : String(whole);
      head.innerHTML = `<div class="app-vote-title">${esc(title(prop))} <span class="app-muted">(${tr("voTitle")})</span></div>` +
        `<b>${longDate(state.d)}</b> · <b class="${people >= 50 ? "app-yes" : "app-no"}">${people >= 50 ? tr("voPeopleYes") : tr("voPeopleNo")}, ${fmtPct(people)} ${tr("voYes")}</b>` +
        ` · ${tr("voCantonsYes").replace("{y}", cantonsText)}`;
      if (state.view === "cartogram") return drawCartogram();
      LEVELS.forEach(l => shapes.svg.querySelector(`.app-lvl-${l}`).style.display = l === state.lvl ? "" : "none");
      shapes[state.lvl].forEach(({ u, path }) => {
        const r = result(state.lvl, u);
        if (!r || r.p === null) { path.style.fill = ""; path.classList.add("app-nodata"); }
        else { path.classList.remove("app-nodata"); path.style.fill = COLORS[Math.min(9, Math.floor(r.p / 10))]; }
      });
      if (host.firstChild !== shapes.svg) host.replaceChildren(shapes.svg);
      foot.innerHTML = `<div class="app-legend">${COLORS.map((c, k) => `<span><i style="background:${c}"></i>${k * 10}–${k * 10 + 10} %</span>`).join("")}` +
        `<span><i class="app-nodata-swatch"></i>${tr("voNoData")}</span></div>` +
        `<div class="app-note">${tr("voAbroad")} ${tr("voSource")}.</div>`;
    }

    // The cartogram: GeoResources' vote-map, given this vote in the schema of its votes.json
    function drawCartogram() {
      const c = prop.communes;
      const data = {
        source: tr("voSource"),
        vote: { date: state.d, en: title(prop), fr: title(prop), yes: prop.yes, no: prop.no,
          communesYes: c.yes.filter((y, i) => y !== null && y > c.no[i]).length, communes: c.yes.filter(y => y !== null).length },
        communes: geo.communes.map((u, i) => [u[0], u[1], u[2], c.el[i] || 0, c.yes[i] || 0, c.no[i] || 0, u[4]]),
        cantons: geo.cantons.map(u => { const s = sums().cant.get(u[0]) || { yes: 0, no: 0 }; return [u[0], u[1], s.yes, s.no, u[2]]; }),
        lakes: geo.lakes,
      };
      if (blobUrl) URL.revokeObjectURL(blobUrl);
      blobUrl = URL.createObjectURL(new Blob([JSON.stringify(data)], { type: "application/json" }));
      const box = document.createElement("div");
      box.className = "geores";
      host.replaceChildren(box);
      if (R_.sims["vote-map"]) R_.sims["vote-map"](box, { lang: L, src: blobUrl, view: "people", title: "false" });   // the title is above
      else A().message(box, tr("appError"));
      foot.innerHTML = `<div class="app-note">${tr("voAbroad")} ${tr("voSource")}.</div>`;
    }

    dateSel.addEventListener("change", () => { state.d = dateSel.value; state.p = ""; load(); });
    propSel.addEventListener("change", () => { state.p = propSel.value; prop = vote.proposals.find(p => String(p.id) === state.p); sync(); draw(); });
    panel.querySelectorAll("[data-lvl]").forEach(b => b.addEventListener("click", () => { state.lvl = b.dataset.lvl; sync(); draw(); }));
    panel.querySelectorAll("[data-view]").forEach(b => b.addEventListener("click", () => { state.view = b.dataset.view; sync(); draw(); }));
  };
})(globalThis);
