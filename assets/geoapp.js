// GeoApp page modes, around the tool of a page (the first .geores sim in <main>):
//   ?embed            the tool alone, for iframes (Moodle), with a link to the full page
//   ?present, key P   projector: the tool fills the screen (GeoRes "Enlarge"), larger controls, no cursor when idle
//   ?<option>=<value> presets: only the options listed in the tool's data-presets ("station,compare")
// and a toolbar under the tool: Full screen (present mode), Link (copy the address), Embed (copy the iframe code).
// Loaded at the end of <body>, before DOMContentLoaded: presets reach the sim before GeoRes.mount().
(function () {
  const doc = document.documentElement;
  const tool = document.querySelector("main .geores[data-sim]");
  if (!tool) return;
  const params = new URLSearchParams(location.search);
  const fr = (doc.lang || "").toLowerCase().startsWith("fr");
  const T = fr
    ? { present: "Plein écran", link: "Lien", embed: "Intégrer", copied: "Copié", open: "Ouvrir dans GeoApp",
        rotate: "Sur un téléphone : tournez-le à l'horizontale ou passez en plein écran." }
    : { present: "Full screen", link: "Link", embed: "Embed", copied: "Copied", open: "Open in GeoApp",
        rotate: "On a phone: turn it sideways or use Full screen." };

  // Presets: data-presets="station,compare" → ?station=ch-ber sets data-station
  const camel = k => k.replace(/-([a-z])/g, (_, c) => c.toUpperCase());
  (tool.dataset.presets || "").split(",").map(s => s.trim()).filter(Boolean).forEach(k => {
    const v = params.get(k);
    if (v !== null && /^[\w .,:+-]{1,60}$/.test(v)) tool.dataset[camel(k)] = v;
  });

  // Address of the page without the modes
  const pageUrl = () => {
    const u = new URL(location.href);
    ["embed", "present"].forEach(k => u.searchParams.delete(k));
    return u.toString();
  };

  if (params.has("embed")) {
    doc.classList.add("app-embed");
    const wrap = document.createElement("div");
    wrap.className = "app-embed-wrap";
    document.body.prepend(wrap);
    wrap.appendChild(tool);
    const a = document.createElement("a");
    a.className = "app-embed-link";
    a.href = pageUrl();
    a.target = "_blank";
    a.rel = "noopener";
    a.textContent = T.open + " ↗";
    wrap.appendChild(a);
    return;
  }

  // Present mode: larger controls first (the Enlarge view measures its content when it opens), then Enlarge
  let idle = null;
  const wake = () => {
    doc.classList.remove("app-idle");
    clearTimeout(idle);
    idle = setTimeout(() => doc.classList.add("app-idle"), 2500);
  };
  function present(on) {
    doc.classList.toggle("app-present", on);
    const btn = tool.querySelector(":scope > .geores-expand");
    if (btn && tool.classList.contains("geores-expanded") !== on) btn.click();
    if (on) { document.addEventListener("pointermove", wake); wake(); }
    else { document.removeEventListener("pointermove", wake); clearTimeout(idle); doc.classList.remove("app-idle"); }
  }
  // Leaving the Enlarge view (its button or Escape) leaves present mode
  new MutationObserver(() => {
    if (doc.classList.contains("app-present") && !tool.classList.contains("geores-expanded")) present(false);
  }).observe(tool, { attributes: true, attributeFilter: ["class"] });
  document.addEventListener("keydown", e => {
    if (e.key !== "p" && e.key !== "P") return;
    if (e.ctrlKey || e.metaKey || e.altKey || e.target.closest("input, select, textarea, [contenteditable]")) return;
    present(!doc.classList.contains("app-present"));
  });

  // Toolbar
  const bar = document.createElement("div");
  bar.className = "app-toolbar";
  const button = (icon, label, action) => {
    const b = document.createElement("button");
    b.type = "button";
    b.innerHTML = `<i class="bi bi-${icon}" aria-hidden="true"></i> <span>${label}</span>`;
    b.addEventListener("click", () => action(b));
    bar.appendChild(b);
    return b;
  };
  const copy = (b, text, label) => {
    const done = () => {
      b.classList.add("app-done");
      b.querySelector("span").textContent = T.copied;
      setTimeout(() => { b.classList.remove("app-done"); b.querySelector("span").textContent = label; }, 1800);
    };
    if (navigator.clipboard) navigator.clipboard.writeText(text).then(done, () => prompt("", text));
    else prompt("", text);
  };
  button("arrows-fullscreen", T.present, () => present(true));
  button("link-45deg", T.link, b => copy(b, pageUrl(), T.link));
  button("code-slash", T.embed, b => {
    const u = new URL(pageUrl());
    u.searchParams.set("embed", "");
    const src = u.toString().replace(/embed=(&|$)/, "embed$1");
    copy(b, `<iframe src="${src}" style="width:100%;aspect-ratio:16/10;border:0" allowfullscreen></iframe>`, T.embed);
  });
  tool.after(bar);

  document.addEventListener("DOMContentLoaded", () => {
    // after GeoRes.mount (its listener was added first, by geores.js in <head>)
    if (tool.querySelector("canvas")) {
      const hint = document.createElement("p");
      hint.className = "app-rotate";
      hint.textContent = T.rotate;
      bar.after(hint);
    }
    if (params.has("present")) present(true);
  });
})();
