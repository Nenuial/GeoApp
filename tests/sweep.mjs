// Steady controls: on every page with a sim, each slider is moved through its range (and each button with
// data-alt is toggled) at several widths; the controls must keep one layout (nothing moves, no new row).
// node tests/sweep.mjs [width …]   (the site must be built and served: make render, make serve on port 4321)
import { chromium } from "@playwright/test";
import { readdirSync, statSync } from "node:fs";
import { join, relative } from "node:path";

const site = new URL("../_build/site/", import.meta.url).pathname;
const base = process.env.GEOAPP_URL || "http://localhost:4321";
const widths = process.argv.slice(2).map(Number).filter(Boolean);
const WIDTHS = widths.length ? widths : [1280, 1000, 800, 390];
const pages = [];
(function walk(dir) {
  for (const name of readdirSync(dir)) {
    const p = join(dir, name);
    if (statSync(p).isDirectory()) { if (!["site_libs", "data", "assets"].includes(name)) walk(p); }
    else if (name.endsWith(".html")) pages.push("/" + relative(site, p));
  }
})(site);

const browser = await chromium.launch();
let bad = 0;
for (const width of WIDTHS) {
  const page = await browser.newPage({ viewport: { width, height: 900 } });
  for (const path of pages) {
    await page.goto(base + path, { waitUntil: "networkidle" });
    if (!(await page.locator("main .geores[data-sim]").count())) continue;
    await page.waitForTimeout(300);
    const results = await page.evaluate(async () => {
      const frame = () => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)));
      const out = [];
      for (const sim of document.querySelectorAll("main .geores[data-sim]")) {
        const box = sim.querySelector(".geores-controls");
        if (!box) continue;
        const layout = () => {
          const o = box.getBoundingClientRect();
          return [...box.querySelectorAll("input, button, select, label")]
            .filter(e => e.offsetParent)
            .map(e => { const r = e.getBoundingClientRect(); return [r.left - o.left, r.top - o.top, r.width].map(Math.round).join(","); })
            .join(" ") + " h" + Math.round(o.height);
        };
        const seen = new Map();
        const note = what => { const l = layout(); if (!seen.has(l)) seen.set(l, what); };
        note("start");
        for (const input of box.querySelectorAll('input[type="range"]')) {
          if (!input.offsetParent) continue;
          const min = +input.min || 0, max = input.max === "" ? 100 : +input.max, start = input.value;
          const set = v => {
            input.value = v;
            input.dispatchEvent(new Event("input", { bubbles: true }));
            input.dispatchEvent(new Event("change", { bubbles: true }));
          };
          for (let i = 0; i <= 40; i++) {
            set(min + (max - min) * i / 40);
            await frame();
            note(`${input.className || input.name || "slider"}=${input.value}`);
          }
          set(start);
          await frame();
        }
        for (const button of box.querySelectorAll("button[data-alt]")) {
          if (!button.offsetParent) continue;
          button.click(); await frame(); note(`after ${button.dataset.alt}`);
          button.click(); await frame();
        }
        out.push({ sim: sim.dataset.sim, layouts: seen.size, examples: [...seen.values()].slice(0, 4) });
      }
      return out;
    });
    for (const r of results) {
      if (r.layouts > 1) { bad++; console.log(`${width}px ${path} ${r.sim}: ${r.layouts} layouts (${r.examples.join(" | ")})`); }
    }
  }
  await page.close();
}
await browser.close();
console.log(bad ? `${bad} sims move` : `steady: every sim keeps one layout at ${WIDTHS.join(", ")} px`);
process.exit(bad ? 1 : 0);
