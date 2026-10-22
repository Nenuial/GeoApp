// Screenshots for review: a few pages at phone, projector and full-HD sizes, dark and light.
// node tests/screens.mjs [outdir] (the site must be served: make serve, port 4321)
import { chromium, devices } from "@playwright/test";
import { mkdirSync } from "node:fs";

const out = process.argv[2] || new URL("screens/", import.meta.url).pathname;
mkdirSync(out, { recursive: true });
const base = process.env.GEOAPP_URL || "http://localhost:4321";
const PAGES = { home: "/index.html", "home-fr": "/fr/index.html", field: "/climatology/index.html",
  climogram: "/climatology/climogram.html", "oc-russie": "/fr/oc/russie/index.html",
  "russia-pyramid": "/fr/oc/russie/russia-pyramid.html", present: "/climatology/climogram.html?present" };
const SIZES = { phone: devices["iPhone 13"], beamer: { viewport: { width: 1280, height: 800 } },
  hd: { viewport: { width: 1920, height: 1080 } } };

const browser = await chromium.launch();
for (const [size, opts] of Object.entries(SIZES)) {
  for (const theme of ["dark", "light"]) {
    const ctx = await browser.newContext(opts);
    await ctx.addInitScript(v => { try { localStorage.setItem("quarto-color-scheme", v); } catch (e) {} },
      theme === "dark" ? "alternate" : "default");
    const page = await ctx.newPage();
    for (const [name, path] of Object.entries(PAGES)) {
      await page.goto(base + path, { waitUntil: "networkidle" });
      await page.waitForTimeout(400);
      await page.screenshot({ path: `${out}/${name}-${size}-${theme}.png`, fullPage: name !== "present" });
    }
    await ctx.close();
  }
}
await browser.close();
console.log(`Screenshots in ${out}`);
