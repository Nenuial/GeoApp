// Thumbnails of the tool pages for the listings: a picture of the sim (dark theme), assets/thumbs/<slug>.jpg,
// and `image:` in the front matter of the page (and its French twin) when it has none.
// node tests/thumbs.mjs [slug …]   (the site must be built and served: make render, make serve on port 4321)
import { chromium } from "@playwright/test";
import { readdirSync, readFileSync, writeFileSync, existsSync, mkdirSync, statSync } from "node:fs";
import { join, relative, basename, dirname } from "node:path";

const root = new URL("../", import.meta.url).pathname;
const base = process.env.GEOAPP_URL || "http://localhost:4321";
const only = new Set(process.argv.slice(2));
const out = join(root, "assets/thumbs");
mkdirSync(out, { recursive: true });

// Tool pages: <field>/<slug>.qmd (English), oc/<chapter>/<slug>.fr.qmd (French only)
const pages = [];
for (const dir of readdirSync(root)) {
  const d = join(root, dir);
  if (dir.startsWith(".") || dir.startsWith("_") || !statSync(d).isDirectory()) continue;
  if (["assets", "data", "data-raw", "tests", "tools"].includes(dir)) continue;
  const walk = p => readdirSync(p).forEach(n => {
    const f = join(p, n);
    if (statSync(f).isDirectory()) return walk(f);
    if (n.startsWith("index.") || !n.endsWith(".qmd")) return;
    const oc = relative(root, f).startsWith("oc/");
    if (oc ? n.endsWith(".fr.qmd") : !n.endsWith(".fr.qmd")) pages.push(f);
  });
  walk(d);
}

const browser = await chromium.launch();
const ctx = await browser.newContext({ viewport: { width: 1280, height: 900 }, deviceScaleFactor: 0.6 });
await ctx.addInitScript(() => { try { localStorage.setItem("quarto-color-scheme", "alternate"); } catch (e) {} });
const page = await ctx.newPage();
for (const qmd of pages) {
  const slug = basename(qmd).replace(/(\.fr)?\.qmd$/, "");
  if (only.size && !only.has(slug)) continue;
  const rel = relative(root, qmd).replace(/(\.fr)?\.qmd$/, ".html");
  const url = `${base}/${rel.startsWith("oc/") ? "fr/" : ""}${rel}`;
  await page.goto(url, { waitUntil: "networkidle" });
  const sim = page.locator("main .geores[data-sim]").first();
  if (!(await sim.count())) { console.warn(`no sim: ${url}`); continue; }
  await page.waitForTimeout(1500);   // let animations settle on a representative frame
  await sim.screenshot({ path: join(out, `${slug}.jpg`), type: "jpeg", quality: 78 });
  // image: in the page and its French twin
  for (const f of [qmd, qmd.replace(/\.qmd$/, ".fr.qmd")]) {
    if (!existsSync(f) || f.endsWith(".fr.fr.qmd")) continue;
    const text = readFileSync(f, "utf8");
    if (/^image:/m.test(text.split("\n---")[0])) continue;
    const up = relative(dirname(f), out);
    writeFileSync(f, text.replace(/^---\n/, `---\nimage: ${up}/${slug}.jpg\n`));
  }
  console.log(`${slug}: ${url}`);
}
await browser.close();
