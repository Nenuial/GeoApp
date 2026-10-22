// Every page of the English site needs its French twin (<page>.fr.qmd): babelquarto removes the pages
// without one from the French site, and the French links to them break. OC pages (oc/) are French only.
// Usage: node tools/check-pairs.mjs   (exit code 1 when a twin is missing)
import { readdirSync, existsSync, statSync } from "node:fs";
import { join, relative } from "node:path";

const root = new URL("..", import.meta.url).pathname;
const SKIP = new Set(["_build", "_site", "_extensions", "node_modules", "tests", "data-raw", ".quarto"]);

function* qmds(dir) {
  for (const name of readdirSync(dir)) {
    if (name.startsWith(".") || SKIP.has(name)) continue;
    const path = join(dir, name);
    if (statSync(path).isDirectory()) yield* qmds(path);
    else if (name.endsWith(".qmd")) yield relative(root, path);
  }
}

const missing = [];
for (const f of qmds(root)) {
  const oc = f.startsWith("oc/");
  if (f.endsWith(".fr.qmd")) {
    if (!oc && !existsSync(join(root, f.replace(/\.fr\.qmd$/, ".qmd")))) missing.push(`${f}: no English page`);
  } else if (oc) {
    missing.push(`${f}: OC pages are French only (name it .fr.qmd)`);
  } else if (!existsSync(join(root, f.replace(/\.qmd$/, ".fr.qmd")))) {
    missing.push(`${f}: no French twin`);
  }
}
if (missing.length) {
  console.error(missing.join("\n"));
  process.exit(1);
}
console.log("Pages: every English page has its French twin, OC pages are French only.");
