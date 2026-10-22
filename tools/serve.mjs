// The built site at the root of http://localhost:<port>, as Caddy serves it in production:
// clean URLs ({path}, {path}.html, {path}/index.html) and precompressed .gz files when the browser accepts them.
// node tools/serve.mjs [port] [folder]   (defaults: 4321, _build/site)
import { createServer } from "node:http";
import { createReadStream, statSync } from "node:fs";
import { extname, join, normalize } from "node:path";

const port = Number(process.argv[2] || 4321);
const root = process.argv[3] || new URL("../_build/site/", import.meta.url).pathname;
const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8", ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8", ".json": "application/json", ".svg": "image/svg+xml", ".png": "image/png",
  ".jpg": "image/jpeg", ".jpeg": "image/jpeg", ".webp": "image/webp", ".ico": "image/x-icon", ".txt": "text/plain",
  ".woff": "font/woff", ".woff2": "font/woff2", ".xml": "application/xml",
};
const isFile = p => { try { return statSync(p).isFile(); } catch { return false; } };

createServer((req, res) => {
  const path = normalize(decodeURIComponent(new URL(req.url, "http://x").pathname)).replace(/^(\.\.[/\\])+/, "");
  const base = join(root, path);
  const file = [base, base + ".html", join(base, "index.html")].find(isFile);
  if (!file) { res.writeHead(404, { "Content-Type": "text/plain" }); return res.end("404"); }
  const headers = { "Content-Type": TYPES[extname(file)] || "application/octet-stream", "Cache-Control": "no-cache" };
  let send = file;
  if (/\bgzip\b/.test(req.headers["accept-encoding"] || "") && isFile(file + ".gz")) {
    send = file + ".gz";
    headers["Content-Encoding"] = "gzip";
  }
  res.writeHead(200, headers);
  createReadStream(send).pipe(res);
}).listen(port, () => console.log(`GeoApp: http://localhost:${port}/`));
