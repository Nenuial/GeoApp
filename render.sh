#!/bin/sh
# Render GeoApp outside Dropbox (rule of Work/Geography/CLAUDE.md): the sources are copied to a work
# folder, rendered there with babelquarto (English, then French under fr/), and the site is copied back
# into _build/site (excluded from Dropbox, served by `make serve`, packed by `make release`).
#
#   ./render.sh          the whole site (babelquarto always renders everything)
#
# Work folder: GEOAPP_RENDER_DIR, default ~/Library/Caches/geoapp-render.
# Language links are root-relative ("/fr/…"), so the same build works locally and on any host.
set -e
src=$(cd "$(dirname "$0")" && pwd)
work="${GEOAPP_RENDER_DIR:-$HOME/Library/Caches/geoapp-render}/GeoApp"
mkdir -p "$work" "$src/_build/site"

# Sources: everything but builds, caches, tests and the print SVGs of GeoResources (unused on the web;
# babelquarto copies the project twice more, so they would cost 115 MB per copy)
rsync -a --delete \
  --exclude '/.git' --exclude '/_build/' --exclude '/_site/' --exclude '/.quarto/' \
  --exclude '/tests/' --exclude '/_extensions/geores/dist/svg/' --exclude 'node_modules/' \
  --exclude '*Copie en conflit*' --exclude '*conflicted copy*' --exclude '.DS_Store' \
  "$src/" "$work/"

(cd "$work" && Rscript -e 'babelquarto::render_website(site_url = "", preview = FALSE)')

# The data files are fetched at /data/…: the French copy babelquarto makes is not needed
rm -rf "$work/_site/fr/data"

# Precompressed copies of the data, served by Caddy (and tools/serve.mjs) to browsers that accept gzip
[ ! -d "$work/_site/data" ] || find "$work/_site/data" -name '*.json' -exec gzip -kf9 {} +

rsync -a --delete "$work/_site/" "$src/_build/site/"

conflicts=$(find "$src/_build/site" \( -name '*Copie en conflit*' -o -name '*conflicted copy*' \) | wc -l | tr -d ' ')
[ "$conflicts" = 0 ] || echo "warning: $conflicts Dropbox conflict copies in _build/site" >&2
echo "Rendered in $work, site copied to $src/_build/site"
