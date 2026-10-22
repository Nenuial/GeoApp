#!/bin/sh
# Release GeoApp: tag the commit, send the rendered site to the server, build the image there, start it.
#   tools/release.sh v2.0.0          (after make render and make test, with everything committed)
#   tools/release.sh --rollback v2.0.0
# The server: SERVER (default ubuntu@5.196.76.20), folder REMOTE (default GeoApp, in the home folder).
# Every release is approved by the user first (Work/Geography/CLAUDE.md, GeoApp/CLAUDE.md).
set -e
SERVER=${SERVER:-ubuntu@5.196.76.20}
REMOTE=${REMOTE:-GeoApp}
here=$(cd "$(dirname "$0")/.." && pwd)

if [ "$1" = "--rollback" ]; then
  tag=${2:?"usage: $0 --rollback <tag>"}
  ssh "$SERVER" "cd $REMOTE && docker image inspect geoapp:$tag >/dev/null && echo GEOAPP_TAG=$tag > .env && docker compose up -d"
  echo "GeoApp rolled back to $tag"
  exit 0
fi

tag=${1:?"usage: $0 <tag, e.g. v2.0.0>"}
cd "$here"
[ -z "$(git status --porcelain)" ] || { echo "commit first: the release must match a commit" >&2; exit 1; }
[ -f _build/site/index.html ] || { echo "render first (make render)" >&2; exit 1; }
conflicts=$(find . -name '*onflit*' -not -path './tests/node_modules/*' | wc -l | tr -d ' ')
[ "$conflicts" = 0 ] || { echo "Dropbox conflict copies: remove them first" >&2; exit 1; }
git tag -a "$tag" -m "GeoApp $tag" 2>/dev/null || echo "tag $tag exists already: releasing it again"

ssh "$SERVER" "mkdir -p $REMOTE/releases/$tag"
rsync -az --delete _build/site/ "$SERVER:$REMOTE/releases/$tag/site/"
rsync -az docker/Dockerfile docker/Caddyfile "$SERVER:$REMOTE/releases/$tag/"
rsync -az docker/compose.yml "$SERVER:$REMOTE/compose.yml"
ssh "$SERVER" "cd $REMOTE/releases/$tag && docker build -q -t geoapp:$tag . && cd ~/$REMOTE && echo GEOAPP_TAG=$tag > .env && docker compose up -d"
echo "GeoApp $tag released"
