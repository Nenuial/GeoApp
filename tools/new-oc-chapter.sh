#!/bin/sh
# A new OC chapter: oc/<name>/index.fr.qmd, listed on the OC page and the French home page by itself.
#   tools/new-oc-chapter.sh chine "Chine"
# Then: a cover in assets/fields/<name>.jpg (GeoDoc OC has one per chapter), an accent and an icon for
# <name> in assets/styles.css and assets/tiles.ejs, and the chapter's tool pages (<sim>.fr.qmd).
set -e
name=${1:?"usage: $0 <folder name> <title>"}
title=${2:?"usage: $0 <folder name> <title>"}
here=$(cd "$(dirname "$0")/.." && pwd)
dir="$here/oc/$name"
[ ! -e "$dir" ] || { echo "oc/$name exists already" >&2; exit 1; }
order=$(($(ls -d "$here"/oc/*/ 2>/dev/null | wc -l) + 1))
mkdir -p "$dir"
cat > "$dir/index.fr.qmd" <<EOF
---
title: "$title"
description: ""
image: ../../assets/fields/$name.jpg
order: $order
body-classes: "app-field-page app-field-$name"
listing:
  contents: ["*.qmd"]
  type: grid
  fields: [image, title, description]
  image-placeholder: ../../assets/fields/$name.jpg
  sort: "title asc"
---
EOF
echo "Created oc/$name/index.fr.qmd (order $order). Add a description, the cover, the accent and the tool pages."
[ -f "$here/assets/fields/$name.jpg" ] || echo "note: no cover assets/fields/$name.jpg yet" >&2
