GEORES := ../GeoResources
PORT ?= 4321

.PHONY: all render serve check test sync new-oc-chapter

all: check render test

# Whole site, rendered outside Dropbox (render.sh), copied to _build/site
render:
	./render.sh

# The built site at http://localhost:$(PORT) (absolute paths such as /data/… need a server at the root)
serve:
	node tools/serve.mjs $(PORT)

# Every English page has its French twin (babelquarto drops untranslated pages from /fr/); OC is French only
check:
	node tools/check-pairs.mjs

# Playwright checks against the built site (starts its own server)
test:
	cd tests && npx playwright test

# GeoResources: copy dist/ and the shortcode into _extensions/geores (commit the copy with its VERSION)
sync:
	$(GEORES)/tools/sync.sh "$(CURDIR)"
	cat _extensions/geores/VERSION

# A new OC chapter: make new-oc-chapter NAME=chine TITLE="Chine"
new-oc-chapter:
	tools/new-oc-chapter.sh "$(NAME)" "$(TITLE)"
