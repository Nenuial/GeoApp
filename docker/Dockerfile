# GeoApp: the rendered site (_build/site, copied next to this file as site/ by tools/release.sh) served by Caddy.
# Built on the server: docker build -t geoapp:<tag> .
FROM caddy:2-alpine
COPY Caddyfile /etc/caddy/Caddyfile
COPY site /srv
