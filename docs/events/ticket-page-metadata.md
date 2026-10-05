# Event discovery and transactional metadata

The canonical discovery URL remains `/eventos/:eventId`. Checkout and order pages
point their canonical metadata to that event and use `noindex,follow`. Cloudflare
Pages Functions also return `X-Robots-Tag: noindex, follow`, `private, no-store` and
`Referrer-Policy: no-referrer` on those two transactional routes, before React runs.
They return the normal app shell; order retrieval still requires the existing
server-side capability. Search directives are not authorization controls.

Previously checkout JSON-LD treated positive remaining inventory as `InStock`
even when checkout was disabled, and used face value as the offer price despite
additional buyer fees or tax. Transactional pages no longer publish that duplicate
listing. The public event preview retains structured `Event` data, Open Graph and
Twitter metadata. Generic `Event` avoids classifying every workshop as a concert.
Missing or blank event images now use the existing application preview image.

Publishing verified offers on the canonical event page remains separate work:
it needs an authoritative complete price and actual payment availability, together
with the public event's venue, participants and visibility rules. This change does
not assert rich-result eligibility, provider checkout success or publication of
private event 141.

Validation covers initial response headers and body preservation, invalid routes,
private previews, escaped event content, missing images, checkout metadata, receipt
canonical metadata and existing directory/checkout regressions. Edge preview tests
run in the existing repository quality gate.

References: [Google Event structured data](https://developers.google.com/search/docs/appearance/structured-data/event),
[Schema.org Event](https://schema.org/Event), and
[Cloudflare Pages file-based routing](https://developers.cloudflare.com/pages/functions/routing/).
