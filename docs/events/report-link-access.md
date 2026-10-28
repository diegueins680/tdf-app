# Event report link access

EVT-REPORT-LINK-001 implements the organizer's 27 October 2026 request for an
in-application PATCH CULTURE report visible to anyone who has the link.

The page is served by the existing Cloudflare Pages application at
`https://www.tdfrecords.net/reportes/eventos/<capability>`. The capability is
32 cryptographically random bytes encoded as 43 base64url characters. Only its
SHA-256 digest is committed. The actual link is delivered separately to the
organizer, never included in a public repository, test log, menu or sitemap.

The authorized editorial snapshot lives only in the edge worker, outside the
React bundle and static assets. It preserves the provisional USD80 reported
gross, USD35 owed to Andes, USD45 profit before Meet2Go fees, USD22.50 per party
at 50/50, seven pints and the conditional USD90 calculation / USD10 discrepancy.
The underlying source is the organizer-supplied closeout summary already saved
as event 141 budget note `CIERRE-141-20261024`. The snapshot does not read or
publish private budgets, ledgers, attendees, payment credentials or attachments.
It does not update any financial entry, event state or payment.

## Access boundary

| State or request | Result |
| --- | --- |
| Enabled, matching complete capability, GET | Read-only HTML, no login |
| Enabled, matching complete capability, HEAD | 200, no body |
| Missing, malformed or wrong capability | Generic 404, no report content |
| Disabled or malformed configuration | Generic 404, including an old valid link |
| Rotated digest | Old link returns 404; only new matching capability works |
| Cookie, event ID, name, query-string token | Grants no access |
| POST, PUT, PATCH, DELETE or OPTIONS | 405, no mutation |
| Unrecognized report path | 404 before the SPA can load |

Success and denial responses carry private/no-store, CDN no-store, no-referrer,
noindex/nofollow/noarchive/nosnippet, nosniff and a restrictive CSP with no scripts,
forms, frames or external resources. Legacy production-host redirects for this
route use a noncacheable 302 with the same protective headers. Other existing
routes retain their current behavior. The visible page uses the existing TDF
logo, Spanish content, responsive layout, accessible table and print stylesheet.

Anyone receiving or forwarding the capability can read this snapshot. Revocation
cannot remove copies already saved by a recipient. noindex is a crawler request;
the token check is the access boundary. Provider access logs may contain the URL:
do not publish raw report paths in operational logs or analytics. This page does
not load the app's analytics or external preview services.

## Revoke and rotate

Set the Pages binding `EVENT_REPORT_LINKS_ENABLED=false` and redeploy to revoke
access. Only an absent value or the exact string `true` enables the page. The
binding must be changed in each deployed environment that served the report;
immutable branch previews retain their own configuration. Delete or disable old
preview deployments when revoking a distributed preview link.

For rotation, generate a new 32-byte base64url capability with Node's
`crypto.randomBytes(32).toString('base64url')`. Store it privately (file mode 0600),
calculate its SHA-256 hex digest and set `PATCH_CULTURE_REPORT_TOKEN_SHA256` to
that 64-character lowercase digest. Deploy, then deliver the new URL privately.
Empty or malformed digests fail closed. Do not commit the token or use a
`VITE_*` variable. Existing event, checkout and accounting APIs are unaffected.

## Verification

`node --test scripts/__tests__/event-preview.test.mjs scripts/__tests__/ticket-page-preview.test.mjs`
executes actual Request/Response handlers, grant/deny cases, revocation and
rotation transitions, method refusal, HTML escaping and protective middleware,
alongside the existing event preview and ticket header regressions. Denial
assertions are negative controls for bypassing the digest or configuration gate.
The finite case matrix is not a cryptographic proof or a proof of CDN behavior.

Use Wrangler Pages locally to verify route compilation and installed worker
responses, then inspect a browser at desktop and narrow widths. A Vite-only
server does not execute Pages Functions. Deploy through the existing reviewed
web release process, verify the exact deployed commit and actual HTTP headers,
and only then label the canonical production URL live. No API deployment or
database migration is required.
