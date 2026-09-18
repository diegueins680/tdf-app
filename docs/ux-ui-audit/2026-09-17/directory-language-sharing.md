# Directory language and sharing recovery — 2026-09-18

Extends UX-260917-006 and records UX-260917-047. Depends on the current-session
and optional-storage fix in PR447; these changes retain that implementation.

## Confirmed behavior

A real production-bundle arrival with `html.lang=en` still rendered the directory title,
heading, search, filters, result types and accessible labels in Spanish. The existing
English authentication/navigation fixes remain valid; the newly observed directory
surface was not covered by their acceptance scripts. ES/EN copy now uses the existing
i18next catalogs. Taxonomy requests carry the selected supported language; titles and
structured metadata follow it. Changing languages preserves query input, account state,
public names, descriptions and Ecuador cities. Stored geolocation feedback uses message
keys so a subsequent language change also updates that feedback.

A browser counterexample makes the native sharing promise reject with `AbortError`.
The existing unhandled promise produces a page error. The new handler consumes native
and clipboard rejections, exposes localized status/recovery messages, retains the detail
link as a manual alternative, and prevents a second share while one is pending. A copy
success is displayed only after `writeText` resolves. Native share acknowledgement is
not evidence of message delivery to another person. Cancellation/no-target responses
are informational; neither automatically copies nor retries user content.

Primary reference consulted2026-09-18: [W3C Web Share Recommendation](https://www.w3.org/TR/web-share/),
share algorithm and promise rejection behavior. Platform targets are chosen by the
browser/OS; this increment does not introduce a new sharing provider.

## Verification

- 26 focused component/helper tests pass:19 directory cases and7 existing authoritative
  onboarding helper cases. Coverage includes ES→EN/EN→ES renderer changes, preserved
  query/public content, all inherited session/storage regressions, recovery error clearing,
  and six native/clipboard capability/outcome combinations.
- Ten ES/EN production-bundle journeys pass in three engines/five profiles: delayed
  search label, read failure and explicit retry, retained query, localized taxonomy request,
  ambiguous favorite write followed by persisted read, error dismissal, sharing cancellation,
  denied clipboard followed by successful explicit copy, geolocation denial and map labels.
  These use synthetic APIs and platform sharing/clipboard functions; they do not prove
  server persistence, native OS dialogs, real payment/provider behavior or message delivery.
  Serious/critical axe violations and page errors: zero.
- TypeScript, lint, production build pass; initial JS367923gzip bytes/fivepreloads.
  Strict catalog audit passes with1138reviewed candidates and pinned mobile4122bb75.
- No consequential authority transition was added; inherited DirectoryFavoriteAuthority
  and first-value conformance remain applicable. Full formal CI remains required. No claim
  of whole-platform accessibility, physical screen-reader coverage or field p75 data.
- Initial bilingual harness applied its localStorage setup to the blocked cross-origin map
  iframe and raised a fixture-origin error. Setup was restricted to the top frame; the final
  complete run passes. That failed harness run is not production-defect evidence. Separately,
  the share-cancellation negative control fails on the real old application handler.

Versioned receipt and phone screenshots are in `evidence/directory-language-sharing.json`.
Merge, independent review, CI, Cloudflare publication and production acceptance remain
separate pending steps. This is not a completed whole-platform audit.
