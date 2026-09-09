# RSVP operations, previews, and deep links

## Database rollout

The canonical migration is
`tdf-hq/sql/2026-09-08_event_rsvp_identity_privacy_feed.sql`. Run the repository migration
preflight against an anonymized production-shaped database before registering or applying
it. The migration:

- records malformed and duplicate legacy rows in `event_rsvp_migration_evidence` before
  removing them from the live table;
- deterministically keeps the row with the newest `updated_at`, then `created_at`, then
  highest `id` for each event/person pair;
- leaves every historical RSVP hidden (`show_on_profile = false` and
  `visibility_decided_at IS NULL`);
- validates canonical status values and enforces one row per `(event_id, party_id)`;
- creates feed, aggregate, and mutation-rate indexes; and
- makes anonymous event visibility require explicit `metadata.isPublic = true`.
- removes legacy text-linked RSVP and throttle rows during hard Party deletion.

The migration evidence table is the duplicate/invalid-row operational counter and audit
source. `event_rsvp_mutation_rate_limit` provides bounded per-hour mutation counters, while
the application emits JSON mutation logs for success, eligibility rejection, and rate-limit
outcomes without Party identifiers or user-authored content.

Local verification uses a disposable PostgreSQL 16 cluster:

```sh
./scripts/test-event-rsvp-identity-migration.sh
```

The paired rollback disables the RSVP workflow capability but deliberately preserves rows,
consent decisions, evidence, constraints, and indexes. Destructive schema rollback requires
a separately reviewed retention/export plan and is not part of an application rollback.

## Cloudflare Pages preview function

`functions/eventos/[eventId].js` fetches only the anonymous directory event projection and
injects escaped Open Graph, Twitter, canonical, date, image, and `MusicEvent` JSON-LD values
into the initial SPA HTML. Private, hidden, deleted, and malformed identifiers return a
non-enumerating 404. A cancelled event may retain a public preview when it remains public,
but clients disable new RSVP and share actions. Event preview responses use `Cache-Control:
no-store` so revoking public visibility cannot leave event details in an edge cache.

Set `PUBLIC_API_BASE` in the Cloudflare Pages preview and production environments. Validate
on a non-production preview deployment before release:

```sh
curl -fsS -A 'facebookexternalhit/1.1' 'https://PREVIEW_HOST/eventos/EVENT_ID' \
  | rg 'og:title|og:image|event:start_time|application/ld\+json|canonical'
curl -fsS -A 'Twitterbot/1.0' 'https://PREVIEW_HOST/eventos/EVENT_ID'
```

The returned initial HTML must contain the actual event values. Browser-only metadata from
the React application is a secondary enhancement and is not crawler evidence.

## Universal Links and Android App Links

The mobile app accepts only positive numeric `/eventos/:eventId` paths. Campaign handling
ignores all parameters except the reviewed `utm_source` and `utm_campaign` values used for
anonymous attribution; the event resource remains the parameter-free canonical URL.

Cloudflare functions serve the association documents without inventing deployment
credentials:

- `APPLE_TEAM_ID` is required for `/.well-known/apple-app-site-association`.
- `ANDROID_APP_LINK_SHA256_CERT_FINGERPRINTS` is required for
  `/.well-known/assetlinks.json`; provide comma-separated release certificate SHA-256
  fingerprints.
- The iOS application identifier and Android package remain those declared in
  `tdf-mobile/app.config.ts`; verify them against the actual App Store and signed Android
  release records before publishing.

Until those externally controlled values, HTTPS deployment, and signed release builds are
available, association-file and installed-app opening verification remain pending. Web
fallback at `/eventos/:eventId` continues to work independently.

## Privacy and incident checks

Public responses expose aggregate accepted/interested counts only. Self RSVP responses omit
party identity, and organizer rows require explicit organizer/admin authorization. Profile
activity is a live projection from RSVP plus current event visibility, so removing or
declining an RSVP, hiding/deleting an event, blocking access, or removing account ownership
does not leave an independent social post to clean up.

Backend structured logs intentionally contain operation, public event identifier, canonical
status, and outcome—not email, name, token, or party identifier. Alert on mutation-rate
rejections, 409 eligibility changes, migration-evidence growth, unique-constraint errors,
and preview 5xx responses. Never log an auth intent or raw request authorization header.
