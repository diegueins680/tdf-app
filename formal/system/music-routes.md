# `/music` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the handlers on main `f615c862`:
`tdf-hq/src/TDF/Server/MusicRelease.hs`, `MusicReleaseCatalog.hs`, `MusicReleaseUpload.hs`, `MusicReleaseDDEX.hs`, `MusicReleaseCommerce.hs`, `MusicReleaseAssets.hs` and `tdf-hq/src/TDF/MusicRelease/*`.

This describes what the code does today. New requirements written from it have status `implemented but unspecified`.

Every route is behind `AuthProtect "bearer-token"` except `POST /music/playback-events`, which is public and identified by a client-chosen opaque id.

**Shared guards**
- `requireOwnerPermission`: a strict administrator, or `music_can(actor, artist party, permission)`: a verified artist acting for themself, or an active team member holding the owner/admin role or that permission.
- Staff-only transitions, DDEX exports and party registration, and infringement actions require `hasStrictAdminAccess`.
- Feature flags: `music_releases.authoring` (503 when off), `music_releases.public` (404), `music_releases.processing`, `music_releases.commerce`, `music_releases.ddex_export`.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| version transition, corrections | MEDIA-MUSIC-RELEASE-001 | reviewed |
| uploads create, part record, confirm | MEDIA-MUSIC-ASSET-001 | reviewed |
| ddex-exports | MEDIA-MUSIC-DDEX-001 | reviewed |
| `POST purchases` | PAY-MUSIC-PURCHASE-REPLAY-001 | reviewed |
| paypal create/capture, datafast checkout/status | PAY-MUSIC-OUTCOME-001 | reviewed |
| downloads/free, entitlements download | PAY-MUSIC-DOWNLOAD-001 | reviewed |
| releases create, draft, content, validate, terms, comments, resolve | MEDIA-MUSIC-AUTHORING-001 | new |
| upload provider bind, cancel | MEDIA-MUSIC-UPLOAD-SESSION-001 | new |
| ddex/parties | REC-DDEX-REGISTRY-001 | new |
| infringement-reports create, action | MEDIA-MUSIC-INFRINGEMENT-001 | new |
| playback-events (public and personal) | MEDIA-MUSIC-PLAYBACK-001 | new |
| favorites, playlists, playlist items | MEDIA-MUSIC-COLLECTION-001 | new |

## Defect fixed on 2026-11-07

**Verified music money could be rejected with no record.** The PayPal capture and Datafast status paths recorded the verified payment with `recordVerifiedPayment` directly. Every other checkout domain uses `recordVerifiedPaymentOrReconcile`, which records an open `verified_payment_not_admitted` reconciliation exception when authoritative captured money is not admitted (for example into a checkout that expired meanwhile). Music answered 502 and kept nothing, so captured money could go unrecorded. `recordMusicVerifiedPayment` now uses the reconciling helper, PAY-CAPTURE-RECON-001 names the music domain, and a PostgreSQL test in `MusicPurchaseSpec` drives a COMPLETED capture into an expired checkout and finds the open exception.

Remaining callers of the plain helper: the PayPal webhook (records its own mismatch) and the provider reconciliation worker (its inbox event stays recorded and is retried).

## Open behaviours for the owner to decide

**Concurrency and status codes**
- Release creation replays by key without a lock, so two concurrent creations with one key can create two releases; a duplicate slug answers 500.
- A duplicate DDEX party DPID answers 500 instead of 409.
- Adding an item to a playlist the caller does not own answers 409 instead of 404.
- Audit rows are unique per (release, idempotency key) across routes, so a key reused on another route collides with 500.

**Authorization**
- A team member with upload permission can load an upload session but cannot bind, confirm or cancel it (the writes filter on the creator).
- An artist editor can resolve staff-only editorial comments.
- The artist can target `uploading`, `processing`, `validation_failed` and `cancelled` states directly through the transition route.

**Audit gaps**
- A change-request comment and an infringement action both change the version state without the audit row the transition route writes.
- DDEX party registration writes no audit row.

**Other**
- No notification is sent for infringement reports or editorial comments.
- The anonymous playback identity is client chosen, so the per-identity play limit can be evaded by rotating ids.
- The Datafast status route queries the provider before recording the attempt, so a transport error there leaves no attempt record.
