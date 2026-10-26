# Music credential response caching

`MEDIA-MUSIC-CREDENTIAL-CACHE-001` is a scoped response-header obligation for
music bearer credentials. It does not certify the whole application's cache
policy, media delivery, or a CDN.

The baseline is protected main `e05d003669a930d54b4fe7dd71d72d09335c11ae`.
A real local HTTP run of compiled candidate
`5f45677588bd8494226e00fd51d81c134b84bdd6` returned a signed public asset URL
without `Cache-Control`. The URL was not fetched; no unauthorized cache
retrieval or production disclosure was demonstrated.

## Boundary and transition

Inputs are WAI's request method and decoded `pathInfo`, plus the response
status, header list and streaming body. For an inventoried credential operation,
replace every case-insensitive `Cache-Control` header with exactly one
`Cache-Control: private, no-store`. Preserve the response status, body bytes and
all other headers, including duplicate `Set-Cookie`, CORS, `Vary` and `ETag`.
For every other operation, return the response unchanged.

The operation inventory follows `TDF.API.MusicRelease` and its actual signing
handlers:

| Method | Path | Credential |
| --- | --- | --- |
| GET / HEAD | `/music/assets/{assetId}/access` | Public streaming/artwork access URL |
| POST | `/music/entitlements/{entitlementId}/download` | Entitlement-bound download URL |
| POST | `/music/downloads/free` | Free-download URL |
| POST | `/music/releases/{releaseId}/versions/{versionId}/uploads` | Multipart creation URL, including an idempotent response |
| GET / HEAD | `/music/uploads/{uploadId}/parts/{partNumber}` | Multipart part upload URL |
| GET / HEAD | `/music/uploads/{uploadId}/completion` | Multipart completion URL and body |
| DELETE | `/music/uploads/{uploadId}` | Optional multipart abort URL |
| GET / HEAD | `/music/ddex/exports/{exportId}/download` | Authorized package URL |

Only trailing empty path segments are ignored for classification. The request
itself is never rewritten. Internal segments and method identity remain exact;
query text cannot add or remove the policy. WAI percent-decodes static path
segments before both this boundary and Servant routing. Malformed captures on
an otherwise matching path receive the policy on their error response too.

`musicCredentialCache` wraps `Server.mkApp` for direct application users and is
also outermost in `App.Boot.wrapApp`. All pre-existing middleware retain their
relative order. The outer wrapper covers startup, CORS denial and exception
responses synthesized outside the inner application. Applying the operation
twice is idempotent, including if intervening middleware adds a conflicting
cache directive.

## Obligations and evidence

| Property | Obligation | Verification boundary |
| --- | --- | --- |
| MCC-001 | Every inventoried operation emits exactly one private/no-store directive, regardless of success or denial status | Finite WAI response cases and actual public Servant router cases |
| MCC-002 | Non-cache headers, status, body and request data remain unchanged | Deterministic WAI comparisons, duplicate cookies and CORS/Vary controls |
| MCC-003 | Catalog, library, lookalike paths, prefixes, query-only matches and other methods retain their existing cache behavior | Finite negative-route cases |
| MCC-004 | Nested boundaries preserve one directive and cover startup/outer exception responses | Existing startup and exception middleware composed with both wrappers |

`tdf-hq/test/TDF/MusicCredentialCacheSpec.hs` exercises the actual middleware and
public API router. `tdf-hq/test/TDF/ServerSpec.hs` additionally targets actual
`mkApp` GET/HEAD asset parsing errors and an unaffected catalog validation error.
The aggregate `Spec.hs` registers `music-unit-credential-cache`; the standalone
`MusicCredentialCacheMain.hs` imports the same specification for focused Stack
execution. Registration in Cabal alone is not execution evidence.

The focused canonical Stack run passed **42 examples, zero failures** on the
recorded source fingerprints. MCC-001 through MCC-004 are **TESTED** within
those finite WAI and public-router cases. This is not mathematical proof,
unbounded route coverage, or target-environment acceptance. The three direct
`mkApp` cases, full aggregate execution, mutation controls and fresh candidate
HTTP qualification remain separate until their own recorded execution. The
execution report records exact source hashes and later outcomes separately.

## Assumptions and limits

The canonical application is served through these entry points; an alternate
application that omits both wrappers is outside this boundary. WAI method/path
and header-name representations retain their documented semantics. Compliant
HTTP caches honor `no-store`; a malicious cache or previously stored response
is not erased by issuing a new header. The inventory must be reviewed when a
new credential endpoint is introduced.

Authorization predicates, feature gates, signed URL JSON, TTLs, upload sessions,
rights, payments, storage/CDN object responses and original masters are
unchanged. This header does not revoke an already issued URL, independently
validate its S3 signature, prevent copying, or replace backend/media-delivery
authorization. Catalog and private library caching remain separate obligations.
A fresh candidate binary, actual HTTP success/denial checks, CI, independent
review and production rollout remain release gates; no deployment is claimed.
