# Artist-owned featured track

The preference is one artist-owned reference to either a canonical recording or
an existing public legacy release. It does not duplicate catalogue records,
publish content, grant audio access or replace the normal queue. Every public
read rechecks membership and canonical public asset authorization. Unavailable
references and their timestamps are hidden from public responses; the owner can
still inspect and clear the saved reference.

`GET /music/artists/{artistPartyId}/featured-track` returns the current public
reference. Authenticated `GET /music/me/featured-track` retains the owner's saved
reference, including an unavailable choice. `PUT /music/me/featured-track` requires
`artistPartyId` to equal the authenticated Party; the server derives its writer
from authentication. This expected-owner field prevents a delayed clear command
from applying to a newly signed-in account. Other accounts, including unrelated
administrators, cannot select or clear another artist's preference.

Example authenticated request (use the existing authenticated client):

```http
PUT /music/me/featured-track
Content-Type: application/json

{"artistPartyId":17,"recordingId":"44000000-0000-4000-8000-000000000001","legacyReleaseId":null}
```

Both source fields null (or omitted) clear the preference. Two non-null source
identities are rejected. Concurrent writes use the last successful server write;
the response is the authoritative current preference. Public music must be
enabled. Saving changes the order on the next profile visit and does not interrupt
the current listening context.

The profile player matches the preference against its exact artist-scoped queue,
moves only the matching recording/release first, and preserves all other order.
A missing, foreign, revoked or unplayable selection falls through to existing
queue recovery. Direct legacy eligibility remains metadata membership, not a
server decoding/health verdict: known provider pages are excluded by the shared
client classifier, and native failures use normal rendition/track recovery.
The optional preference read has a two-second abort bound and cannot prevent the
remaining catalogue from loading. Explicit pause and latest-profile fences remain
in effect.

The owner selector requires a successful same-account settings baseline before
allowing changes. It aborts outstanding work at session/profile boundaries and
ignores foreign or stale receipts. An unavailable saved choice can be replaced or
cleared even when the current playable catalogue is empty.

## Database and verification

Migration `2026-10-30_artist_featured_track.sql` adds the preference and its scoped
eligibility/upsert functions. Rollback removes these functions while retaining
saved artist choices; reapplication restores behavior without deleting them.
No production migration or deployment has run.

The PostgreSQL runner `node scripts/test-artist-featured-track.mjs` uses its own
new disposable database, the actual publication fixture and public delivery gate.
It checks unpublished/foreign/ambiguous choices, legacy membership, territory,
private diagnostics, withdrawal, clearing, repeat application and preserving
rollback. `--contract-only` checks the shipped handler query/bindings, public
projection and expected-owner guard with guard-removal negative controls; this
mode is not a PostgreSQL, compiled-handler or HTTP-authentication pass.

Local Stack and PostgreSQL binaries are unavailable. Required CI compiles the
backend and executes the database runner. Client checks and their exact counts
are recorded in PR #670; current-head native/HTTP qualification remains required.

## Mobile contract publication blocker

Both clients were generated from the same OpenAPI contract and their bytes match.
The generated Mobile file type-checks in isolation. However, the connected GitHub
integration returns 403 `Resource not accessible by integration` for writes to
`diegueins680/tdf-mobile`, and local Git has no credentials for that repository.
The existing published Mobile pin is retained. The reproducible companion patch is
`pending-contracts/artist-featured-track-mobile.patch` (base
`7586be92f447faaf05d1f7d7073af6321ad1496a`); the source contract can also regenerate it.

Publishing the Mobile commit and advancing the root pin is required before merge.
The generated-client gate intentionally remains authoritative and is expected to
reject Mobile regeneration drift until that dependency is published. No gate is
removed, skipped or weakened. Keep #670 in draft. The full playback request also
still requires authoritative media-health persistence, safe periodic probes,
repair/revalidation UI and final hosted/native release evidence.


## Related hosted playback evidence

The previous 3a863269336b2c9104ec16bb168d7883bc5e240f candidate completed 67 of
69 native AAC/FLAC cases. The two failures demanded exactly three grants for
every failed rendition; Firefox and WebKit classified the final rejection as
terminal after two. The exhaustion case now checks that every rendition is tried,
none exceeds the initial grant plus two retries, the next recording actually
plays, and all counts remain stable during a later observation window. The audio
element identity check remains. This amended native case requires hosted rerun.
The same older candidate's repository job failed its synthetic Docker cleanup
check; no passing repository-wide or deployed claim is made for that execution.
