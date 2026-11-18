# Artist-owned featured track

This composition retains the backend preference APIs and restores the PR #670 editor and artist navigation behind its disabled-by-default rollout gate. Native-session reconciliation and fresh qualification remain required. No production deployment or feature activation is claimed.

One preference per artist references a canonical recording or an existing legacy release. It does not publish content, grant media access or duplicate catalog records. Canonical eligibility uses the current public asset authorization, including publication and territory. Legacy eligibility requires at least one current source that is not confirmed invalid. Public artist reads also respect the directory visibility policy. Saved unavailable references remain available to their owner.

`GET /music/artists/{artistPartyId}/featured-track` returns the public reference; unavailable IDs and timestamps are concealed. Authenticated `GET /music/me/featured-track` retains the owner's saved choice. `PUT /music/me/featured-track` requires `artistPartyId` to equal the authenticated Party, including for administrators. This expected-owner field prevents a delayed command from applying to a newly signed-in account. All successful responses are private and no-store.

```http
PUT /music/me/featured-track
Content-Type: application/json

{"artistPartyId":17,"recordingId":"44000000-0000-4000-8000-000000000001","legacyReleaseId":null}
```

Null or omitted source fields clear the preference; two references are rejected. Concurrent writes use the last successful server write. Saving has no playback side effect. The dependent player must match the returned preference against its authorized artist queue and then fall through to eligible tracks, preserving explicit pause and request-generation fences. Reference availability does not guarantee successful decoding or network access.

The original `2026-10-30_artist_featured_track.sql` migration and introduction history are immutable. Forward migration `2026-11-07_music_featured_source_health.sql` couples legacy availability to persisted source health. Confirmed invalidity hides the public reference while retaining the preference; successful validation restores eligibility. Recovery retains preference data and the health policy.

Database and HTTP checks cover ownership, ambiguous/foreign/unpublished selections, trusted territory, unavailable public projection, invalid reselection, successful restoration and profile withdrawal. The contract-only runner checks shipped query bindings and negative controls; it does not establish HTTP authentication or PostgreSQL behavior. Both generated clients are published; Mobile PR #182 merged through normal protection. Current qualification and remaining release prerequisites are recorded in `unified-playback-progress.md`.
