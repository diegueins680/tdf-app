# Artist profile self-service

An authenticated account can activate its own artist profile immediately at
`POST /artists/me/activate`. The request accepts no target identity or role.
The server locks the account's Party and uses the persisted
`artist.self-service.artist` policy to grant only Artist, create the canonical
profile, and complete any pending `artist.onboarding:create` request in the same
transaction. Repeated calls preserve profile content and do not duplicate grants
or audit events. Inactive accounts and revoked Artist assignments remain blocked.

The web onboarding page refreshes the authoritative session before opening the
editor. Mobile self-creation uses the same server transaction and refreshes the
session after saving. Older clients posting an artist access request receive an
approved result immediately; other feature requests retain manual review.
Existing links to the artist access-request form open profile creation instead.
Administrators retain their existing profile access without adding a mixed Artist
role that would remove their strict administrative scope.

Deployment requires `2026-09-16_artist_self_service` through the registered
production migration lane, followed by the backend and web/mobile updates. The
rollback disables only the new assignment policy and preserves existing profiles
and grants; roll back the application before disabling its required policy.

Run `scripts/test-artist-self-service.sh` with local PostgreSQL available. It uses
an isolated database to exercise the real Haskell activation transaction,
concurrent requests, retries, pending-request completion, revoked/inactive
accounts, and rollback after a failed profile write. It also verifies migration
reapplication and rollback. Web coverage is in `ArtistOnboardingPage.test.tsx`,
`loginRouting.test.ts`, and `featureRegistry.test.ts`; mobile routing coverage is
in `onboardingIntent.test.ts` and `featureRegistry.test.ts`.

The release preflight captures the existing event-discovery flags and requires
fleet agreement. Rollout and rollback preserve those flags, and a change after
preflight blocks the rollout instead of overwriting an active feature.
