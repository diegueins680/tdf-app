# Existing-account artist claims

The repository owner selected separate account and artist identities on 2026-09-17.
An existing account must request verified management access; claiming an imported
artist must not move credentials, followers, purchases or history between Parties.

The authenticated `/artista/crear?claimArtistId=…` flow now resolves the artist's
published directory profile through the existing `/directory/party-profiles/:id`
endpoint, collects ownership/representation evidence, and submits an `administration`
claim through `/directory/claims`. It preserves the signed-in account and selected
artist. Retries of the same evidence reuse the idempotency key. A missing/private
profile stays unavailable; no new profile or credential is invented as a fallback.

Verification uses the existing directory administrator review queue, not an email
string match. Submitting evidence grants no access. The existing approved-claim
trigger requires the profile and claimant account to match before a manager row
can be written. Approved profiles are edited in **Mis perfiles y clasificados**;
this is a directory management grant, not a global Artist role, credential transfer,
release ownership grant, payout permission, or access to another Party's legacy
`/artists/me` editor. Those broader grants are outside this claim's scope.

The existing guest registration and own-profile activation contracts are unchanged.
The canonical OpenAPI already defines all reused endpoints; no API or generated
client schema change is required. The UI adapter adds the existing Party resolver.

Validation: five component tests cover submission, pending access, idempotent retry,
unavailable resolution and session changes; the onboarding regression covers both
Customer and Artist sessions without logout. The real PostgreSQL fixture
`scripts/__tests__/fixtures/artist-management-claim.sql` verifies pending/mismatched
claim denial, matching approved management, revocation, and unchanged Party,
credential and follower identities. It is also wired into the existing directory
migration test. Local execution used the complete schema and all 104 artist-branch
registered migrations, in a private cluster; this SQL fixture is not an HTTP test.
