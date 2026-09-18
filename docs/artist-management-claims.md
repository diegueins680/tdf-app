# Existing-account artist claims

The repository owner selected separate account and artist identities on 2026-09-17.
An existing account must request verified management access; claiming an imported
artist must not move credentials, followers, purchases or history between Parties.

The authenticated `/artista/crear?claimArtistId=…` flow prepares a claim target with
`PUT /directory/artist-claim-targets/:partyId`, collects ownership evidence, and
submits an `administration` claim through the existing `/directory/claims`.
Preparation locks the existing core artist's Party row for the transaction and
reuses its canonical directory target. A post-backfill artist without a twin gets
one private draft. Existing drafts stay drafts; blocked, suspended and archived
targets remain unavailable. The response contains only the target ID and the core
artist's public name, never private directory content. No manager, credential,
role or publication is created. Loading failures offer an explicit retry.

Verification uses the existing directory administrator review queue, not an email
string match. Submitting evidence grants no access. The existing approved-claim
trigger requires the profile and claimant account to match before a manager row
can be written. Approved profiles are edited in **Mis perfiles y clasificados**;
this is a directory management grant, not a global Artist role, credential transfer,
release ownership grant, payout permission, or access to another Party's legacy
`/artists/me` editor. Those broader grants are outside this claim's scope.

The existing guest registration and own-profile activation contracts are unchanged.
The additive preparation endpoint is in canonical OpenAPI and generated clients.
Deploy the compatible backend before promoting web; existing clients retain their
original endpoints. No schema migration is needed. Rollback must retain endpoint
availability while clients that use it remain deployed.

Validation: five component tests cover submission, pending access, idempotent retry,
unavailable resolution and session changes; the onboarding regression covers both
Customer and Artist sessions without logout. The real PostgreSQL fixture
`scripts/__tests__/fixtures/artist-management-claim.sql` verifies pending/mismatched
claim denial, matching approved management, revocation, and unchanged Party,
credential and follower identities. It is also wired into the existing directory
migration test. Local execution used the complete schema and all 104 artist-branch
registered migrations, in a private cluster; this SQL fixture is not an HTTP test.

Continuation 2026-09-17: UX-260917-013 fences claim acknowledgements and own-profile
activation by the current session generation, including renewed credentials for
the same Party. Optional browser storage failure falls back to an in-memory
confirmed session. A refreshed session must contain the persisted Artist role
before opening the editor. The verified management review contract remains intact.
Two new activation tests failed before the repair; 15 activation/claim tests pass
afterward. This is component conformance, not a production activation or SMTP test.


UX-260917-014 formal scope: `ArtistClaimTarget.tla` models two competing preparation
transactions, zero or one existing twin, and an allowed or blocked target. The
row lock spans lookup, optional insert and commit; `runSqlPool` supplies the
transaction. `UniqueTarget`, `PersistedTarget`, `BlockedNotReturned` and
`NoAuthorityFromPreparation` are safety properties; weak fairness of each
transaction gives `RequestsTerminate`. Removing the row lock is a required
negative control for `UniqueTarget`. This assumes a live database, terminating
transactions and that competing preparation calls use this handler. It does not
prove unrelated profile writers, administrator reviews, HTTP delivery, arbitrary
contention or the entire application. The runtime conformance script sends eight
parallel requests against an isolated real backend/PostgreSQL database and checks
private drafts, lack of grants/credentials, blocked denial and idempotent receipts.
The existing claim-review SQL fixture separately covers authorization/revocation.

Executed verification for UX-014 (2026-09-18 UTC): Stack/GHC9.10.3 compiled the
complete backend. The production-shaped schema runner passed complete migration
and restart/idempotency checks, then `test-artist-claim-target-http.sh` passed
against that real executable and disposable PostgreSQL17. Eight concurrent PUTs
reused one private draft; anonymous preparation returned401; blocked/unknown
artists returned404; the draft's private name did not leak; repeated claims returned
the same submitted receipt. SQL asserted three twins, no publication, manager or
credential creation, and exactly one submitted claim. The fixture now runs in
backend CI after production-schema verification. 25 focused UI tests, TypeScript,
UI lint, strict catalog and 14 CI-pipeline tests passed. TLC1.7.2 explored49 distinct
states for the new model with safety and conditional liveness; the no-lock negative
control violated UniqueTarget in7 states. The complete existing TLC/Alloy6.2.0 runner
also passed its expected controls. No production claim or new native artifact is
claimed by these isolated tests.

Review follow-up (discussion_r4042542142): the same Party may own both a core
artist directory twin and a newer band. Preparation now selects only `artist`
source profiles and separately requires the canonical target to remain `artist`.
A blocked artist cannot be bypassed by selecting its band's unblocked profile;
a canonical link to a different kind fails closed without a replacement twin.
The expanded real HTTP fixture reproduced the old behavior (200 instead of404
for a blocked artist with a newer band). Positive execution is recorded below.
`ArtistClaimKind.tla` exhaustively bounds source/canonical kinds to artist/band;
`OnlyArtist` and `NoWrongSourceReuse` connect these predicates to the SQL source
filter and canonical guard. Termination assumes one fair atomic transaction.
The unfiltered/unguarded negative model must violate `OnlyArtist`. This model
does not claim to verify canonical graph integrity or administrator decisions.
