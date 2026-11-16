# `/directory` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server/Directory.hs`, `Directory/Policy.hs`, `API/Directory.hs` and `tdf-hq/sql/2026-08-14_music_directory_core.sql` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- All 23 routes are behind `AuthProtect "bearer-token"`.
- `requireProfileCapability`: an active `directory_profile_manager` row for the caller with the named capability (edit, publish, contact); 404 otherwise.
- `requireDirectoryAdmin`: Admin role and ModuleAdmin.
- `requireAdult`: age assurance (adult attested or verified; a minor needs a guardian).
- `reserveIdempotency`: actor-scoped (actor, operation, key) with a request fingerprint; 409 on reuse with a different body; 24-hour expiry. `consumeRate`: per-day counters (429).
- Search documents are refreshed by SQL functions; notifications are rows; no email or storage IO.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| `PATCH admin/claims/{claimId}/status` | ID-CLAIM-REVIEW-001 | reviewed |
| `POST saved-searches` | DIR-SAVED-SEARCH-001 | reviewed |
| `POST claims` | ID-ARTIST-CLAIM-001 | reviewed |
| age-assurance | DIR-AGE-ASSURANCE-001 | new |
| profiles create/update/status | DIR-PROFILE-AUTHORING-001 | new |
| classifieds create/status | DIR-CLASSIFIEDS-001 | new |
| applications, invitations, contact | DIR-ENGAGEMENT-001 | new |
| reviews, favorites | DIR-REVIEWS-FAVORITES-001 | new |
| artist-claim-targets, verifications, reports | DIR-SUBMISSIONS-001 | new |
| admin verifications status, moderation decisions, merges | DIR-ADMIN-MODERATION-001 | new (critical) |

`SYS-STATE-001` binds the profile, classified, application and invitation state machines structurally; it does not state who may transition them or with what effects.

## Open behaviours for the owner to decide

**Identity and visibility**
- Any authenticated user can prepare an artist claim for any legacy artist party, which creates a draft directory identity for it. Because a directory identity always wins over the legacy profile (ART-PUBLIC-VISIBILITY-001), the legacy artist disappears from public search until someone publishes the twin.
- Profile merges jump straight to executed with requester = approver, without checking that source and target share a subject, kind or reviewed plan, and without reconciling the source's managers onto the target.
- Any account, including one already verified or restricted, can re-attest itself adult and overwrite its age-assurance row.

**Moderation and review**
- Verification decisions have no self-review guard, no state machine and no audit, and the verified badge is public.
- A moderation decision replay re-runs the case update and the target mutation; closed cases can be re-decided; profile suspension bypasses the declared profile transitions.
- Reports are not checked against an existing target, and a second report by the same reporter with the same reason answers 500.

**Concurrency and status codes**
- Profile edits have no optimistic concurrency and ignore the profile status (a merged or suspended profile can still be edited).
- Profile, application and invitation status changes read then write without a lock or conditional update.
- Duplicate profile and classified slugs answer 500; an unknown profile on claim creation answers 500.

**Limits**
- Claims, verifications and reports have no rate limit or per-target uniqueness.
- A rejected classified body consumes its idempotency key.
- Notifications are written outside the creating transaction.
