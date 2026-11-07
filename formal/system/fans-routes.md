# `/fans` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the handlers on main `f615c862`: `tdf-hq/src/TDF/Server.hs` (`fanSecureServer`), `tdf-hq/src/TDF/ServerFanClub.hs`, `tdf-hq/src/TDF/Social/FanEffects.hs`, `tdf-hq/src/TDF/Server/Catalog.hs` (role requests) and `tdf-hq/sql/2026-09-28_interaction_compatibility.sql`.

This describes what the code does today. New requirements written from it have status `implemented but unspecified`.

All 27 routes are behind `AuthProtect "bearer-token"` (`"fans" :> FanSecureAPI` inside `ProtectedAPI`).

**Shared guards**
- `requireFanAccess`: Fan or Customer role, unique roles. Used by the profile and follow routes only.
- `checkIsOfficer`: Admin or Agency role, or a `FanClubOfficer` row for the club. Used by moderation, inbox handling, events and election creation.
- `requireFanClubPostAccess`: the artist, an officer, Admin/Agency, or a follower. Used by posts and post reactions.
- The club member profile, memories, reports, inbox messages, candidacy and votes check **only** that the caller is signed in and the club exists.
- In canonical interaction mode, replies, reactions and reply hides go through the interaction command ledger (write budget, moderation).

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| profile, artist-profile, role-request | COMMUNITY-FAN-PROFILE-001 | new |
| follows create/delete | COMMUNITY-FAN-FOLLOW-001 | new |
| notifications read / read-all | COMMUNITY-FAN-NOTIFICATION-001 | new |
| member-profiles/me, memories create/react/report, inbox send | COMMUNITY-CLUB-PARTICIPATION-001 | new |
| posts create, posts react | COMMUNITY-CLUB-POSTS-001 | new |
| posts pin/unpin/hide/unhide, memories hide/unhide/delete, inbox reply/status, events | COMMUNITY-CLUB-OFFICER-001 | new |
| elections create, candidacy, vote | COMMUNITY-CLUB-ELECTION-001 | new |

No existing requirement specified any of these routes. `ART-FOLLOW-001` covers the sibling `/social-events/artists/{id}/follow` routes, not these.

## Defect fixed on 2026-11-07

**Elections accepted candidacy and votes at any time.** `FanClubElection` carries a status and optional candidacy and voting windows, but neither `createCandidacy` nor `castVote` looked at them: a party could stand or vote before the window opened, after it closed, or in a closed election. `validateFanClubElectionPhase` now refuses (409) a closed election and anything outside a set window; `lookupFanClubElectionPhaseTarget` applies it in the same transaction as the target lookup. Tested in `ServerFanClubSpec`.

## Open behaviours for the owner to decide

**Who may participate**
- Any signed-in account can enrol as a member of, publish memories into, and message any club without following it.
- Voter eligibility is not restricted (no follower or membership check) and self-votes are allowed; a changed ballot is silently discarded.
- An artist editor's follow leaves club membership, officer and candidacy rows behind when they unfollow.

**Moderation and audit**
- Memory authors cannot delete their own memories; officer hides and deletes record no reason and no audit.
- Reports reach no moderation queue and are unlimited per reporter.
- Officer inbox replies overwrite each other silently, and the fan is never notified.

**Validation**
- Fan profile bio, city and avatar URL, member profile handle, bio and avatar, and election manifestos are unbounded.
- Election windows are not validated for ordering at creation, and the election status is never advanced automatically.

**Status codes**
- Marking a foreign notification read answers 204; duplicate election years and candidacies answer 400 rather than 409.
