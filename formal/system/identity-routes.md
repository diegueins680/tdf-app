# Identity and access mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/ServerAuth.hs`, `Auth.hs`, `Server.hs` (access requests, academy, parties), `ServerExtra.hs` (bands), `FeatureRegistry.hs`, `Catalog/Security.hs` and `Profiles/Artist.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- Login, Google login, signup and password reset are public (`credential-intake`); password change is registered `handler-session` but reads only the Authorization header or a username in the body; access requests, referrals, parties and bands are behind `AuthProtect`.
- Since 2026-11-10 password login locks an identifier out for the rest of a 15-minute window after ten failures, and signup and reset requests are limited to five per email per hour (`TDF.Auth.Throttle`, `auth_rate_limit`). There is no per-address limit: the backend has no trusted client address.
- Sessions are database-clock windows (ID-SESSION-004); password replacement revokes interactive tokens (ID-SESSION-001); reset tokens are single-use within 900 seconds (ID-SESSION-002/003).

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| login, login/google, signup, v1/signup | AUTH-CREDENTIAL-INTAKE-001 | new (critical) |
| v1/password-reset, confirm, password/change (both) | AUTH-PASSWORD-RECOVERY-001 | new (critical) |
| access-requests create, cancel, decision | AUTH-ACCESS-REQUEST-LIFECYCLE-001 | new (critical) |
| referrals/claim, parties, bands | CRM-PARTY-INTAKE-001 | new |

ID-SESSION-001..004, ID-ARTIST-CLAIM-001 and AUTH-ACCESS-PROVISION-001 already hold on these routes; the decision route refuses the requester as reviewer in code, in the grant store and by trigger.

## Open behaviours for the owner to decide

**Abuse limits (critical)**
- **Decided 2026-11-10.** Login failures, signup and reset requests are throttled per identifier (see Guards). Still open: no per-address limit, so a client can try a few passwords against many accounts; anyone can lock a known identifier out for up to 15 minutes; current-password guessing on the change route and Google login are not throttled.

**Disclosure**
- **Decided 2026-11-10.** Login checks the password first and answers a disabled account exactly like a wrong password. Still open: password change answers 403 "Account disabled" for a known disabled username without a password, and signup answers 409 for an existing email.

**Boundaries**
- With a username in the body, password change needs no session, contradicting its `handler-session` registration; the Cookie header is never read.
- No audit row records a password change.

**Concurrency and data**
- A racing duplicate signup or exact-name band answers 500; a Google account can share a primary email with a password account and break email login for it.
- A referral owner may claim their own code; claims per email are uncapped.
