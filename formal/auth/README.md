# Authentication proofs (AUTH-LOGOUT-001)

## Browser logout intent

`LogoutIntent.tla` models `tdf-hq-ui/src/session/logoutIntent.ts`: an explicit
logout begins an intent with a fresh revision and a pending server call; the
call finishes the intent as confirmed or unconfirmed; only fresh interactive
authentication clears it, again with a fresh revision. Callbacks carry the
revision they started with and may arrive at any time, including after a later
begin or clear.

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration, up to 4 revisions | TLC 1.7.2 | no violation |
| Unfenced finish (negative control): begin, fresh login, stale callback marks the new login as unconfirmed | TLC | `Safe` violated, as expected |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe`, up to 8 revisions, any number of callbacks | Apalache 0.62.4 | **proved** |
| Unfenced protocol: reachable violation | Apalache | counterexample, as expected |

`Safe` = restoration is suppressed exactly from an explicit logout until fresh
authentication (`loggedOutSinceAuth ⇔ hasLogoutIntent`), and revisions are never
reissued. The correspondence to the TypeScript module is by review:
`finishLogoutIntent` writes only when `ownsLogoutRevision`, and `clearLogoutIntent`
issues a fresh revision. Browser storage failures (memory-only intent, the
`unavailable`/`invalid` revisions) are outside the model; the existing Vitest
suites cover them.

Run: `npm run verify:inductive-proofs`.

## Interactive session expiry (ID-SESSION-004)

`SessionExpiry.tla` models bearer tokens and their validity window on the database
clock: `TDF.Auth.apiTokenWithinValidity`, `TDF.ServerAuth.sessionValidityWindow`
(expiry = issue time + max(1, TTL)) and `TDF.Config.validateSessionTokenTtl`
(startup refuses a TTL above the cookie Max-Age). Tokens are interactive
(password or Google login), service, or password reset. Legacy interactive
sessions exist from the start without an expiry and are not backfilled. The
database clock may move to any value, including backwards. Tokens can be revoked.

`Safe` = `WithinWindowAtUse` (every accepted interactive use is before its expiry)
`/\ LegacyInteractiveFailsClosed` (an interactive token without expiry is never
accepted) `/\ TtlBoundedByCookie` (every interactive window is 1 to Max-Age
seconds) `/\ ResetNeverAuthenticates`.

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration: 3 tokens (one legacy), Max-Age 3, TTL 2, clock 0..5 | TLC 1.7.2 | no violation (2,046,828 states) |
| Token without expiry accepted (pre-enforcement) | TLC | `Safe` violated, as expected |
| TTL above Max-Age admitted at startup | TLC | `Safe` violated, as expected |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe` | Apalache 0.62.4 | **proved** for up to four tokens, any legacy subset, Max-Age and TTL in 1..100000 with TTL ≤ Max-Age, and any clock |
| A minted session is accepted (non-vacuity witness) | Apalache | reachable, as expected |
| Token without expiry accepted: reachable violation | Apalache | counterexample found, as expected |

The step from the model to the code is by review: one validity check per
authenticated request (`lookupUsernameFromToken` and the `AuthProtect` handler).
The cookie itself, and the browser forgetting it, are outside the model.
