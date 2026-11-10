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
modeled database clock may move forwards or backwards within the parameter
domains below. Tokens can be revoked.

`Safe` = `WithinWindowAtUse` (every accepted interactive use is before its expiry)
`/\ LegacyInteractiveFailsClosed` (an interactive token without expiry is never
accepted) `/\ TtlBoundedByCookie` (every interactive window is 1 to Max-Age
seconds) `/\ ResetNeverAuthenticates`.

| Obligation | Tool | Result |
|---|---|---|
| Bounded exploration: 3 tokens (one legacy), Max-Age 3, TTL 2, clock 0..5 | TLC 1.7.2 | no violation; owner-reported 2,046,828 states (count not independently verified from retained hosted logs) |
| Token without expiry accepted (pre-enforcement) | TLC | `Safe` violated, as expected |
| TTL above Max-Age admitted at startup | TLC | `Safe` violated, as expected |
| `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′`, `IndInv ⇒ Safe` | Apalache 0.62.4 | **proved** for up to four tokens, any legacy subset, Max-Age and TTL in 1..100000 with TTL ≤ Max-Age, and clock readings 0..MaxTime with MaxTime in 0..1,000,000, including backwards steps |
| A minted session is accepted (non-vacuity witness) | Apalache | reachable, as expected |
| Token without expiry accepted: reachable violation | Apalache | counterexample found, as expected |

The step from the model to the code is by review: one validity check per
authenticated request (`lookupUsernameFromToken` and the `AuthProtect` handler).
The cookie itself, and the browser forgetting it, are outside the model.

The SessionExpiry model/CFG inputs above retain their actual hosted execution at
source `182ae5e2437756bea7edf28a5f13898eec040627` ([verify job](https://github.com/diegueins680/tdf-app/actions/runs/42313255678/job/128219591685)); this composition does not relabel it as a new execution. The unchecked-TTL negative control is run in TLC only; the missing-expiry control is checked in both tools.

The model checks upper expiry, legacy rejection, TTL bounds and reset-token exclusion.
It does not encode the stronger issued-at lower bound or malformed-window denial
of the complete runtime predicate introduced in PR #690. The separate source-derived
authorization/recovery projections also do not establish that full helper or
whole-session refinement; native and PostgreSQL witnesses keep their own identities.

## Meta webhook admission (`AUTH-WEBHOOK-001`)

`meta_webhook_admission.py` (Z3 4.13.3, pinned) encodes
`verifyMetaWebhookSignature` and `parseMetaWebhookSignature` of
`tdf-hq/src/TDF/API/Types.hs` as a decision over the configured secret, the
`X-Hub-Signature-256` header and the raw body. It refuses to run when the text
of those two functions changes (the pin is the hash of that region, not of the
module).

| Obligation | Result |
|---|---|
| Admission needs a present, nonblank secret, a header passing all four syntactic guards and a digest equal to HMAC(secret, exact body) | proved (UNSAT of the negation) |
| Admission is exactly that conjunction | proved |
| An absent or blank secret answers 503 whatever the header says | proved |
| With a usable secret the only refusal is 401; admit, 401 and 503 are exhaustive | proved |
| An admitted header is refused for a body or secret whose HMAC differs | proved |
| Admit, 401 and 503 each reachable (non-vacuity) | witnesses found |
| Dropping the blank-secret check, any syntactic guard or the digest comparison | each breaks at least one proof |

HMAC-SHA256 is an uninterpreted function: the model states which digest the
verifier demands, not that the digest is unforgeable, and it does not cover the
constant-time comparison, provider replay or key rotation. That every Meta
callback route calls the verifier before decoding is the structural gate
`scripts/check-webhook-signatures.py`; behaviour of the real function is the
QuickCheck suite `tdf-hq/test/TDF/MetaWebhookSignatureSpec.hs`.
