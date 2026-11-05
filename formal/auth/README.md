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
