# Draft PR: restore mobile auth truth and ticket-checkout continuity

## Problem

The mobile client could retain a phantom authenticated state when the authoritative `/session` response was JSON `null`. An anonymous buyer opening ticket checkout could also remain behind an authentication spinner instead of understanding the event and continuing through signup/login.

## Changes

- Treat a successful null session as revocation and clear the API token, party/session state, query cache, SecureStore, and legacy storage.
- Load public event context independently from authentication.
- Distinguish auth loading from anonymous state and offer Create account, Existing account, and Back actions.
- Preserve the requested event ID in the authentication return target.
- Add regression tests for both state transitions.

## Verification

- Focused Jest: 2 suites / 31 tests passed.
- Full Jest: 64 suites / 316 tests passed.
- TypeScript check passed.
- ESLint passed with zero warnings.

These are mocked/source checks. No iOS/Android device or simulator, virtual keyboard, safe-area, orientation, enlarged-text, screen-reader, real backend, or payment-provider flow was executed.

## Risk and rollback

No dependency, schema, payment, role, or business-policy change. The main risk is navigation behavior that still needs physical-device validation. Revert commits `10d5dc9` and `3c132ff` to roll back.

The parent integration and full audit are in [tdf-app draft PR #238](https://github.com/diegueins680/tdf-app/pull/238). Nothing in this PR deploys or performs a real transaction.
