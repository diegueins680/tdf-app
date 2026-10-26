# COMMUNITY-PROFILE-001 — Edit only an owned, loaded profile

Authority: the product owner's 25 October 2026 user-value completion instruction
prioritizes verified data-loss defects; ST-004 in
`docs/persona-testing/journey-specifications.md` requires authoritative profile
read/write, distinct loading/error/empty states and safe account transitions.

## Problem and boundary

`FanHubPage` and `useFanProfile` previously initialized blank forms, accepted
replacement writes before their profile GET succeeded, and retained the previous
account's fields when the account changed. The artist update in
`TDF.Profiles.Artist` replaces nullable fields and genre memberships. A failed
read must therefore never be interpreted as permission to save an empty profile.

## Contract

1. Enable editing and saving only after a successful profile read whose returned
   Party matches the current session Party. An authoritative empty DTO for that
   Party is a valid baseline; missing or mismatched data is not.
2. Keep artist and fan drafts in memory with an account generation. On account
   change, synchronously hide the old fields and block saves until the new
   account's baseline has been initialized. Returning to the same account must
   not revive callbacks from an earlier generation. Each generation captures the
   synchronous authentication epoch: login, logout or credential replacement
   revokes old callbacks immediately, before React commits a new session scope
   or unmounts the previous editor.
3. Guard the save handler as well as its buttons and inputs. Failed reads offer
   an explicit retry; retry reads must not create a profile write.
4. Do not replace locally edited fields when a background read settles. On save
   completion, a fresh read may normalize the draft only if the user has not
   edited it since that save began.
5. Capture the submitting account generation in asynchronous upload and save
   callbacks. An old generation must not change the active draft, invoke its
   saved callback or display its success/error message. Cache invalidation uses
   the submitting Party, not the Party active when the response arrives. Successful
   saves also refresh shared public discovery/catalog/profile caches, even if the
   submitting account is no longer active; that must not refetch another account's
   private editor or display the old account's receipt.

The shared `useOwnedProfileDraft` hook implements draft ownership and hydration;
the fan hook and artist editor admit their own DTOs and retain their domain
validation. Session credentials, endpoint authorization, provider admission and
persisted profile formats retain their existing contracts.

## Acceptance and evidence

Rendered editor and real QueryClient regressions cover pending/error reads,
explicit retry, one-field edits preserving all other values, account changes,
late reads/saves/uploads and background-read conflicts. Real `SessionProvider`,
`SessionQueryProvider`, fan endpoints, API client and authorization-header
regressions intercept Fetch to verify zero old-profile dispatches with a new
principal's bearer during a batched login and after same-Party reauthentication.
These checks include recovery through the new login's successful baseline. Negative controls run
the new assertions against protected main before the repair. A disposable
API/PostgreSQL/browser run checks that failed-read actions cause zero writes and
that recovered saves persist the intended edit while retaining the other fields.
Hosted checks, independent review, protected merge and deployed verification
remain separate from local evidence.

This is a client data-loss prevention contract, not proof of universal
transaction-fenced session revocation. Already dispatched requests retain their
captured HTTP credentials and the server's existing authorization semantics.
Concurrent editors on different devices still have no optimistic version fence;
remote changes and local dirty drafts require a separate conflict-resolution
design. No provider upload, production data mutation or financial transaction is
needed to test this repair. Rollback is a reviewed web revert; no migration or
data deletion is required.
