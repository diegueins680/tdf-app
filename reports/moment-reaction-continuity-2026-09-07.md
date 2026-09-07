# Moment reaction onboarding continuity — 2026-09-07

## Outcome

`moment_reaction` onboarding completion now fails closed unless the authenticated Party has a durable, in-window reaction on a moment belonging to an event that is still present in the public directory projection. The completion endpoint remains idempotent, so repeated or delayed mobile handshakes cannot create duplicate conversions.

Mobile persists a pending first-value completion under a Party-scoped key before calling the completion endpoint. A failed handshake is retried when the authenticated Party's first-run state is loaded again, and acknowledged state is removed only for the same Party and value. Invalid local values are discarded without being sent to the API.

## Authoritative reaction boundary

The server verifier joins `event_moment_reaction` to `event_moment` and `directory_public_event`, then requires all of the following:

- `reactor_party_id` matches the authenticated Party;
- the reaction was created no earlier than the Party's recorded signup completion;
- the reaction timestamp is not in the future relative to the completion request; and
- the reaction's parent event is currently in the public directory projection.

Missing, cross-Party, pre-signup, future-dated, and non-public-event records leave onboarding eligible and incomplete. A valid record completes once; later calls return `newlyCompleted: false` while preserving the original first-value label.

## Mobile truthfulness and retry

The event-moment repository now returns whether the selected reaction is present in the server-returned moment. The treatment gate asks for completion only after a remote acknowledgement that leaves the authenticated Party's reaction selected. A local fallback and a remote deselection remain valid domain interactions but do not claim onboarding conversion.

If the domain write succeeds and the completion request fails, the pending label survives app restart under the authenticated Party's key. `FirstRunProvider` retries it after loading authoritative eligibility. Active-Party guards prevent a response racing with account switching from clearing or attributing the prior Party's queued state. A replay that atomically completes onboarding emits the same one-shot conversion analytics; a server response with `newlyCompleted: false` emits no conversion.

## Verification

- Backend focused Hspec covers missing, cross-Party, pre-signup, future, non-public-event, valid, and repeated completion cases.
- Mobile focused Jest covers Party-scoped persistence, retry cleanup, invalid-state rejection, remote selected-state detection, local-fallback suppression, and replay conversion analytics.
- Full mobile Jest passed: 66 suites and 352 tests, followed by successful TypeScript, ESLint, and release-profile checks.
- Repository quality, 50 production-release tests, 16 CI-scope tests, and the strict catalog-list audit passed.

## Scope and residual risk

This slice does not activate or change experiment assignment. The onboarding experiment remains paused until assignment and exposure semantics are server-authoritative and operational activation criteria are explicitly approved. No deployment, production mutation, merge, or experiment activation is part of this change.
