# Event operations implementation report — 2026-09-14

## Outcome

This branch completes the evidence/audit and bounded formal-specification phases, implements an
additive PostgreSQL foundation for the highest-risk invariants, and closes the existing authenticated
event-invitation authorization flaw. It does **not** claim the entire multi-phase end-to-end product
is complete. The remaining API, worker, web, mobile, engagement, finance, and collaboration work is
listed below and remains production-disabled/unimplemented.

Work is isolated on local branch `feat/event-operations-formal-foundation` at
`/Users/diegosaa/GitHub/tdf-app-event-operations`, based on local `origin/main` commit `17a33eca1`.
The dirty primary worktree and mobile submodule were not modified.

## Completed artifacts

- Evidence-backed repository/tool/domain audit, 62-row gap matrix, glossary, environmental/fairness
  assumptions, canonical domain, transition contracts, threat model, delivery plan, and traceability.
- ADRs selecting the existing `social_event` identity, bounded hybrid formal methods, and the shared
  resource allocation calendar.
- Six TLA+ safety models, one TLA+ liveness model, a generated PlusCal reservation-race translation,
  two booking override configurations, and one Alloy relational model with a satisfiable integrated
  scenario plus eight assertions.
- Reproducible checksum-pinned formal runner and a GitHub Actions workflow definition. The workflow
  file exists locally; GitHub did not run it in this environment.
- Additive migration for canonical lifecycle sidecar/mapping, owner/coproducer relations, scoped
  time-bounded grants, migration issues, immutable revisions/command receipts/audit/transitions,
  multi-session IANA scheduling, secure invitation-token metadata, and RACI/task policy/override.
- Existing logistics dependency mutation now receives an event-scoped advisory lock and database
  cycle guard. A two-transaction reverse-edge race is covered by the PostgreSQL test. Completion for
  opted-in event-operation tasks requires RACI and completed dependencies unless a version-bound,
  reasoned, policy-referenced immutable override exists.
- Existing event invitations now enforce organizer/admin creation, `pending` initial status,
  recipient-only list disclosure for non-managers, and recipient update attenuation. Recipients may
  accept/decline but cannot transfer the target, rewrite messages, or make later conflicting status
  transitions.
- Restored the repository's previously implemented but later lost atomic/idempotent moment-reaction
  helper and its engagement evidence write. Also corrected the pre-existing `Text`/`Maybe Text` DTO
  projection mismatch that prevented this module from compiling.

## Verified evidence

| Command/evidence | Result |
|---|---|
| `npm run verify:event-operations:formal` with pinned temporary Java/TLC/Alloy paths | PASS; all seven TLC configurations, satisfiable Alloy scenario, eight Alloy assertions with no bounded counterexample |
| `npm run test:event-operations-foundation-migration` | PASS on ephemeral PostgreSQL 16; apply twice, mappings, owner issue, timezone, RACI, sequential and concurrent DAG rejection, completion/override, invitation digest uniqueness, immutable audit, rollback, reapply |
| linked `tdf-hq-test --match 'social event handler helpers'` | PASS; 31 examples, 0 failures, including invitation create/update/list authorization regressions |
| linked `tdf-hq-test --match 'records moment-reaction additions atomically and retains evidence after removal'` | PASS; 1 example, 0 failures |
| `git diff --check` | PASS at the time recorded; no whitespace errors |
| `bash -n scripts/verify-event-operations-formal.sh` and `sh -n scripts/test-event-operations-foundation-migration.sh` | PASS |
| `package.json` JSON parse | PASS |

Detailed TLC state counts, model bounds, fairness, counterexample corrections, checksums, and exact
runner command are in `formal/event-operations/README.md`.

The targeted Haskell suite required a clean rebuild of 203 modules and linked successfully. Stack
then failed in its package copy/register phase because it attempted to copy an absent, unrelated
`tdf-hq-exe` artifact. The newly linked Hspec binary was run directly with the two filters above and
passed. This is not represented as a successful end-to-end `stack test` command. Existing full UI,
mobile, E2E, backend, performance, and accessibility suites were not run as part of this foundation
report.

## Failed/adjusted checks retained as evidence

- A normal Homebrew OpenJDK installation failed because the host Apple Command Line Tools were too
  old. Homebrew auto-updated itself and upgraded `cmake` to 4.4.3 before that failure. No Xcode tools
  were removed/installed. Verification used the already-downloaded OpenJDK bottle extracted under
  `/private/tmp`.
- TLC inside the sandbox failed because of its local RMI socket. Approved out-of-sandbox runs passed.
- Parallel TLC processes collided in the release's shared temporary standard-module path; the runner
  is intentionally sequential.
- The first Alloy scenario was UNSAT due to an overstrong task locality fact. The fact was corrected
  from “every task has dependencies” to “any dependencies are event-local”; the scenario then became
  SAT and assertions remained without counterexamples.
- The first migration fixture lost quoted SQL through shell escaping; it was replaced by a tracked
  fixture SQL file. A session visibility default then contradicted its own constraint and was changed
  from `team` to canonical `internal`. The final clean-container run passed.
- The clean baseline could not compile the social-events test component because
  `EventMomentReactionDTO.emrPartyId` expected `Maybe Text` while the projection supplied `Text`, and
  `ServerSpec` imported a missing `toggleMomentReactionDb` implementation/export. Git history showed
  that implementation had previously existed. It was restored, the projection was corrected, all
  203 test modules linked, and the focused tests above passed.
- The targeted `stack test` wrapper still returned nonzero after linking because Cabal's copy phase
  expected an executable artifact not built by that target. This package-target defect remains a
  baseline CI/tooling issue; the linked test executable itself passed the recorded filters.
- `npm run quality:repo` completed 41 of 42 continuous-improvement-loop tests and failed only when
  loading `tdf-hq-ui/jest.config.cjs`: `@testomatio/reporter/jest` is declared in `package.json` and
  the root lockfile but absent from `node_modules` in both this clean worktree and the primary
  worktree. No dependency installation or test bypass was performed. The formal audit inside that
  command still passed with 0 critical findings and 0 errors.

## Remaining implementation

- Wire the lifecycle/command/relationship/grant tables into typed Servant APIs with a disabled-by-
  default `event.operations` flag, expected-version checks, rejected-command audit, and generated web/
  mobile clients.
- Replace legacy organizer-null claiming with a reviewed ownership migration/cutover.
- Implement secure external invitation issue/accept/revoke/expire with token digest, account linking,
  command receipts, notification delivery, and replay tests. The new security table is foundation,
  not a complete guest flow.
- Evolve logistics APIs/UI to write the new RACI/task policy atomically; add workstreams, subtasks,
  checklists, recurrence, templates, typed production/logistics requirements, approvals, evidence,
  and all requested views.
- Bind party/venue/room/asset/provider identities to the existing canonical resource/exclusion
  calendar; implement buffer/capacity/multi-resource transaction tests and availability UX.
- Add the event-linked opportunity/proposal/engagement lifecycle, explainable ranking, immutable
  contracts/acceptances/milestones, cancellation/replacement/dispute handling, and verified review
  linkage. Disable/remove fake contract “sent” success before exposure.
- Couple engagement settlement to existing checkout/provider inbox/refund/dispute/ledger primitives.
  Payouts and providers stay disabled until sandbox verification and independent legal/accounting/
  security approval.
- Complete scoped discussions, notification preferences/quiet hours/retries/DLQ, calendars/iCal,
  webhooks, offline command synchronization, observability, Spanish/English accessible web/mobile
  workspaces, performance tests, migration rehearsal, recovery, and operational runbooks.

## External limitations and prohibited actions

`git remote show origin` could not resolve the GitHub SSH host and `gh auth status` reported invalid
credentials. The work is committed only on the local feature branch; no remote branch, push, PR, CI
run, review, or merge is claimed. No screenshots were produced. No production database, deployment,
feature flag, credential, payment, refund, or payout was touched. The new migration is not added to
the production manifest; that is a later reviewed rollout step after API/client compatibility and
release rehearsal.
