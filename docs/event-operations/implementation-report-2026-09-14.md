# Event operations implementation report — 2026-09-14

## Outcome

The dependency-ordered local branch chain completes the evidence/audit and bounded formal-
specification phases, implements an additive PostgreSQL foundation for the highest-risk invariants,
closes the existing authenticated event-invitation authorization flaw, and adds the first typed,
authenticated lifecycle API slice. It does **not** claim the entire multi-phase end-to-end product is
complete. Public lifecycle effects, workers, workspace UI, mobile, engagement, finance, and
collaboration work listed below remain production-disabled or unimplemented.

Work is isolated at `/Users/diegosaa/GitHub/tdf-app-event-operations`. The first local branch,
`feat/event-operations-formal-foundation`, is committed at `cde0e806b6a5365e1e9f5b4052b74e38ea1aa232`
from local `origin/main` commit `17a33eca1`. The dependent API work is on
`feat/event-operations-api-foundation`, committed at
`b6ba963623861d1e2804b18adb46e6e4f5f5be80`. The next dependent hardening work is on
`feat/event-logistics-transaction-hardening`. The dirty primary worktree and mobile submodule were
not modified.

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
- Added a persistent `event.operations.api` feature flag that is disabled by default, cannot be
  enabled without an actor and reason, and has immutable change history. A separate implementation-effect allowlist permits only five early
  planning/review/approval edges whose effects are confined to canonical state and immutable audit.
- Added a single PostgreSQL transition command boundary that rechecks the feature gate and
  contextual time-bounded authority inside the transaction, locks event state, rejects stale
  versions, applies global command-key idempotency, enforces independent approval, and records
  accepted, rejected, and conflicting commands. Publish and every cross-domain transition remain
  fail-closed with `transition_effects_not_ready`.
- Added authenticated Servant snapshot/transition routes, strict JSON DTOs for all 14 canonical
  states, explicit domain-error mappings, the matching OpenAPI contract, regenerated web types, and
  a small typed web client. The snapshot exposes only the caller's active capabilities and currently
  executable transitions.
- Hardened the existing social-event logistics handler rather than introducing another task store.
  Activity create/update, optimistic version compare-and-swap, assignment replacement, and dependency
  replacement now execute in one database transaction. A constraint or DAG failure restores the
  entire prior snapshot instead of exposing a partially updated plan.

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
| `npm run test:event-operations-api-migration` | PASS on ephemeral PostgreSQL 16; disabled default and activation guard, immutable flag history, contextual read, invalid-command/conflicting-key audit, exact replay, actor-changing global key conflict, version conflict, effects gate, reason guard, separation of duties, concurrent transition, rollback, reapply |
| linked `tdf-hq-test --match 'event operations executable API contracts'` | PASS; 5 examples, 0 failures, including all 14 states, strict request decoding, and stable non-success error statuses |
| local `openapi-typescript` 7.10.1 generation | PASS; OpenAPI parsed and web type artifact regenerated |
| focused TypeScript compile of `eventOperations.ts` and generated types | PASS with strict mode, ES2022, bundler resolution, DOM libraries, React JSX, and Vite client types |
| `npm run test:event-operations-foundation-migration` after logistics hardening | PASS on ephemeral PostgreSQL 16; additionally proves whole-transaction rollback for a cyclic update and cross-event dependency during create |
| `stack build tdf-hq:exe:tdf-hq-exe --fast --ghc-options=-fno-code --no-copy-bins` | All 187/187 modules typechecked, including `SocialEventsHandlers` and aggregate server; command intentionally interrupted with exit 130 when Cabal began an identical second pass because `-fno-code` produced no copy artifact. Not claimed as a passing wrapper command. |

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
- The declared worktree `npm run generate:api:ui` command could not find `openapi-typescript` because
  dependencies are incomplete. The already installed generator from the untouched primary worktree
  ran against this branch successfully. Regeneration also brought the previously stale generated
  client into line with other OpenAPI paths already in `main`, causing a larger mechanical diff than
  the event-operations-only additions.
- Full UI TypeScript checking, using the two existing dependency trees without installing packages,
  reached only two unrelated baseline errors: `LoginPage.tsx` imports missing
  `markWebSignupCompleted`, and `AppShell.tsx` imports missing
  `retryPendingFirstValueCompletion`. The focused new-client compilation passed. The mobile API
  generator was honestly skipped by its repository guard because the mobile submodule/install is
  incomplete.
- The API-target Haskell build compiled and linked all 208 test modules. As in the first branch,
  Stack then returned nonzero only in copy/register because the separately absent `tdf-hq-exe`
  artifact was not built by the requested target; the linked focused Hspec run passed.
- The first hardened API-migration run failed before commit because the immutable flag-history trigger
  referred to `event_operation_reject_mutation` instead of the foundation's actual
  `event_operation_reject_history_mutation` helper. The name was corrected and the complete clean-
  container test then passed; the failed run did not apply the migration to any persistent database.

## Remaining implementation

### Fourth-branch task transaction correction

`feat/event-task-commit-invariants` adds a checked `TaskCommit` model (121 generated / 112 distinct
states, depth 5), two checked expected-counterexample mutation configurations, and a new reversible
SQL migration. Final-state checks cover completed-task dependencies after relation replacement,
policy activation, prerequisite reopening and version-bound overrides. A per-event write fence
prevents concurrent RACI removals from each counting the other's uncommitted assignment. It also
rejects generic protected-policy deletion/weakening and protected task deletion/movement.

The full pinned TLC/Alloy runner, foundation migration, lifecycle API migration, and new task-commit
migration tests all passed. The latter uses deterministic observed database-lock barriers under
READ COMMITTED, REPEATABLE READ and SERIALIZABLE; tests both orders of completion/dependency races,
immediate/deferred checks, incompatible-data migration refusal, rollback twice and reapply. Its
foundation-only negative control first reproduced the old completion/dependency bypass. Details
and operational limitations are in [PR 04](pr-04-task-commit-invariants.md).

No Haskell/API/UI/mobile code changed in this branch. No new HTTP/E2E, payment sandbox, browser,
accessibility or large-plan performance result is claimed. Task-sidecar exposure remains blocked
on the contextual authorization, time-window, history and HTTP conflict handling still listed below.

### Pending product work

- Extend the typed lifecycle API beyond the five safe early edges only as each ticket, booking,
  contract, notification, public-visibility, and financial effect gains an atomic/outbox
  implementation and an executable failure/compensation test. Add HTTP-level authorization and
  database integration tests once the repository's API test harness can boot the migrated schema.
- Replace legacy organizer-null claiming with a reviewed ownership migration/cutover.
- Implement secure external invitation issue/accept/revoke/expire with token digest, account linking,
  command receipts, notification delivery, and replay tests. The new security table is foundation,
  not a complete guest flow.
- Evolve logistics APIs/UI to write the new RACI/task policy atomically; add workstreams, subtasks,
  checklists, recurrence, templates, typed production/logistics requirements, approvals, evidence,
  and all requested views. Existing activity/assignment/dependency writes are now one transaction,
  but hard delete/history, RACI projection, and external route-verification side effects still need
  the reviewed canonical command path.
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

Initially `git remote show origin` could not resolve the GitHub SSH host and `gh auth status`
reported invalid credentials. During the fourth branch, read-only GitHub access recovered after
sandbox escalation; both GitHub API and Git SSH confirm `main` remains the audited base. Remote
publication evidence is recorded separately when verified; local checks are not hosted CI or review.
No screenshots were produced. No production database, deployment,
feature flag, credential, payment, refund, or payout was touched. The new migration is not added to
the production manifest; that is a later reviewed rollout step after API/client compatibility and
release rehearsal.
