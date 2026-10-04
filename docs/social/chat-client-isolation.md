# Chat client isolation — 2026-09-15

Depends on [DM read/API PR #390](https://github.com/diegueins680/tdf-app/pull/390).
The backend is the authorization boundary; this web repair prevents stale browser
display after an observed denial and separates accounts on a shared device.

## Changes and traceability

| Defect | Mechanism | Regression coverage |
|---|---|---|
| Chat lists/messages and navigation unread badge reuse another account's query cache | Include signed-in party in all chat and associated profile query keys; remount workspace/drafts on account switch | `ChatPage.privacy.test.tsx`: account-switch case retains one QueryClient and verifies old body/preview/draft disappear |
| React Query retains old successful data after a failed refetch | Do not render stale previews, names, bodies or enabled composer on an error; remove errored conversation from shared thread cache; require current thread membership before fetching messages | Thread-refetch and message-refetch denial cases include header, preview, body, badge and disabled send button |
| Shared browser read markers mark another account's thread as read | Party-scoped localStorage keys and storage-event filtering; selected-thread preference also scoped | `chatReadState.test.ts`: separate marks for two participants in one thread; no inheritance from unknown legacy owner |
| Delayed send result clears/invalidates newly selected conversation | Mutation carries its thread ID; completion uses submitted variables | Existing send contract is retained; current-send component assertion |

No message content is newly persisted in browser storage. Old unscoped read markers
are left intact for compatibility, but never copied into an account because their
owner is unknown; some conversations may initially appear unread after upgrade.
No schema migration, API contract change, graph-derived grant or contact import.
Spanish copy and existing MUI controls remain. Existing keyboard/label behavior is
retained; this slice has component evidence, not a new native or full-app E2E claim.

The server may authorize an earlier overlapping snapshot. A device cannot learn of
a remote block until it revalidates; existing thread/message polling intervals are
10s/3s when active, subject to browser scheduling and connectivity. This change does
not promise remote deletion of delivered data. On any fetch error, display fails
closed, including transient network errors; a later successful poll can restore
access. Other legacy profile/search endpoints remain outside this repair.

## Research and flags

First-party [TanStack Query v5 query keys](https://tanstack.com/query/latest/docs/framework/react/guides/query-keys),
accessed 2026-09-15 (update date unavailable), describes keys as cache identities
and requires dependent variables in the key. The account scope and denied-display
behavior are TDF-specific inferences; tests use the real QueryClient.

This compatibility/security repair applies to the existing chat page; it does not
activate the gated social preview or change consent authority. All new production
social flags remain off. Roll back application UI separately while retaining the
minimum safe backend reader/trigger from #390/#386. Reintroducing the shared cache
keys would restore the demonstrated UI defect. Account-scoped read markers are
additive and can be retained across a UI pause; there is no destructive rollback.

## Verification and remaining scope

- 28 focused component/read-state/selector tests passed locally. Earlier cold initial
  render exceeded the new fixture's 5s wait; a 15s async/30s example allowance kept
  every privacy assertion, with the subsequent complete run passing. No CI assertion
  was removed. The final child-worktree run, including the lint-only assertion syntax repair,
  passed all 28 tests.
- Focused lint, unchanged catalog audit and repository application typecheck
  (`npm run typecheck --workspace tdf-hq-ui`) passed, exit 0.
- An additional `tsc -p tsconfig.json` run (including all test files) failed with 134
  diagnostic output lines in unchanged test files, none in these changed files.
  This is an observed failed extra check, not a baseline execution or a passing gate.
  The repository uses `tsconfig.app.json` for its application typecheck.
- Remaining: canonical connection selector integration (legacy new-chat choices still
  say “Amigo mutuo”), versioned thread pagination, message idempotency, native client
  behavior, real authenticated full-app journeys and all global handoff blockers.

No measured collaboration/booking/sales gain is claimed. No merges, deployment
commands, production flag activation or production experiments are part of this PR.
The earlier automatic-provider deployment exception remains in [handoff](handoff.md).

## Hosted verification

At implementation/evidence head `8b7daf9c81da8e2c1d1fe8f785371fb33d174078`,
[social CI](https://github.com/diegueins680/tdf-app/actions/runs/35052928324)
passed both model/PostgreSQL and social-client jobs after the readiness repair.
[Full CI](https://github.com/diegueins680/tdf-app/actions/runs/35052928363) passed
UI quality, persona journeys, repository quality, contracts, production migrations
and the aggregate gate. Backend/mobile/migration-test jobs were skipped by this
UI-only diff and are not counted as passes; backend verification belongs to #390.
The build check and catalog job also passed. Exact check snapshot:
`evidence/chat-client-isolation/ci-8b7daf9c8.json`. Later evidence-only commits
should not be mistaken for new implementation checks.
