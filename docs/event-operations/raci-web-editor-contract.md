# RACI web editor contract — before implementation

Dependency: draft PR 387, exact head `13abde1cba82b7b79c21b6f35e87c91de676366e`.
Audit: canonical task view is deliberately read-only; PR 387 supplies authorized, paginated
options and PR 384 supplies the idempotent command. Reuse those boundaries, without names,
directory requests, new permissions, schema changes or production activation. Remote default
branch verified as main at `0784958ecb8916106ab21ae3121f50893b71aefc`; this increment stays
on the dependency branch, not an unreviewed merge of current main. Local React/MUI/TypeScript,
Jest, Playwright, Git, Node, Stack, Docker and pinned TLC/Alloy tooling are available.

EW01: only explicit opening fetches editor context. Display options only from its validated
current-session/exact-target result. Source and recipient selections come from the latest
page; exclude source and already-visible same-role assignments. Do not infer authority from
the older task snapshot, global role, people lookup or an assignment. Server rechecks everything.
EW02: each page replaces the prior page and clears selections/confirmation. Never accumulate
a mixed-revision roster. Read errors show retry, not permissive defaults. No automatic polling.
EW03: a nonempty justified single-pair change requires a separate confirmation dialog showing
event/task, source/target, role, reason and exact aggregate revision. Freeze body/key at review.
Only the confirm button dispatches. Editing/cancelling review cannot mutate the server.
EW04: one synchronous in-flight guard prevents double dispatch. No optimistic RACI or success.
Success requires the existing strict target/key/body/revision-bound command receipt. Only a
current session/target generation may consume responses. Unmount cancels/ignores late work.
EW05: initial explicit HTTP 409 is a conflict, not success; discard options and require an
explicit fresh read and new review. No silent rebase. Any other failed/invalid response is
conservatively uncertain. An explicit retry keeps the frozen key/body/revision exactly; after
uncertainty, even a later 409 does not prove the first attempt failed. No automatic retries.
EW06: retain uncertain request details in memory for the mounted context and show the key.
Disable local back/refresh and dialog cancellation during sending/uncertainty; warn on browser
unload and explicitly explain that navigation/logout loses local recovery details. No persistence
of tokens, reasons or receipts. Other shell navigation, process crashes and cross-tab cookies
are not a durable offline recovery solution. Session changes always clear restricted data.
EW07: Spanish default and English fallback; native labeled selects, validation instructions,
keyboard-accessible MUI confirmation/focus restoration, status/alert feedback. IDs identify
existing parties honestly; no invented names, consent, invitation, booking or notification.

## Formal/executable boundaries

`RaciWebEditor` abstracts three context generations, two revisions, one reviewed body/key,
two send attempts, eligible/ineligible context, explicit confirmation and valid/invalid receipt.
It checks explicit confirmation, context-bound dispatch/receipt, one in-flight operation,
same-command retry and server-validated success. Negative controls remove each of those five
guards. No fairness/eventual-network-response claim. Existing TaskView, RaciEditorContext,
CommandBoundary, RaciReassignment and scoped Alloy relations remain applicable; no new relation
or grant is created. Snapshot/key/clock/domain abstractions require rendered adversarial tests.
Implementation may begin only after the complete formal suite passes within these bounds.

Executable tests must cover late pages, logout/rotation/navigation, read failure, non-manager,
unsupported lifecycle, invalid forms, review cancellation, double click, ambiguous failure with
exact replay, replay conflict, initial conflict, malformed success, valid receipt, Spanish/English
and dialog accessibility. Synthetic browser journeys are UI evidence, not full-stack server E2E.

Rollback removes only the editor entry point/component; the existing read and command APIs,
SQL receipts and immutable history remain. No schema rollback or accepted-work deletion.

## Formal results (2026-09-15)

Exact command, run from the repository root with permission for TLC's local RMI socket:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Completed exit 0 before feature implementation: 23 positive TLC configurations, 53 named
negative controls, 13 PlusCal integrity tests, 2 SAT Alloy scenarios and 13 UNSAT assertions.
`RaciWebEditor.cfg`: 154 generated / 120 distinct states, depth 9. All five new mutations
produced exit 12 and their named invariant failure. Earlier parser errors (`=<<` tokenization),
an integer/string sentinel comparison and a sandbox RMI denial were failures, not successful
checks. Tuple spacing and a tuple-wrapped visible generation corrected the model; no invariant
was weakened. Existing Alloy models are unchanged; these finite checks are not a universal
proof of the UI or its SQL refinement. [PR 26 evidence](pr-26-raci-web-editor.md) records the
executed implementation checks and remaining scope.
