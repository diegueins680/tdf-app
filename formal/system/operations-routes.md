# `/operations` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Operations/Server.hs`, `Operations/Model.hs`, `Operations/API.hs` and `tdf-hq/sql/2026-08-09_admin_operations_control_center.sql` plus `2026-10-11_operations_failure_replay.sql` on main `f615c862`.

This is the first route domain reviewed in this audit that was already fully specified: `OPS-COMMAND-001`, `OPS-APPROVAL-001`, `OPS-MANUAL-001`, `OPS-AUX-WRITE-001` and `OPS-FAILURE-REPLAY-001` (all `specified and implemented`, with `scripts/lib/operations_conformance.py` as conformance evidence) cover all 11 routes by handler; they never quoted the paths, which is why the ledger listed them as unspecified. No new requirement was written; the ledger now links each route.

**Guards (all 11 routes)**
- Bearer session, re-validated inside the transaction (`withCurrentAuthSession`, token `FOR SHARE`), roles re-read `FOR SHARE`; 401 otherwise.
- `lockOperationsScope`: organization `operations_enabled`, branch active, actor and every named party an active scope member (403).
- Work items: `FOR UPDATE`, mutating role, entity-type write visibility (404), `expectedVersion` compare-and-set (409); every effect writes work-item event, stream event and admin audit in the same transaction.
- Serialization or deadlock answers 409; any other SQL error answers a sanitized 500.
- No handler sends email, push or provider calls; push tokens are only stored (encrypted, 503 without the key).

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| work-items seen, transition, assignment, priority | OPS-COMMAND-001 | reviewed |
| approvals create, decision | OPS-APPROVAL-001 | reviewed |
| work-items create | OPS-MANUAL-001 | reviewed |
| notes, saved-views, push-subscriptions | OPS-AUX-WRITE-001 | reviewed |
| integration-failures replay | OPS-FAILURE-REPLAY-001 | reviewed |

The distinct-approver rule of `OPS-APPROVAL-001` holds: the handler refuses `requester = actor` (409) and the table carries `CHECK (approver_party_id IS NULL OR approver_party_id <> requester_party_id)`.

## Minor observations (no requirement violated)

- A decision on an unknown approval answers 409 rather than 404; a replay of a non-retryable failure answers 404 rather than 409/422.
- Approval creation accepts `amountMinor` without a currency; manual work-item creation rejects that combination.
- No worker or cron executes approved requests (`execution_status = 'pending'`) or expires pending ones; the `expired` and `cancelled` decision states appear unused.
- The approver is not required to have visibility of the linked work item (creation requires it); only a distinct party, not a distinct role, is required.
- A shared saved view needs no extra role to be shared.
