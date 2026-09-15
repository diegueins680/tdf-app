# Event operations specification index

- [Repository audit](repository-audit-2026-09-14.md)
- [Gap matrix](gap-matrix.md)
- [Glossary](glossary.md)
- [Assumptions and constraints](assumptions-and-constraints.md)
- [Canonical domain](canonical-domain.md)
- [State machines](state-machines.md)
- [Threat model](threat-model.md)
- [Traceability matrix](traceability-matrix.md)
- [Delivery plan](delivery-plan.md)
- [Implementation report](implementation-report-2026-09-14.md)
- [Draft PR 01](pr-01-formal-foundation.md)
- [Draft PR 02](pr-02-api-foundation.md)
- [Draft PR 03](pr-03-logistics-transaction-hardening.md)
- [Draft PR 04](pr-04-task-commit-invariants.md)
- [Task transaction contract](task-commit-contract.md)
- [Receipt replay authorization contract](receipt-replay-contract.md)
- [Draft PR 05](pr-05-replay-authorization.md)
- [Snapshot and database-error privacy contract](snapshot-privacy-contract.md)
- [Draft PR 06](pr-06-snapshot-privacy.md)
- [Authenticated HTTP verification contract](http-verification-contract.md)
- [Draft PR 07](pr-07-http-verification.md)
- [Command target privacy contract](command-privacy-contract.md)
- [Draft PR 08](pr-08-command-privacy.md)
- [Session transaction fence contract](session-fence-contract.md)
- [Draft PR 09](pr-09-session-fence.md)
- [Draft PR 10: complete-schema rehearsal and baseline blockers](pr-10-schema-rehearsal.md)
- [Draft PR 11: canonical schema migration dependencies](pr-11-schema-dependencies.md)
- [Verified GitHub delivery checkpoint](github-delivery-2026-09-14.md)
- [Formal models](../../formal/event-operations/README.md)
- [ADR 0115: canonical event operations](../adr/0115-canonical-event-operations.md)
- [ADR 0116: bounded formal verification](../adr/0116-bounded-formal-verification.md)
- [ADR 0117: shared resource allocation](../adr/0117-shared-resource-allocation.md)

The bounded models were completed and validated before implementation, as required. The dependent
implementation branches advance in the delivery-plan order and update traceability with exact
code/test evidence. No document in this directory implies production activation or full end-to-end
completion.
