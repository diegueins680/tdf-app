# Payment recovery evidence (18 November 2026)

Two isolated PostgreSQL 17.11 rehearsals at commit
[`3b0c799e`](https://github.com/diegueins680/tdf-app/commit/3b0c799e2168e55649ed6b621f5dde11143582be)
used synthetic data only:

- **Duplicate delivery after restore:** after restoring a pre-confirmation snapshot,
  the canonical payment writer applied one of eight concurrent deliveries and
  recognized seven repeats. A ninth delivery left the order, attempt, receipts,
  and accounting unchanged. The harness retained the confirmation in memory;
  this proves idempotency on redelivery, not durable recovery.
- **Fresh process after restore:** with the attempt already in the snapshot,
  the new process found the pending checkout but queued no Datafast status
  query and did not reapply the synthetic payment. Enabling recovery with a
  ready synthetic merchant did not change this result. The
  [query worker at the tested revision](https://github.com/diegueins680/tdf-app/blob/3b0c799e2168e55649ed6b621f5dde11143582be/tdf-hq/src/TDF/Commerce/ProviderReconciliation.hs#L215)
  schedules PlaceToPay and PayPhone, not Datafast.

Do not use this evidence to mark Datafast recovery verified. The outstanding
acceptance test must recover a saved Datafast attempt after a full restart,
validate the provider status response, and apply it once through the canonical
writer. An attempt created after the snapshot requires an independent discovery
source. Neither rehearsal exercised the Datafast HTTP handler, contacted the
Datafast sandbox, or changed production.
