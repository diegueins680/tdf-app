# Social refactor — review workspace (2026-09-14)

**Delivery is in progress. No rollout, merge, or production activation is authorized by this packet.**
Original baseline: `17a33eca11d585d84435af85340beece9b51d14e` (remote main verified).
The reviewed dependency now includes main `73edd77a36c8dcc73e5217303c62376ae684853b`.
See the [handoff and acceptance status](handoff.md) for actual PRs, evidence,
remaining work and the automatic-provider deployment exception.
Use [audit](audit.md), [decisions](research.md), [policy](policy.md), and
[verification](verification.md) together. Passing bounded models are not a proof of the application.

## Priority and dependency order

1. Audit, source-backed decisions, executable models and counterexample tests.
2. Additive PostgreSQL relationship/policy operations, focused persistence and race tests.
3. Authenticated, disabled-by-default API and client integration for Following and Discover.
4. Qualify legacy integration, all clients, lifecycle and media boundaries, full-schema
   migration, CI, performance and operational rollout. Incomplete stages remain draft.

The objective is useful music-industry connections and subsequent collaboration,
bookings and sales. Activity alone is not success. Preserve the existing payments,
booking, releases, event and media authorities. No graph service is proposed.

Legacy chat enforcement: [write boundary](dm-write-boundary.md),
[read/API boundary](dm-read-boundary.md), and [session authority](session-boundary.md).
