# Feature discoverability and authorization inventory — 2026-08-21

Baseline: `origin/main` at `560ac9954b655ba8aa719963a2c890110b74a232`, plus the persona-program registry correction in PT-001. This is a generated source inventory, not a production authorization audit or release-readiness claim.

## Counts

- 137 feature records: 44 primary destinations, 57 important submodules, 26 contextual routes and 10 technical routes.
- 122 unique registry web-route declarations; the router audit resolves 156 actual web paths/redirects.
- 35 features with native mobile treatment; 38 actual Expo Router paths.
- 3,014 role × module × feature × action × platform rows for 11 representative fixtures and 17 actions.
- 548 expanded concrete backend endpoints: 493 mapped to stable features; 40 pending explicit disposition.
- Endpoint dispositions: 458 user/admin interface mapped, 32 documented API-only, 3 security-sensitive concealed, 11 technical API-only, 4 technical/static and 40 pending.

## Artifacts

- [Feature inventory](feature-inventory.csv)
- [Web/mobile route mapping](route-and-mobile-mapping.csv)
- [Role–module–feature–action–platform matrix](role-module-feature-action-platform-matrix.csv)
- [Backend capability inventory](backend-capability-inventory.csv)
- [Pending backend dispositions](pending-backend-capabilities.csv)
- [Machine-readable summary](generated-summary.json)

## Important gate

The earlier 2026-08-06 packet reported zero pending endpoints, but current API growth and corrected parsing reveal 40 rows needing owner review. They include catalog/security revisions, DDEX references, directory favorite deletion, label project notes, public Domo/event ticket/records/review reads and event research writes. “Pending” is not evidence of an exploit; it means interface intent, exact action, handler authorization and record scope have not been reconciled into this audit.

Generate review evidence with:

```sh
ALLOW_PENDING_FEATURE_DISPOSITIONS=1 npm run generate:feature-audit-reports
```

The opt-in permits the packet to be written but does not turn it green. The ordinary command must fail until every pending row has a documented feature/action or an explicit API-only/concealed/technical disposition plus authorization evidence.

## Reconciliation with prior work

The [2026-08-06 packet](../2026-08-06/README.md) remains valuable for its architecture, production assignment, telemetry/privacy, experimental-feature and rollout analyses. This packet refreshes generated inventories rather than asserting those narrative/production checks were repeated. DDEX `501` behavior and known web/mobile exceptions are acknowledged prior findings.

No production assignment, role, module, preference, feature flag or database record was read or changed while generating this packet.
