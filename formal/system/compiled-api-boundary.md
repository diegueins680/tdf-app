# Compiler-derived API boundary: SYS-API-003

`tdf-hq-exe --describe-api` exports the compiler's `TypeRep` structure for
`TDF.Server.CombinedAPI`, the same type passed to `serveWithContext`. It does not
start the server, load runtime configuration, connect to PostgreSQL or invoke a
provider. It includes the trial API preceding the main API, alternative order,
path symbols, capture names/types, request modifiers, bodies, auth combinators,
HTTP methods, declared success statuses and opaque Raw mounts.

This closes a discovery gap left by generating web/Mobile clients solely from
handwritten OpenAPI. It does **not** turn type names into JSON schemas or establish
handler behavior. An absent `AuthProtect` does not prove anonymous access: handlers,
headers, cookies, webhook signatures and middleware can enforce other boundaries.
Raw mounts can serve paths not enumerated by Servant types. Feature enablement,
DTO codecs, error responses, ownership and authorization need separate evidence.

`inspect-compiled-api.mjs` invokes that mode with deliberately invalid database and
port configuration and no inherited credentials. It preserves the full compiler
output, executable hash, source tree/fingerprints and generated contract hash in
a new private output directory. Source or binary changes during inspection reject
the observation. It compares typed methods/paths and success statuses against the
resolved OpenAPI operations in generated traceability. Parameter-name differences
are retained but normalized for routing comparison. Duplicate typed routes,
undocumented routes and documented routes without a typed counterpart stay visible;
a Raw mount may explain some of the latter but never silently removes them.

The decoder recognizes explicit compiler constructors; a new/unrecognized
combinator or malformed node fails. The current comparison emits discovery gaps,
not a PASS or an automatic exemption. Each gap needs reconciliation before a
complete API conformance claim or a strict zero-drift baseline is possible.
CI retains the actual output after building/testing the backend. Its build supplies
the provenance link; a hash alone does not prove an arbitrary executable was built
from the associated source checkout. Historical reports remain revision-scoped.

The selected Stack/GHC was used to inspect actual Servant/Multipart constructor
names and argument ordering. Hspec controls distinguish removal of authentication,
a required-to-optional header mutation, and a changed method. Node controls verify
branch isolation, opaque mounts, modifiers, unknown-node rejection, added/removed
routes, duplicate routing and status drift. These are executable contract controls,
not a universal proof of API implementation correctness.

## Executable declaration drift gate

The compiled b7628e6a5 backend emitted845 typed operations and two Raw mounts. Its
comparison with481 documented operations found386 undocumented operations,
25 documented operations with no typed route, three competing declarations and
21 declared success-status differences. These counts are discovery findings, not
all confirmed runtime defects: disabled handlers can intentionally return501/503,
and `NoContent`/middleware behavior requires HTTP inspection.

`compiled-api-surface.json` preserves the full declaration shape and branch order.
CI compares its just-built executable against this reviewed implementation
snapshot, including authentication, request modifiers, body and response types,
status declarations and Raw mounts. A mismatch fails and still retains
`contract-candidate.json` plus the discrepancy report. To update after reviewing
intent and affected contracts, copy that fresh candidate to the canonical snapshot,
regenerate the requirement inventory/traceability, and rerun the gate. Never
regenerate solely to silence an unexplained failure. The ten controlled mutations
cover removals, additions, security/modifier/body/response/status/method/order and
Raw-path changes. Matching this snapshot does not waive existing OpenAPI gaps.

The initial discovery found three competing declaration identities:
`GET /version`, `GET /trials/v1/subjects`, and `GET /radio/presence/{partyId}`.
All25 documented/unmounted operations belong to `MerchReputation` API types whose
handlers exist but are absent from the served CombinedAPI. Mounting them would
require feature/privacy authorization review; discovery alone does not authorize
activation. These findings remain open in the machine-readable comparison receipt.

The `GET /version` duplicate is repaired by removing the shadowed Meta route.
The first-served `VersionInfo` handler and published response fields remain the
runtime authority (AUTHORITY-022). The removed DTO had no consumer and fabricated
its `builtAt` from request time. Compiled snapshot regeneration must confirm the
single remaining declaration. AUTHORITY-023 and AUTHORITY-024 reconcile the two
other identities through the scoped [read contracts](routed-read-boundaries.md).
Fresh compiled/HTTP evidence must verify those repairs. The gate now rejects any
competing typed route even when its regenerated snapshot matches; a negative
control attempts that exact bypass. Opaque Raw mounts remain an explicit limit.

## Explicit availability: MKT-REPUTATION-001

`api-availability.json` is the canonical typed-route deferral list. The25
merchandise-reputation declarations retain their existing synthetic-local-only
scope from `docs/merch-reputation.md` (AUTHORITY-026). They are individually
listed with requirement ownership and provenance. A regenerated declaration
snapshot cannot authorize mounting them. The compiled gate rejects an
unexpectedly mounted deferral, an unexplained missing documented operation,
a disappeared documented deferral, duplicate entries and malformed entries.
The raw comparison still reports all missing declarations; a separate availability
receipt explains only reviewed deferrals. Policy bytes are captured and hashed
before the final freshness check. Raw mounts and actual feature/authorization
behavior remain outside this typed-route contract.

Fresh read-only observation at2026-10-05T07:59:37Z on backend645f56f found all nine
merchandise-reputation production flags false, interaction runtime enabled with
activation history true, social runtime disabled with no activation history, and
no event-operation flag table. These are distinct control families. Missing
controls are unknown, never silently interpreted as disabled. This historical
observation is not a substitute for a fresh release inspection.

The always-selected repository lane runs `check-api-availability.mjs` against the
actual policy, resolved OpenAPI and canonical compiled snapshot, so policy-only
changes cannot bypass admission by skipping a backend build. Python also rejects
missing/duplicate operation identities and invalid requirement/provenance links.
The backend inspector separately verifies the snapshot against the new executable.
