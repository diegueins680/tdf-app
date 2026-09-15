# Notification evidence minimization — 2026-09-14 Ecuador / 2026-09-15 UTC

This continuation depends on payment recovery [#343](https://github.com/diegueins680/tdf-app/pull/343),
`c263a499cc760ce7f97fe7833211f4d7de7494ea`. It hardens inbox retention; it does not activate a
provider, deploy staging, execute refunds, or complete the overall payment platform.

## Access, overlap and baseline

The isolated payment worktrees were clean at continuation start. `git fetch origin` completed;
default `main` remained `17a33eca11d585d84435af85340beece9b51d14e`, already in this dependent
stack. New event PRs #345/#346 were inspected: they add event HTTP/privacy behavior and some
shared CI/OpenAPI files, not payment inbox storage. This patch avoids those shared manifests,
OpenAPI files and generated clients. Original dirty worktree changes remain untouched.

GitHub authentication/read access, branch creation, commits and the staging branch push were
verified. At the `2026-09-15T00:13:54Z` check, the dedicated staging Fly secret names were still
absent. Existing Fly/Koyeb access failures remain in
[staging evidence](https://github.com/diegueins680/tdf-app/pull/344); no account alias, provider
credential, hosting deployment or sandbox payment was newly verified here.
The final approved local staging inspection at `00:47:40.611Z` again found both health
endpoints OK but no usable local Fly authentication; metadata reads failed closed and no
payment/provider readiness was inferred. Its sanitized report is preserved on staging #344.

## Implementation

See [ADR 0116](../adr/0116-minimized-provider-notification-evidence.md) for field-by-field
retention, trust boundaries, source URLs, compatibility and remaining risks.

- Every current inbox writer passes through provider-specific bounded field projection
  before any payload bytes enter SQL/encryption. PayPal, PlaceToPay and PayPhone are covered.
- The original callback remains the authentication input. This patch does not weaken signature
  verification or make a PayPhone callback authoritative.
- New payload checksums cover projected stored bytes. Exact valid legacy redelivery accepts
  its original checksum while retaining the original row/reference and ciphertext unchanged.
- Duplicate checks additionally compare immutable provider-created timestamps. Changed amount,
  resource, event type or trust evidence cannot silently reuse a stored event.
- `ProviderEventPayload` debug rendering never emits decrypted bytes. Malformed PayPal JSON
  errors no longer include parser input. The application pool already filters SQL debug logs
  in `TDF.DB.makePool`; that existing control was inspected, not bypassed or reconfigured.
- No public API/client/UI change, SQL migration, backfill, destructive operation or historical
  purge is introduced. There is no Datafast inbox adapter to activate; its existing polling
  path and the rest of the payment platform still require the parent regression checks.

## Local verification record

Environment: macOS, Stack/GHC 9.10.3, Node 24.8.0 for local script checks, disposable PostgreSQL
16 for runtime tests. All callback bodies, merchant identifiers, signatures and keys in these
tests are synthetic. No provider HTTP call is made by these new persistence tests.

| Command | Source / timestamp evidence (UTC) | Outcome |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Baseline #343 source; executed between 00:13:54 and 00:19:26 | 85 examples, 0 failures; 1.1448 seconds; exit 0. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| tail -n 45` | Intermediate implementation before the final eighth pure test; completion observed by 00:32:08 | 92 examples, 0 failures; 1.0964 seconds; exit 0. Not presented as the final source's test count. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 65` | Source `b524dc04a7633443420195ccdfad8121034dda38`; first invocation at 00:32:08 | Docker socket denied by sandbox; exit 126, no database test ran. |
| Same guarded PostgreSQL command, approved rerun | Same source; completion observed by 00:44:07 | **36 examples, 0 failures**, 15.9008 seconds; exit 0. Includes the existing 27 retry/recovery examples and nine new persistence/legacy/concurrency cases. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tail -n 22` | Same source; started 00:44:35, completion observed by 00:47:38 | **2564 examples, 0 failures**, 36.1716 seconds; exit 0. Includes eight new pure minimization/error tests and existing Datafast/PayPal/backend regressions. Database-specific tests are separately counted above. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-notification-evidence-audit.json` | Same source; report generated 00:44:25.156 | Exit 0; 1413 files / 1122 candidates, zero unreviewed candidates or stale decisions. No catalog decision change needed on this branch. |
| `docker ps -a --filter name=tdf-provider-retry --format '{{.Names}} {{.Status}}'` | At 00:44:35, after the PostgreSQL run | Exit 0, no rows: the harness removed its disposable test containers. |
| `git diff --check` | Source and documentation working tree; observed by 00:47:38 | Exit 0. |

PostgreSQL, full backend and catalog checks completed successfully. Existing compiler warnings
were not suppressed. Observed bounds are used where exact command-start timestamps were not
emitted; no provider execution time is invented. Full web/native-device and credentialed
provider sandbox/staging payment tests were not executed for this internal backend patch.
The parent #343's backend CI was still running at 00:47:38; local success is not substituted
for that pending remote result. The separate staging #344 catalog repair passed its hosted
check on source `79f8366fd9cae469494bd7bbecebc2e3c788477b` at 00:41:43, with the original
gate intact; it does not validate this new inbox patch.

## Rollout, rollback and operator boundary

Deploy reviewed inbox writers together only after sandbox/staging qualification. All provider
activation flags and merchant evidence requirements remain. Old workers can parse retained
standard fields, but old writers still retain complete bodies. Reverting to old storage code
reopens the issue; preserve the queue, reconcile original provider references, and obtain an
approved intake/rollback plan rather than clearing evidence or authorizing replacement charges.

Historical full-body ciphertext remains unchanged. No production contents were inspected, so
their presence/absence of prohibited data is unknown. Security/PCI and Ecuadorian legal/privacy
reviewers must direct any retention remediation, with financial references preserved and no
payloads copied into logs or support tickets. This patch does not claim data purging or PCI,
legal or accounting certification.

The original PlaceToPay body-derived event identity still permits distinct IDs for formatting
or unused-field variants. New same-ID events deduplicate projected evidence; this is not a
claim of cross-ID semantic deduplication. Global ingress limits, historical cleanup, merchant
onboarding, refunds/captures/voids, subscriptions, settlement/dispute execution, SRI processes
and compliant marketplace payouts remain separately tracked work.

**Follow-up, 2026-09-15 UTC:** [ADR 0118](../adr/0118-signed-payment-notification-identity.md)
adds signed, versioned PlaceToPay identity for new events and exact historical
redelivery compatibility. It addresses forward cross-ID amplification; it does
not retroactively collapse or rewrite historical rows. The preceding paragraph
describes the #347 implementation, not the later dependent fix.
