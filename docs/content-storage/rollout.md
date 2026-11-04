# Storage rollout and recovery

Status: implementation candidate; production activation blocked by the open
obligations in `formal/system/content-storage.md`. No production mutation or
Google provider write is established by this record.

Use the current `ops/hetzner/README.md` release process. Keep broad ORM migrations
disabled. Apply only the reviewed manifest, in order, retaining all migration
introduction ancestors (do not squash them away). Never rerun an individual old
function-definition SQL file over a newer schema. All five new forward steps
must be rehearsed as a single ordered batch. Rollbacks reject occupied schemas;
accepted uploads require compatible forward recovery, not dropping their tables.

TDF files use the existing persistent private `uploads/content-storage` bind and
the public assets `content` directory. Confirm writable mounts, free disk space,
backup inclusion, UID/GID, private access denial and hash survival across container
replacement. A row tombstone is not proof of completed physical deletion. Inspect
cancelled rows with NULL deleted_at and lease-expired reservations for recovery;
never release their accounting merely because time elapsed. Filesystem rename and
database commit are not one atomic durability boundary. Restore both as one set.

For existing accounts, prepare an ownership and exact-size inventory of all
historical managed files. Distinguish external references, already registered
objects and legacy managed bytes to avoid double counting. Preserve existing
larger entitlements. Hash and review the report; the non-executing helper can be
invoked as documented by `node scripts/prepare-content-storage-reconciliation.mjs INPUT_JSON NEW_PRIVATE_PLAN_JSON`. Its plan does not prove inventory completeness or authorize execution.
Only the reviewed writer can call the invoker-security reconciliation function
with the expected account revision. Do not reconcile unknown usage to zero.

Before enabling ordinary uploads, rehearse public artist covers, private course
and payment attachments with every authorized reader, own-provider consent and
revocation, shared-provider outages, duplicate concurrent requests, timeouts after
create, ownership changes, cancellation, deletion failure and restore. Verify
that unrelated TDF login remains valid. Complete the open credential-vault,
form-draft and canonical content/manifest work first. Existing personal tokens
must not be migrated between accounts or silently adopted from legacy unbound
browser storage.

Require current protected checks, independent review, a compatible recovery image,
reviewed migration ledger and coordinated backup/restore evidence. The current
release preparation can report executionAllowed=false for unfinished release
authority; do not substitute a legacy deployment path. After rollout, verify the
actual API version, all writers, frontend contracts and synthetic authorized
uploads. Backend health alone is not upload verification.

## Local verification commands

```sh
npm run generate:api
npm run typecheck
node --test scripts/__tests__/content-storage-model.test.mjs scripts/__tests__/content-storage-reconciliation.test.mjs
TDF_TEST_POSTGRES_HOST=127.0.0.1 node --test scripts/test-content-storage-postgres.mjs
npm run quality:backend
npx playwright test --config=playwright.storage-upload.config.mjs
```

The PostgreSQL runner creates and drops only its own new nonce `_test` database.
Never point it at a remote production host. The browser fixture is isolated from
real providers; its localhost assertions do not establish production behavior.

## Mobile companion

`mobile-companion.patch` contains the exact source and regenerated client changes
from local Mobile commit `53a381c18c3db51a2810d9f05b3e133d6af9d6e9`, based on
`bee23755401f1a3cb6c31fbfb7b42f5c6ccc032f`. That main tree equals the root's
current `f4c7aba45dc153db8809dbe194bc826ff615ef88` pin. The connected integration
rejected blob creation in `diegueins680/tdf-mobile` with HTTP403. No companion PR
or remotely available commit was created; the root deliberately retains its
published pin. Do not point a gitlink at an unpublished local commit. Applying
and publishing the reviewed companion, then pinning its exact commit, is required
before generated-client conformance can pass. Nineteen directed Mobile tests,
Mobile typecheck and focused lint passed on the local companion; native device,
EAS and store distribution are unverified.

The plan input is an object with `observedAt` (ISO timestamp within 15 minutes)
and 1–100 `accounts`. Each account supplies positive decimal-string `partyId`,
nonnegative decimal-string `legacyBytes` and `expectedRevision`, boolean
`ownershipVerified` and `sizesVerified`, relative `evidenceFile` and its lower-case
`evidenceSha256`. Evidence must stay in the input folder and be at most 50 MiB.
The output is a new mode0600 file; assertions require operator review.
