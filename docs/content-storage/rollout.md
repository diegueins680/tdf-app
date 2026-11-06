# Storage rollout and recovery

Production activation remains blocked. PR703 and Mobile179 are already merged;
this security follow-up starts from root main c47c2a8d8851a58a828dec830704f8c352496c20
and its exact Mobile14318a7 pin. No production upload, deployment, inventory
application or provider operation is performed by this change.

The candidate adds `CONTENT_STORAGE_UPLOADS_ENABLED` (disabled by default),
server-only `CONTENT_STORAGE_VAULT_KEY` (32 random bytes encoded as 64 hex digits),
AEAD-encrypted delegated credentials, account-bound opaque handles, authenticated
private readers and transactional canonical attachment references. Keep the key
out of public environment variables, logs and DB backups. Independently protect
and test recovery of the key with the matching database/files backup. Rotation
requires a reviewed re-encryption/reconnect procedure; replacing the key alone
makes old envelopes unavailable and never falls back to plaintext credentials.
Disconnect erases the active envelope; backups follow their retention policy.
Users can separately revoke the application's Google consent in Google settings.

All account uploads stay disabled until the historical inventory is reviewed,
canonical publication body/manifest accounting and routing is reconciled, the
native selection/consent UI is complete, exact-image HTTP/provider/persistence
and coordinated restore tests pass, and required independent review/CI are green.
A root flag or successful build is not that evidence. Keep `RUN_MIGRATIONS=false`.
The additive November 6 migration follows the five unchanged November 4 steps;
preserve its introduction commit and the entire ordered migration manifest.

Mobile main has subsequently integrated an independent artist-launch contract
(PR180), while the root's shipped pin is Mobile14318a7. The security companion is
rooted at that exact pin and preserves the tested root contract. Before merging
it into newer Mobile main, reconcile that successor's additional generated
contracts against their authoritative backend branch; do not remove them as an
incidental storage change. This cross-branch dependency blocks independent Mobile
activation and is distinct from the already completed Mobile179 merge.

The current connected GitHub integration again rejected Mobile blob publication
with HTTP403 (`Resource not accessible by integration`). The reviewed local
companion is preserved in `mobile-companion.patch`, based on published14318a7;
root intentionally retains that published gitlink. Apply the patch only to a clean14318a7 checkout using `git apply --unidiff-zero docs/content-storage/mobile-companion.patch` from the appropriate repository path. The patch contains regenerated
API types plus native authority/session tests. Its publication and successor
contract reconciliation remain blocked; do not point root at an unpublished SHA.

## Required operational evidence

- Review a byte/hash/ownership inventory of every managed root and historical
  attachment store, and distinguish personal-provider references from TDF bytes.
  Unknown ownership or unknown size blocks capacity reconciliation; never record
  zero as an assumption. Existing higher allowances remain intact.
- Test owner A, unrelated B, a permitted domain reader, revoked grant and revoked
  TDF session over the actual authenticated HTTP route. Private files must fail
  through assets/CDN paths, including guessed names, traversal and root overlap.
- Verify Google account consent, refresh, disconnect/revocation and explicit
  public permissions using an isolated provider account. Never reuse shared TDF
  credentials or folders for a personal destination. A private file's inherited
  folder permissions are rejected in the personal-private path; complete owner-only ACL verification and real-provider creation tests are required. Do not certify them
  from a local mock.
- Rehearse duplicate/concurrent requests, process termination during copy/rename/
  commit, reconnect during refresh, interrupted deletion, persistent-volume
  replacement, disk exhaustion and coordinated restore of files/database/key.
  Compare SHA-256 hashes, manifests, reservations and tombstones after restore.
- Confirm all public `content/` traffic reaches the manifest-checking API reader.
  Direct static/CDN mount access bypasses that authorization boundary and blocks
  activation. Purge pre-existing caches and prove private/staging denial.

Current local follow-up validation: the web production build and focused lint/
TypeScript passed; directed web recovery/security tests, including authenticated
blob transport and private/public result separation, passed. Thirteen real
PostgreSQL16 reservation/concurrency/replay/deletion cases and nine finite policy/
reconciliation checks passed. Three new Mobile helper cases, Mobile TypeScript
and focused lint passed on the unpublished companion. New AEAD/private-reader
Hspec cases are authored but not yet a compiled pass. Hosted CI is queued; the
new compiler-derived API declaration and Mobile publication remain pending.

The commands and historical receipts below are retained as prior evidence;
they do not certify the current candidate or production.


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
canonical content/manifest work first. Rehearse the tab-scoped metadata recovery
with original-file reselect on both desktop and phone; browser storage denial must
remain visible. No binary is saved in the tab draft. Existing personal tokens
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

## Candidate verification

Hosted run 41921346038 compiled and tested source tree
`6a0c5682a9325f8d0c084e5d94244f21d2800628` (branch head `7b4b825`).
Hspec completed 4,184 examples, zero failures and 21 pending. The later API
inspection step rejected the stale declaration; its compiler-derived five-route
addition is now adopted with provenance in `compiler-observation-20261104.json`.
Three of four synthetic browser cases passed; the fourth failed due to a strict
status selector matching pagination as well as the upload. The selector is now
scoped and two reload cases have been added. Those six cases require a fresh
hosted receipt; neither synthetic OAuth nor mocks prove Google permissions.

The public production version read on November 4 returned commit
`4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`, build time
`2026-10-24T01:24:48Z`. This is an observation of an older deployed backend, not
a deployment of this candidate or a successful upload rehearsal.

The web reload follow-up passed the production build, focused TypeScript/lint,
626 course-administration cases, 13 profile-ownership cases, and directed
managed-upload, payment-draft, queue, metadata and inventory transport cases.
An initial parallel profile run hit a five-second test timeout; its isolated
rerun passed all 13 cases without changing the assertion. These are local
receipts, not real-provider or production acceptance. Tab drafts restore metadata
only; original binary bytes must be reselected. Final hosted checks remain
required for the exact integrated head.

Protected main advanced to `8efa5afe` during implementation. The integrated
manifest preserves that exact 239-entry prefix, then appends the five unchanged
storage entries (244 total). The prior 241-entry storage manifest is preserved
as a view omitting main's three independent additions. Both complete parent
review graphs and their hashes are retained in the active successor and
`storage-main-integration-20261104.json`. Earlier build receipts do not certify
this newly integrated backend; fresh hosted checks are required.
