# Storage rollout and recovery

The current repair normally incorporates protected main
`f615c862b1a7d50406cc0506d21d7650e607fa5f`. Earlier source receipts below retain
their actual commit identity; they do not qualify this combined candidate.
Production activation remains blocked. No deployment, provider operation,
production migration, inventory application or flag change is performed here.

The new backend always encrypts delegated credentials with owner/provider/connection
bound AES-256-GCM and returns an opaque handle. Before installing that backend,
configure the server-only `CONTENT_STORAGE_VAULT_KEY` (32 random bytes as 64 hex
characters) and rehearse coordinated recovery of the key, database and files.
Missing or replaced keys fail closed; no new backend plaintext fallback exists.
Keep keys outside public frontend configuration, logs and database backups.

For staged frontend/backend compatibility, `VITE_DRIVE_VAULT_REQUIRED` defaults
off and only literal `true` activates strict credential retirement. Staged mode
accepts the pre-existing bounded owner-scoped provider descriptor through the
same authenticated `/drive/upload` proxy as main. It never accepts unbound
credentials, mismatched owners, malformed vault identities, mixed opaque/raw
pairs or stale asynchronous writes. This mode preserves historical browser
provider secrets and is explicitly NOT vault-retirement acceptance evidence.
Strict mode retires all raw provider descriptors and requires renewed consent.
Deploy the configured backend and verify its vault/recovery behavior before
activating strict frontend mode. An old frontend's local disconnect cannot
establish revocation of a new server vault connection; complete coordinated
client rollout and explicit server disconnection testing before security acceptance.

A legacy descriptor disconnect remains local while staged, because the old backend
has no vault revocation endpoint. Opaque descriptors always use `DELETE /drive/token`;
a denied revocation never falls back to local success. Pending disconnection checks
owner, session epoch and the exact stored connection before removing local state.
Provider-wide consent revocation remains a separate Google settings operation.

`CONTENT_STORAGE_UPLOADS_ENABLED` remains disabled by default; only literal `true`
admits account uploads. This flag does not gate the OAuth vault transition, so it
cannot substitute for the backend-first vault/key preflight above. No personal
upload falls back to a corporate account or folder. Native destination/consent
screens and real-provider qualification remain unfinished. Existing corporate
inventory upload callers must keep their current adapter; any new explicit
account-storage helper is separate and unavailable until its own activation.

The immutable canonical main246 migration prefix and original November6 security
migration are preserved. A new forward identity guard protects the raw credential
owner and attachment binder references. Apply the full reviewed248-step batch in
order with original introductions retained; do not rewrite old applied SQL or
squash introduction ancestors. Two tables are locked before guard shape checks.
Populated rollback refuses to remove admission while preserving historical rows.
The model-owned Party-reference registry remains231, rather than inventing ORM rows.

Historical integration403 was a limitation of an earlier connector. Current native
GitHub CLI authority has successfully published and protected-merged other Mobile
work. It is not the current blocker. Reconcile this candidate's actual compiled
API against a root-compatible Mobile snapshot and preserve current Mobile product
work; its old14318a7 patch is historical and must not be blindly applied over newer
contracts. Actual protected upstream ancestry and current independent review/CI
remain required before integrating a new root pin.

## Required operational evidence

- Review a byte/hash/ownership inventory of every managed root and historical
  attachment store, and distinguish personal-provider references from TDF bytes.
  Unknown ownership or unknown size blocks capacity reconciliation; never record
  zero as an assumption. Existing higher allowances remain intact.
- Test owner A, unrelated B, a permitted domain reader, revoked grant and revoked
  TDF session over the actual authenticated HTTP route. Private files must fail
  through assets/CDN paths, including guessed names, traversal and root overlap.
- Google creation requests bypass domain default visibility with
  `ignoreDefaultVisibility=true`; parent permissions still apply, so private
  selected folders remain rejected and full owner-only ACL checks remain required.
  Verify this using a Workspace domain with broad default visibility before activation.
  Provider contract: https://developers.google.com/workspace/drive/api/reference/rest/v3/files/create
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

Current local follow-up validation: canonical GHC9.10.3 compiled the backend
and Hspec passed 4,270 examples, zero failures and 22 pending. The dedicated
PostgreSQL16 runner passed ten security cases including the real loopback HTTP
owner/foreign-account/resource-reader/session-revocation/static-path assertions.
An initial run had six replay expectations for implicit public permission; the
fixtures now choose public explicitly and two added cases reject unintended
public sharing on private replay. Provider transport remains synthetic.

The compiler-produced API description adds only authenticated
`DELETE /drive/token` (204); existing declarations are unchanged apart from
ordinals. Its exact candidate is adopted with provenance in
`compiler-observation-20261106.json`. Discovery gaps remain tracked obligations.
Web build, TypeScript, focused lint and directed security/recovery tests passed;
the final private transport rerun passed seven cases, including redirect denial.
Thirteen SQL reservation/concurrency/replay/deletion cases, nine finite policy/
reconciliation checks, twenty browser credential/session cases and ten catalog
provenance cases passed. The unpublished Mobile companion passed three new
helper cases, TypeScript and focused lint. Hosted checks and independent review,
Mobile publication/UI, historical inventory and actual provider/image/restore
acceptance remain pending. These receipts do not enable production.

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

## Historical Mobile companion receipt (prior to PR703)

The following identifiers and test counts describe the earlier companion only;
the current patch and published pin are described at the start of this document.

`mobile-companion.patch` contained the exact source and regenerated client changes
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
