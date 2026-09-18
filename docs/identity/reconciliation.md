# Identity reconciliation

This implementation adds a restricted contact-merge lane and actor-scoped contact creation
idempotency. It does **not** authorize account takeover, ownership transfers, or merging artists,
bands and organizations into people. It does not establish identity from names, email, phone,
Instagram handles, similarity or creation timestamps.

## Evidence and review

`Party` is shared by CRM contacts and accounts. `UserCredential`, API tokens, canonical security
roles, artist/fan profiles, directory profiles, managers and memberships serve separate purposes.
Multiple roles on one Party are not duplicate accounts. A person and their artist or organization
profile may be complementary; use the existing directory claim/manager workflow for verified
management access rather than transferring ownership.

The inventory command produces exact-attribute candidate groups. It preserves email local-part
case, dots, plus tags, phone prefixes, accents and identifier punctuation. Trimming creates a
review hint only. Overlapping groups are not joined transitively. Each entire group needs one
reviewed, issuer- and tenant-scoped identity assertion covering every member.

The case queue, before-state snapshots and merge history contain private data. Only database
operators with existing table/function privileges may use these tools. PUBLIC has no access to
the evidence tables or merge/rollback functions. Existing PostgreSQL superusers (including the
current application database role) retain access; there is no new HTTP endpoint for this history. This first lane is an operator workflow, not a
new permission granted to CRM users. Keep output in a private directory; never put inventories,
credentials, contact attributes or proof documents into PRs, Git or general reports.

Use the existing Fly database access workflow with `scripts/identity-reconciliation.mjs`:

```sh
node scripts/identity-reconciliation.mjs --command inventory \
  --db-app APP --database DATABASE --output /private/path/inventory.json
node scripts/identity-reconciliation.mjs --command summary --inventory /private/path/inventory.json
node scripts/identity-reconciliation.mjs --command queue --inventory /private/path/inventory.json \
  --db-app APP --database DATABASE --output /private/path/queue.json
node scripts/identity-reconciliation.mjs --command list \
  --db-app APP --database DATABASE --output /private/path/cases.json
node scripts/identity-reconciliation.mjs --command dry-run --case CASE_UUID \
  --db-app APP --database DATABASE --output /private/path/plan.json
```

The database-backed inventory commands require the reconciliation migration. Inventories retain
archived rows for accounting, exclude them from active candidate groups, and report execution
and rollback counts from the private ledger. Attribute-only inventories never confirm identity.

`queue` creates review cases idempotently. If authoritative evidence identifies a whole group
that includes an established canonical record absent from the attribute hints, supply
`--groups-file PRIVATE_JSON` containing `[{"member_ids":[...]}]`. Every ID must exist in the
inventory. This only creates review cases; it cannot authorize a merge or infer a similarity chain. A database operator reviews the existing private records
and provenance. Record `separate` for established distinct entities, with the reason. Leave
uncertain cases in `review`. To confirm, record the reviewer Party, review time, a reason, current
ordered Party snapshots, and an evidence object containing:

- `basis`: `verified-source-subject` or `authenticated-owner-attestation`;
- `issuer`, `scope`, and the exact `subject` in that issuing system;
- `evidence_reference`: an access-controlled reference to the actual verified evidence;
- `member_ids`: the complete ascending set of Party IDs covered by that assertion;
- `external_reference_review`: `no-unresolved-references`, only after inspecting non-scalar and external dependencies.

Apply the documented decision with `--command review --case CASE_UUID --decision-file PRIVATE_JSON`
and the same database/output arguments. The decision file includes `status`, `reviewer_party_id`,
`reason`, `evidence`, and `expected_fingerprint` from a fresh dry run. It locks the member rows and
rejects stale review evidence. Cases can be reviewed again before any merge or link; every prior
review is retained in the private evidence history. Once an operation exists, review changes are
rejected. A confirmed decision still cannot bypass execution blockers.

These are operator attestations, not cryptographically verified documents. Do not manufacture
proof by copying shared contact fields into the subject. Missing authoritative proof remains a
review blocker. Updating the status alone does not permit execution. Attribute matching alone confirms zero groups. Direct authoritative owner attestations may establish
whole-group identity; authentication, ownership and dependency conflicts still block execution.

## Retain and link complementary records

For a verified person with intentionally separate contact/artist and account records, review the
case as `separate` with whole-group evidence and `disposition: "retain-and-link"`. Then use
`--command link-dry-run --case CASE_UUID` and `--command link --case CASE_UUID --operation UUID
--fingerprint LINK_PLAN_SHA256`, with the database/output arguments above. This creates a private,
informational link with provenance. It changes no Party fields, authentication, management rights,
roles, consent or canonical routing. Both records remain active for their distinct purposes.
`--command links` lists the access-controlled links; `--command unlink --operation UUID` reverses
one linkage while retaining its history and all subsequent contact edits. Retries have no additional
effects. This association alone never establishes permission to manage an artist profile; verified
management still uses the existing directory claim workflow.

## Survivor, fields and dependency safety

The survivor is an established active account, then the oldest created Party, then the lowest
stable ID. Creation time breaks an identity-stability tie; it never resolves field freshness.
Source records with credentials, permissions, profiles, memberships, bookings, messages,
financial/audit references or other detected dependencies are blocked. The function inventories
all declared Party foreign keys, the model-declared Party references, and legacy scalar Party/user/actor/reviewer/approver references. The forward repair migration adds guards for the model-declared columns, including catalog approval history without foreign keys. `node scripts/generate-identity-party-reference-view.mjs --check` verifies this registry against the models; after release, model changes require a reviewed forward migration, not editing applied SQL. Operators must
also inspect integration payloads and any untyped external references before attesting a case;
this scalar inventory cannot prove the absence of arbitrary identities embedded in free text or
external systems. Such dependencies require a separately reviewed relationship migration.

Only missing compatible contact fields are copied. Values remain in the before/after evidence.
Conflicting nonblank values require review, including equally verified values; the system does
not choose by import timestamp. Copying a missing email onto an authenticated account is blocked
because legacy authentication uses that field. Passwords, tokens, consent, roles, payment IDs,
permissions and ownership are never copied. No dependent row is deleted or reparented in this lane.

Execution locks the public tables for one short group, revalidates evidence, snapshots and
dependencies, writes history and archives redundant Party rows in one transaction. The CLI sets
five-second lock and sixty-second statement limits. Run one group at a time off peak. Lock timeout
or failed invariants abort the whole group. Never broaden this lane to bypass a blocked dependency.
Archived contacts leave CRM lists and selectors; authorized CRM lookup resolves the old identifier.
New scalar references and edits to archived contacts are rejected. The alias does not grant
account access, membership, ownership, or a new authentication identity. Existing mappings block
further merges of either endpoint, preventing mapping chains and cycles.

## Execute and undo

After staged validation, deploy the exact reviewed migration and compatible backend using the
existing guarded release workflow. Generate a fresh production dry run. Its fingerprint binds
the before-state and reviewed proof. Reuse the operation UUID on retries:

```sh
node scripts/identity-reconciliation.mjs --command execute --case CASE_UUID \
  --operation OPERATION_UUID --fingerprint PLAN_SHA256 \
  --db-app APP --database DATABASE --output /private/path/result.json
node scripts/identity-reconciliation.mjs --command rollback --operation OPERATION_UUID \
  --db-app APP --database DATABASE --output /private/path/undo.json
```

A retry returns `already-applied` or `already-reverted`. Execution performs no external calls,
notifications, transactions or charges. Undo restores only fields changed by the merge, preserves
unrelated later edits, and removes the active archive mapping. A later edit to a merged field
blocks undo for review. History remains. Schema rollback refuses after any evidence/request use;
whole-database restoration is not the routine undo mechanism.

## Recurrence controls and known remaining work

`POST /parties` accepts `Idempotency-Key` (16–128 ASCII letters, digits, `_` or `-`). The key is scoped
to the authenticated CRM actor, serialized with a transaction advisory lock, and bound to the
validated payload. Replays return the same Party; changed payloads return 409 without leaking
another actor's contact. The web create dialog retains the key across failed retries. Separate
requests remain separate, even with shared attributes. Imports, API clients and jobs using this
endpoint must supply a stable key for each source operation; the optional header preserves
compatibility for existing clients. There is no global unique email/phone/name constraint.

Existing authentication username uniqueness and scoped social-sync/import keys remain in place.
The provider-subject follow-up replaces email-only Google matching with explicit authenticated
account linking and scoped immutable bindings. See [provider-prevention.md](provider-prevention.md)
for rollout, validation and remaining entry points. Public Live Session ingestion and older
creation clients without request keys still require further work before claiming comprehensive
recurrence prevention.

## Validation

Run `node --test scripts/__tests__/identity-reconciliation.test.mjs` and
`scripts/test-identity-reconciliation.sh` with `TDF_IDENTITY_TEST_DATABASE_URL` pointing to an empty,
dedicated test database. Tests cover conservative candidate hints, nontransitive groups,
permutation properties, retries, simultaneous connections, whole-group review, field conflicts,
soft ownership references, bookings, stale plans, archive references, rollback conflicts,
unrelated later edits, and private function/table permissions. CI runs these with the backend.
