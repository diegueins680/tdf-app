# Deletion request operations

Canonical policy: [request-workflow.json](request-workflow.json), owned by
PRIV-DELETE-001. The private executable ledger is
[scripts/privacy-request-ledger.py](../../scripts/privacy-request-ledger.py).
This process records operational evidence; it does not itself delete customer
rows, send email, revoke provider access or certify that an evidence statement is
true. Production inbox coverage, operator assignment and ledger backups must be
verified before declaring this workflow operational.

## Discovered obligations and current evidence

The shipped `/data-deletion` page directs Instagram/WhatsApp deletion requests to
`info@tdfrecords.net` and promises confirmation and completion within30 days.
The initial audit found `/mobile-app/data-deletion` referenced
`privacidad@tdfrecords.com` and `soporte@tdfrecords.com`. The owner subsequently
confirmed `info@tdfrecords.net` as the managed channel; the mobile legal pages
now use that inbox. This integration adds authenticated web intake at
`/cuenta/eliminar`, with a30-day target subject to identity and applicable
retention; it is not operational until its strict backend is deployed.
Follow the [intake/queue procedure](../../docs/mobile-program/account-deletion-operations.md)
and retain the private case-to-feedback mapping when opening the same ledger. Use the earliest actual receipt timestamp for every
case. Waiting for identity, retrying or changing operators never restarts it.

Repository/runbook searches, operational briefs and scoped searches of the
connected mailbox found a March retention sign-off but no maintained request
ledger or execution runbook. Empty search results do not establish that the
published aliases are unmonitored, nor that there are zero pending requests.
The historical sign-off is not current deletion evidence. The owner requested
creation of a workflow if none was found on2026-10-05; this is that implementation.
No real request has been entered or customer data changed by the audit.

## Activate operations

The system owner assigns a primary privacy operator and a backup with authorized
access to the owner-confirmed `info@tdfrecords.net` inbox and authenticated
feedback queue. Investigate any historical aliases for outstanding requests;
their former publication does not prove current delivery. Confirm actual delivery and access
through existing mail administration; do not infer delivery from a website mailto
link, DNS record or an empty search. Do not send a verification message without
explicit messaging authorization. Reconcile existing inbox requests before
claiming complete intake. Record this setup in the protected operational evidence
store, including monitoring, restore ownership and overdue escalation recipients.

Create an owned0700 directory outside every Git checkout on the protected
operations host. The ledger file must be0600 and on backed-up private storage.
Never commit the SQLite database, evidence receipts, inbox exports or customer
identifiers. For example, with an operator-chosen private directory:

```sh
python3 scripts/privacy-request-ledger.py --ledger /private/operator/path/requests.sqlite init
python3 scripts/privacy-request-ledger.py --ledger /private/operator/path/requests.sqlite report
```

The command does not create a missing parent directory and rejects symlinks,
shared permissions and the current source checkout. Operators remain responsible
for avoiding other checkouts, cloud shares, insecure backups and shared OS accounts.
Use individually authenticated OS access; ledger actor identity is the invoking
OS UID, not a caller-supplied identity. File ownership means this tool has one
active owner UID: a backup operator with a different UID cannot simply share it.
A controlled host-administrator handoff must transfer private directory/file and
evidence ownership, verify the external checkpoint, and document operator access.
The tool does not provide that handoff or multi-user authentication. Old retry keys
remain bound to their original UID and conflict under a different operator; inspect
the committed case history before choosing a new action rather than replaying an
already completed effect. Privileged host administrators remain in
the trust boundary. Policy changes require a reviewed ledger migration; editing
the policy cannot silently alter an existing ledger's deadlines or transitions.

## Intake and evidence

For each message, store the original request in the private evidence store. Do
not put its name, email, phone, Party ID or free text in the ledger. Use a random
idempotency key for the original message and retain its mapping privately. Reuse
that key on retry. Record channel and original receipt time, not processing time.

A receipt is a0600 JSON file with only `kind`, `caseId`, `artifacts` and, for scope
or effects, `surfaces`. `artifacts` is a nonempty list of SHA256 references to
reviewed private artifacts. Opening evidence uses kind`received_request` and
caseId`null`; later evidence binds the generated case UUID. The CLI hashes the
receipt and binds its digest to the action, actor and retry key. It does not fetch
or validate the truth of referenced artifacts. The operator must retain them and
verify their content, origin, access controls and hash before recording a step.

```sh
python3 scripts/privacy-request-ledger.py --ledger /private/operator/path/requests.sqlite open \
  --channel mobile --received-at 2026-10-05T12:00:00Z \
  --key RANDOM_OPAQUE_MESSAGE_KEY --evidence /private/operator/path/intake.json
python3 scripts/privacy-request-ledger.py --ledger /private/operator/path/requests.sqlite step \
  --case-id CASE_UUID --action verify_identity --expected-version 0 \
  --key RANDOM_OPAQUE_ACTION_KEY --evidence /private/operator/path/identity.json
```

Verify the requester controls the relevant account/provider identity using an
existing authenticated channel. Email sender text alone is insufficient. Request
only the minimum information needed; never request passwords or bearer tokens.
If clarification is needed, record`request_identity`, preserve the deadline and
escalate delay. Sending notices remains an operator action, not a ledger effect.

## Scope, execution and recovery

The policy's complete `surfaces` list must be accounted for in both the plan and
post-execution verification. Each entry records a disposition (`erase`,
`anonymize`, `retain`, `not_applicable`) and a proofHash. `not_applicable` requires
reviewed evidence too; it is not a shortcut for an uninspected domain. A retained
entry also requires a future UTC reviewAt and a documented scoped rationale in
its proof. Do not invent statutory periods from this runbook.

Resolve the canonical Party and all linked profiles, roles, provider identities,
content, media and imports before planning changes. Inspect current foreign keys
and application ownership rules. Distinguish messages from a deleted-message
webhook, an account deletion request, and a public-content takedown. A Meta message
tombstone proves none of the account workflow. Preserve immutable financial/audit
records only when the approved scope requires retention; explain precisely what
remains. Include blob providers, exported copies, caches, search indexes, analytics,
notifications and backups. Backups require an expiry/review date and a restoration
suppression procedure so erased live data is not silently reintroduced.

Admit the reviewed scope with`plan`, then record`start` before execution. Use the
applicable service-specific, reviewed procedure and recovery plan; this ledger
provides no generic cascading DELETE command. Revoke appropriate sessions and
provider grants as part of the verified plan. Protect unrelated actors and shared
entities. A failed or ambiguous provider/DB outcome is`fail`, not completed.
Reconcile it before`start` retry; use`replan` if scope changes. No blind retry of
an irreversible provider effect is authorized by the ledger.

Post-execution evidence must independently inspect the planned surfaces and
public/private access boundaries, including blob reachability and restoration
suppression. Record`verify_effects` only when each disposition is substantiated.
A changed disposition requires a revised plan. Only then deliver the completion
notice through the verified channel, explaining retained data, reasons and review
schedule. Record`close` using kind`delivered_completion_notice` and actual delivery
evidence. A queued draft, attempted send or failed delivery is insufficient.
`closed` means the case disposition was communicated, not that all data was erased.
For a closed case, `review_retention` appends case-bound proof of later
erasure/anonymization or a justified future review date. Provide the complete
surface map; non-retained surfaces cannot be rewritten, and retained data cannot
be relabeled not_applicable. Original deadline, closure time and event history
remain intact; reports retain a completedLate flag when closure missed the deadline.
A verified withdrawal is permitted only before execution; partial execution must
be reconciled, not relabeled withdrawn.

## Monitoring and restoration

Run`report` at least daily and after each operator handoff. Exit0 means no current
attention flag; exit2 means due within7 days, overdue or retained data review due;
exit1 means ledger/policy/input failure. Monitoring must treat1 and2 as actionable
and retain a timestamped heartbeat outside customer data. No scheduler or email
alert has been installed by this audit, so do not claim unattended monitoring.
Unverified identity does not pause the30-day clock. Escalate unresolved cases to
the system owner before the deadline; do not close them to make the report green.

SQLite transactions serialize writers; expected versions reject stale commands;
retry keys bind actor/action/receipt. Append-only triggers and a hash chain detect
ordinary mutation/corruption, not privileged rewriting or truncation without an
external checkpoint. Back up ledger and private evidence together, retain the
latest event hash/checkpoint separately, and test restoring them into an isolated
private directory. Never overwrite a live ledger to test recovery. Do not regard
a clean local test as production mailbox, deletion or restoration evidence.
