# Event operations threat model

## Assets and trust boundaries

Protected assets include event visibility, identity links, guest tokens, schedules and location,
RACI responsibilities, private chat/evidence, contracts/signatures, provider credentials/events,
money/ledger/payout state, dispute documents, certifications, incident/medical/security plans, and
immutable audit history.

Trust boundaries are browser/mobile to API, offline queue to sync API, API to PostgreSQL, workers to
outbox/inbox, TDF to payment/email/push/calendar/webhook providers, public discovery to private event
data, organization to event-scoped grants, and guest invitation to canonical party identity.

## Principal threats and controls

| Threat | Example | Required controls and tests |
|---|---|---|
| Object-ID authorization bypass | Change event/task/invitation UUID to another visible event. | Contextual authorization after lookup; opaque IDs are not controls; negative cross-event tests. |
| Invitation privilege escalation | Public viewer creates invitation or account conversion inherits sender role. | Organizer/delegate guard, token digest, purpose/scope/time binding, atomic consume, attenuation assertion, replay/revocation tests. |
| Coproduction/transfer escalation | New coproducer becomes owner or keeps stale access after removal. | Separate ownership/grants, dual-approved transfer, revocation epoch/session recheck, model/assertions. |
| Offline stale authority | Queued mutation succeeds after role revocation. | Reauthorize at execution, expected grant/version, signed session freshness, conflict state. |
| Booking race | Two requests confirm the same artist/room/equipment. | Exclusion/capacity constraint inside one transaction, idempotency, concurrency test; justified override in separate privileged command. |
| Task dependency race | Concurrent edges create a cycle after both prechecks pass. | Per-event graph serialization/serializable transaction and database-backed test. |
| Responsibility orphan | Collaborator removed while still Accountable/Responsible. | Atomic reassignment or reject; RACI constraint and adversarial removal test. |
| Cross-task command key leak | A private sibling's UUID receipt changes another task's outcome. | PR 23 namespaces operation keys by canonical task ID inside the existing ledger; current task read is checked before receipt lookup; payload/actor hash binding and concurrent retry tests. |
| Timed responsibility loss | A replace-all editor omits unseen future or expired assignments. | PR 23 replaces one explicit active unbounded pair only, preserving old row/audit and every unrelated assignment; timed sources and expired required roles fail closed. |
| Contract substitution | Party accepts v1 while server confirms edited v2. | Immutable content hash/version, current-version locks, required-party acceptance uniqueness, model-based test. |
| Payment spoofing | Browser redirect marks payment paid. | Signature-verified webhook/server verification, environment/merchant/amount/currency/resource binding, provider inbox dedupe. |
| Duplicate side effect | Retried webhook issues tickets or payout twice. | Scoped idempotency, unique provider event/resource keys, ledger uniqueness, retry/reorder tests. |
| Separation-of-duties bypass | Requester self-approves event/PO/payout through another role. | Context-aware distinct actor policy, active grant time, immutable approver evidence, negative tests. |
| Visibility widening | Mention, attachment, search index, webhook, or chat leaks a restricted task. | Child visibility cannot exceed parent; field-level serializer; search/outbox consumers receive policy version; privacy tests. |
| Sensitive ranking/discrimination | Search secretly uses identity/sensitive attributes. | Allowlisted public factors, factor contribution display, policy version, audit, no protected/sensitive traits. |
| Tampering/repudiation | Delete no-show, approval, contract, or payout audit. | Insert-only DB role/trigger, hash/correlation chain where appropriate, backups/export, privilege tests. |
| Token/document exfiltration | Guest token or dispute evidence appears in URLs/logs. | Store token digest only, redact structured logs, short expiry, secure object storage, malware/content checks, access audit. |
| Time/currency confusion | Cross-zone event shifts instant; cents treated as dollars. | IANA+UTC round-trip property tests; explicit currency/minor units; checked arithmetic; no binary float. |

## Abuse and operational controls

- Rate-limit invitation creation/acceptance, discovery/contact, proposal spam, webhook endpoints, and
  public checkout by privacy-preserving keys.
- Moderate public opportunities and profiles; allow blocking/reporting; do not expose private
  availability detail beyond a policy-safe match result.
- Apply least-privilege service accounts. Payment and sensitive-document providers remain disabled
  by default; production activation requires security, legal, accounting, and operational review.
- Metrics must expose stuck holds, stale readiness, dead letters, reconciliation age, booking
  conflicts, invitation replays, authorization denials, and audit write failure. Logs omit raw tokens,
  contract bodies, payment secrets, identity documents, and private incident details.

## Residual risks

Finite model bounds, compromised owner accounts, malicious provider insiders, jurisdiction-specific
contract/tax defects, provider outages, and database-superuser tampering remain outside the modeled
guarantees. MFA/step-up authentication, key management, provider certification, legal review,
database administration controls, disaster recovery exercises, and penetration testing are required
before production activation.

### Authentication-to-command window (event transaction correction)

The original `loadAuthedUser` passed only party/roles/modules across pool transactions. A regression
then reproduced a snapshot read after token revocation using that already-authenticated context.
The [session fence contract](session-fence-contract.md) and `SessionFence` model define the repair:
an opaque request-local token-row/party/credential witness, current-row validation under `FOR SHARE`,
and lock retention through the actual event read/new command/replay transaction. Both supplied and
stored actor identities must match the captured party. Token deletion, deactivation, rebinding,
rotation and reset-purpose conversion fail closed; copied or missing witnesses do not authorize.
Fingerprint comparison uses `constEq`; Show redacts witness details. Existing event-grant fences
remain independently necessary. See [PR 09 evidence](pr-09-session-fence.md) for the concrete tests.

This is current-token validity, not permanent revocation epochs. Explicitly reactivating the same
credential reauthorizes it; permanent invalidation must rotate the credential or leave the old token
inactive. Other domain transactions and concurrent global role/catalog changes are not fenced merely
because their `AuthedUser` now carries a witness. Full `mkApp`, global auth-policy/session lifetime,
token storage hardening and production-schema rehearsal remain required before activation.

### Command target existence (response-envelope correction)

The earlier command function distinguished an absent event (`not_found`/404) from an unreadable
existing event (`forbidden`/403), unlike the GET snapshot's opaque 404. The
[command privacy contract](command-privacy-contract.md) and `CommandPrivacy` paired model refine
this policy: absent and unreadable commands now return the same exact 404 envelope before exposing
receipt or key conflicts. Internal denial diagnostics remain private and immutable; readable
targets still reject insufficient mutation authority with 403. Negative controls exercise both
distinct errors and premature receipt selection. A disposable SQL regression reproduced the old
leak before implementation. See [PR 08 evidence](pr-08-command-privacy.md) for concrete verification.
This does not claim constant-time access: lock waits, audit writes, database faults, privileged
observability and resource exhaustion remain possible side channels requiring additional review.

### Artist follow consent and stale client receipts (PR 16)

The shared public-artist journey now requires a click after authenticated session readiness
and a successful follow-state lookup; a URL return intent alone cannot dispatch a follow.
Session/profile generations prevent a late response from consuming another profile's
intent, including leaving and returning to the same profile. This is a client receipt
boundary, not server authorization or cancellation of a dispatched request; see the
[AF-01–05 contract](artist-follow-continuity-contract.md).

An independent source audit found that `TDF/Server.hs:fanFollowArtist` automatically creates
`PartyFollow` relationships in both directions between fan-club members. The branch is
unchanged and the synthetic browser tests do not exercise it. Do not infer member-to-member
consent from the artist-follow click or declare complete privacy compliance. Review the
intended club policy, explicit consent, discoverability and revocation effects before a
separate implementation; no data deletion or retroactive consent is authorized here.

### FanHub optional exit and account-bound guidance (PR 17)

The global dismissal marker could affect unrelated accounts and was not a canonical
receipt. [FH-01–06](fanhub-onboarding-contract.md) replaces its use with validated
authenticated eligibility and an explicit empty completion command. Reads and receipts
check session object/generation/mounted lifetime, never use tokens as cache keys, and
reject malformed or nonterminal HTTP-success payloads. Local guest/manager dismissal is
ephemeral presentation, not account completion. An older GET cannot reopen a terminal
acknowledgement in the same context. Same-context duplicate exits coalesce, while stale
responses cannot hide or show a new account's guidance. These client fences do not undo
already-dispatched server requests or establish cross-tab cookie isolation; existing
server authorization and conditional completion remain independent requirements.
