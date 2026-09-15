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

### Authentication-to-command window (identified during HTTP verification)

`TDF.Auth.authWithToken` calls `loadAuthedUser` in its own pool transaction, then passes an
`AuthedUser` containing party/roles/modules to the event handler. `TDF.EventOperations.Server`
subsequently starts another transaction and supplies the party ID, not the session token or a
session epoch, to its SQL function. Event grants are rechecked/fenced there; token activity is not.
Thus subsequent-request token-deactivation tests do not establish that a token revoked between
authentication and command execution will be rejected. This is a code-inspection finding, not a
completed runtime exploit test or an assertion that all revocation races are fixed. Before
activation, model and test that interleaving and introduce a reviewed session-bound transaction
guard without weakening the existing global authentication semantics. Full `mkApp` middleware and
other domains require their own integration checks as well.

The command function also distinguishes an absent event (`not_found`/404) from an unreadable
existing event (`forbidden`/403), unlike the GET snapshot's opaque 404. Code inspection therefore
identifies event-existence metadata as a separate privacy-policy gap for private-event rollout.
The new HTTP tests enforce the currently documented command contract; they do not establish
indistinguishability of absent and inaccessible POST targets. Model and review that error-envelope
policy before enabling private resources rather than assuming object-ID isolation proves it.
