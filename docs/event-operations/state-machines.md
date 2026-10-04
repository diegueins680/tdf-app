# Event operations state machines and contracts

Errors use stable symbolic codes; transport-specific HTTP mappings are shown in parentheses.
Every accepted or rejected sensitive command records an immutable audit fact and correlation ID.

## Canonical event lifecycle

| From | Allowed target | Authority | Required guard/effect |
|---|---|---|---|
| Draft | Planning, Cancelled | Owner/event manager | Required identity, owner, timezone, and primary schedule valid. |
| Planning | Pending approval, Cancelled | Owner/event manager | Readiness rules evaluated; submit freezes review revision. |
| Pending approval | Planning, Cancelled | Owner/event manager | Return/cancel with reason. |
| Pending approval | Approved | Independent event approver | Approver differs from requester where policy requires separation of duties. |
| Approved | Published, Planning, Cancelled | Owner/event publisher | Publishing requires visibility policy, public fields, venue/session validity, and required approvals. |
| Published | Staffing/booking, Planning, Reprogrammed, Cancelled | Owner/event manager | Public projection/outbox updated atomically; reprogram/cancel starts ticket/contract impact workflow. |
| Staffing/booking | Ready, Reprogrammed, Cancelled | Owner/event manager | Required bookings, contracts, RACI, safety gates, and run-of-show checks pass or authorized exceptions exist. |
| Ready | In progress, Reprogrammed, Cancelled | Owner/operations lead | Start instant reached or authorized early-start reason; incident channel activated. |
| In progress | Completed, Cancelled | Operations lead | Completion records actuals, incidents, returns, no-shows, and unresolved attention items. |
| Completed | Settlement pending | Owner/finance operator | Ticketing, bookings, expenses, refunds, disputes, assets, and deliverables reconciled into settlement inputs. |
| Settlement pending | Settled | Independent finance approver | Reconciliation balanced; contractual/milestone conditions and provider evidence satisfied. |
| Settled | Archived | Records manager/owner | Retention/export policy recorded; immutable records remain queryable to authorized parties. |
| Reprogrammed | Planning, Cancelled | Owner/event manager | New revision and schedule required; old instant retained in history; affected parties notified. |
| Cancelled | Archived | Owner/records manager | Cancellation policy snapshot, reasons, ticket/contract/refund consequences, and outstanding disputes retained. |

Any other edge fails with `event_transition_invalid` (409). Missing authority returns
`event_transition_forbidden` (403). A stale expected revision returns `version_conflict` (409) and
does not mutate state. An already-seen idempotency key returns the stored result without repeating
notifications, booking, financial, or public-projection effects.

Rollback never erases history. A reversible transition creates a new transition event to the
permitted prior workflow state; contractual, ticketing, booking, payout, and public side effects use
compensating commands. Approved/settled transitions require separation of duties. Lifecycle
extensions must declare capabilities and transition policy in the catalog, pass the same guards,
and cannot weaken canonical invariants.

## Event transition side effects

| Transition family | Atomic effects | Asynchronous/outbox effects |
|---|---|---|
| Publish/unpublish by return to Planning | Set public projection eligibility and revision | Search index, followers, calendars, caches; deduplicated by transition ID |
| Reprogram | Create schedule revision; invalidate readiness | Notify ticket holders/collaborators; re-evaluate holds, bookings, travel, refunds |
| Cancel | Freeze cancellation policy and affected obligations | Provider refunds, contract cancellation/no-show workflows, resource release, notices |
| Start/complete | Record actual timestamps and unresolved incidents | Live channel changes, task escalation, inventory return reminders |
| Enter/finish settlement | Snapshot reconciliation inputs; independent approval | Payout/refund jobs only after verified provider evidence; alert on timeout |
| Archive | Set retention/export policy and lock ordinary mutation | Search removal and cold-storage workflow; audit remains available |

## Invitation lifecycle

`issued -> accepted | rejected | revoked | expired`; an accepted invitation may later be revoked.

- Accept preconditions: intended recipient proves control of the single-purpose token; token digest
  matches; invitation is issued; `now < expires_at`; command/idempotency key unseen.
- Accept effects: link or create the canonical party identity, preserve assignments/activity, create
  only the invited scopes and time bound, consume token, and append audit. Never copy sender roles.
- Replay returns the original safe result. Expired/revoked/rejected tokens never create a grant.
- Revocation removes derived active access without deleting the invitation or actor history.

## Hold and booking lifecycle

`requested -> held -> confirmed -> in_progress -> completed`, with terminal/attention alternatives
`expired`, `released`, `cancelled`, `no_show`, `disputed`.

- Hold requires a normalized occupied interval, capacity/exclusivity policy, expiry, holder, and
  idempotency key. It does not imply a contract or payment.
- Confirm is one database transaction: lock/serialize the resource policy, expire stale holds,
  verify the exact contract version and authority, insert the exclusion-backed allocation, advance
  state, and append audit/outbox. A conflict returns `booking_conflict` (409).
- Override requires explicit policy, authorized actor, non-empty reason, affected resource/interval,
  and immutable audit. It cannot be smuggled through an ordinary retry.
- Setup, soundcheck, travel, teardown, and recovery buffers are part of the occupied interval.

## Task lifecycle

`open -> in_progress -> in_review -> completed`; `in_review -> rejected -> in_progress`; completed
tasks may be reopened by an authorized approver with reason. Cancelled is explicit, never deletion.

- Dependency insertion fails with `task_dependency_cycle` (409) if the new edge reaches its source.
  Cycle detection and insertion must share a serializable transaction/advisory lock.
- Completion requires all prerequisites completed. An emergency override requires authority, reason,
  policy, and audit; it emits readiness attention.
- Actionable tasks always have one Accountable and at least one Responsible party. Collaborator
  removal atomically reassigns every required role or fails with `responsibility_orphan` (409).
- Safety-critical acceptance/rejection uses expected version; concurrent edits surface conflicts.

## Contract, milestone, and payout lifecycle

- Offer: `draft -> proposed -> accepted -> confirmed`; material edit creates a new draft version and
  prior acceptances no longer count. Rejection, withdrawal, expiry, termination, cancellation, and
  dispute are explicit terminal/attention states.
- Confirmation requires every required party to accept the same immutable version. The accepted
  payload, hash, locale, jurisdiction metadata, actor, and time are retained.
- Milestone: `pending -> submitted -> approved | rejected`; approval is version-conditioned.
- Payout: `not_requested -> pending -> verified -> released | failed | reversed | disputed`.
  Release requires confirmed contract, applicable approved milestone/completion, verified provider
  evidence, balanced ledger posting, independent finance authority, and unused idempotency key.
- Browser redirects cannot enter `verified` or `released`.

## Offline and collaborative writes

Every command carries `command_id`, aggregate ID, actor/session, expected revision, client-created
time, and schema version. Commutative annotations may merge; scheduling, RACI, task approval,
contract, booking, visibility, and finance conflicts become `needs_attention`. Stale authorization
is re-evaluated at synchronization time. A revoked grant or stale session cannot be replayed into a
successful mutation.
