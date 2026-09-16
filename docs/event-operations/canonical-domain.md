# Canonical event operations domain

## Integration boundary

`social_event` remains the aggregate identity. New event-operations tables are additive relations
around existing parties, venues, rooms, resources, directory profiles, bookings, checkouts, ledger,
reviews, chat, notifications, and tickets. Compatibility adapters translate legacy state and
organizer fields until every caller uses the canonical services.

```text
Party/User/Organization
        │ owns, coproduces, receives scoped grant
        ▼
 existing social_event ── revisions ── sessions ── spaces/venues/virtual channels
        │
        ├── plan/template ── workstreams ── tasks ── RACI/dependencies/evidence
        ├── opportunity ── application/proposal ── engagement ── contract versions
        ├── resource bindings ── holds/bookings ── exclusion calendar
        ├── budgets/procurement ── canonical checkout/ledger ── payout/settlement
        └── discussions/activity/outbox/audit ── notifications/calendar/webhooks
```

## Canonical entities and constraints

| Entity | Key relations | Non-negotiable constraints |
|---|---|---|
| Event ownership | event, owner party, kind, validity, provenance | At least one active owner; ownership transfer is dual-recorded and cannot orphan the event. |
| Event grant | event/resource, grantee party, scopes, valid interval, issuer | Scope attenuation, revocation, contextual evaluation, no role inheritance through invitation/coproduction. |
| Event revision | event, monotonically increasing version, content hash, author | Immutable after creation; current pointer changes with optimistic concurrency. |
| Event session | event revision, local date/time, timezone, UTC start/end | Valid IANA zone; positive half-open interval; round-trip preserves intended instant and local metadata. |
| Event space assignment | session, venue/room/stage/virtual resource | Typed binding to existing domain; visibility and capacity policy explicit. |
| Plan/template version | event type/policy, relative schedule, tasks/RACI/dependencies/budget/checklists/gates | Immutable source version; clone records provenance; music examples are seeds, not type restrictions. |
| Task | event/workstream/group/parent, state, expected version, schedule | DAG; deterministic schedule; actionable task has one A and at least one R; completion dependency guard. |
| RACI assignment | task, party, role, valid interval | Unique Accountable per actionable task; required roles cannot be silently removed. |
| Opportunity/proposal | directory listing/profile/event, terms/rate/availability | Reuse directory visibility/moderation; ranking factor explanation stored; sensitive traits excluded. |
| Engagement | event, buyer/hirer, provider, opportunity/invitation, schedule, state | One canonical lifecycle; verified completion is reputation gate. |
| Contract/version/acceptance | engagement, immutable payload/hash/version, required party consent | Confirmation only when all required parties accept the same current version. |
| Resource binding | existing person/room/venue/asset/provider to canonical resource | Stable unique binding; no parallel availability calendar. |
| Hold/allocation | resource, occupied interval, capacity units, expiry, state | Exclusion/capacity check in confirmation transaction; idempotent expiry/release. |
| Budget/procurement | event, currency, estimate/commit/actual, quote/PO/approval | Exact amounts; policy/version snapshots; separation of approval duties. |
| Payment/settlement | canonical checkout, provider evidence, ledger, payable/payout | Browser redirect is not evidence; balanced immutable accounting; payout gates and idempotency. |
| Discussion/decision | event/task/incident scope, participants, visibility | Contextual read/write; mentions and attachment visibility cannot widen parent access. |
| Command receipt | actor, aggregate, command ID, expected version, result | Unique scope key; retries return stored result and never duplicate side effects. |
| Audit event | actor/session, authority, command, before/after, reason, correlation | Append-only application role; sensitive payload minimization; retention/export policy. |

## API command envelope

Every material mutation accepts the equivalent of:

```json
{
  "commandId": "uuid",
  "expectedVersion": 7,
  "clientCreatedAt": "2026-09-14T15:00:00Z",
  "schemaVersion": 1,
  "reason": "required for exceptional transitions only",
  "payload": {}
}
```

Success returns the authoritative aggregate version and stable command result. Duplicate command IDs
within the same operation/aggregate/actor scope return that result. A reused key with a different
payload hash returns `idempotency_key_reused` (409). Stale versions return a typed conflict with the
current version and safe merge metadata, never implicit last-write-wins.

## Transaction boundaries

- Lifecycle transition: authorization, expected revision, guard evaluation, state/audit/outbox write.
- Task graph mutation: graph lock, cycle validation, relation write, version/audit/outbox write.
- Collaborator removal: grant revocation plus all mandatory reassignments or no change.
- Booking confirmation: expiry, policy lock, exclusion/capacity allocation, engagement/booking state,
  audit and outbox in one transaction.
- Contract confirmation: exact current version and every required acceptance locked/validated.
- Financial transition: verified provider event inbox claim, amount/currency/resource binding,
  checkout/payment state, ledger postings, audit/outbox, unique provider event key.

Background delivery uses transactional outbox and provider/webhook inbox records. Consumers are
idempotent; retries cannot create a second task, hold, booking, contract, charge, refund, payout, or
notification.

## Template examples

Seed versioned, editable examples for concert, festival, DJ set, live electronic performance,
studio live session, tour date, hybrid/streamed show, conference, community workshop, and private
gathering. Seeds reference generic capabilities (audience, stage, broadcast, catering, safety,
transport) so no schema or code path is music-only.
