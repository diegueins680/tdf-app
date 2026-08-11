# Unified Admin Operations Control Center

Status: implemented behind `operations_enabled`; external providers remain disabled until their credential checklist passes.

## Repository and current-state analysis

The implementation follows the live repository, not an aspirational rewrite:

- `tdf-hq` is a Servant application using Persistent and PostgreSQL. Authentication yields an `AuthedUser` with party, roles, and modules. Existing business entities remain authoritative.
- `tdf-hq-ui` is React/TypeScript/Vite with MUI, React Query, protected routes, bilingual locale modules, and an OpenAPI-generated type surface.
- `tdf-mobile` is an Expo Router/React Native application with the same generated OpenAPI contract, feature-registry guard, React Query, SecureStore auth, locale/timezone settings, and deep links.
- `tdf-hq/assets/feature-registry.json` is the cross-surface navigation and authorization registry. Mobile registry code is generated from it.
- Production schema rollout is an explicit SQL manifest. The operations migration is additive; its rollback disables capture and visibility without deleting evidence.
- Existing per-recipient notifications are delivery-oriented and cannot represent shared ownership, lifecycle, SLA, event threads, provider replay, or immutable operational history. They remain compatible and are not repurposed.

## Business specification

An operational work item is a shared, durable responsibility around an existing source record. It is not a copy of that record and resolving it has no source-domain side effect. Business actions (approve a registration, issue an invoice, verify a transfer, resolve a booking conflict) continue through their existing APIs and guards. Work lifecycle commands are separately versioned.

Priority order is encoded in capture and SLA policy: registrations/reservations/payments/customer requests, management trust, conversion, then repetitive-work reduction. Correlation keys group related events into one thread; provider event IDs and event deduplication keys make replay safe.

Covered persisted sources include course registrations and receipts, bookings, invoices, payments, packages, service orders, leads, trials, marketplace orders, maintenance, inventory reorder warnings, artist approval, feature-access approval, proposals/quotes, projects/tasks, social events, WhatsApp, Instagram, Facebook, integration failures, and manual/uncorrelated inbound requests. Existing web/mobile form and gateway handlers feed the same business tables, so their committed events are captured atomically.

## Architecture

```text
business transaction / verified provider webhook
                    │ same PostgreSQL transaction
                    ▼
        immutable operations_domain_event
                    │ trigger
                    ▼
          transactional operations_outbox
                    │ SKIP LOCKED, ordered per aggregate
                    ▼
      idempotent work-item/thread projection
          │          │             │
          │          │             └── SLA/reminders/escalations
          │          └── stream replay log (monotonic event id)
          └── immutable audit / delivery / failure queues
                         │
             authenticated REST commands
                         │
             Web dashboard + Expo staff app
```

Each worker claims at most 250 outbox records per tick with `FOR UPDATE SKIP LOCKED`. Multiple application replicas may drain independent aggregates concurrently; a predecessor predicate preserves per-aggregate order without a global lock. Each aggregate has a monotonically assigned sequence. Consumers are at-least-once; durable uniqueness constraints make visible effects idempotent. Failures use exponential backoff plus jitter and enter the admin-visible dead-letter queue after the configured attempt ceiling.

The repository has no established Servant SSE/WebSocket convention. Release 1 therefore uses an authenticated, resumable delta feed (`GET /operations/events?afterId=`) every two seconds in the web control center and bounded 15-second refresh on mobile. A reconnect resumes from the last monotonic ID; React Query invalidation makes repeated events idempotent. This meets the three-second visibility objective without adding an unproven streaming runtime. Native SSE can later wrap the same replay table without semantic or client-contract redesign.

## Data model

- `operations_domain_event`: immutable fact with aggregate, provider ID, correlation, sanitized JSON, and continuous/business clock selection.
- `operations_outbox` and `operations_aggregate_sequence`: ordered, concurrency-safe, leased, retried, and dead-lettered delivery of committed facts.
- `operations_work_item`: shared operational state, scope, source reference, SLA state, assignee/team, bilingual text, timestamps, version, and non-secret metadata.
- `operations_work_item_event`, `operations_note`, `operations_mention`: complete thread and internal collaboration.
- `operations_sla_timer`, `operations_sla_reminder`: business-calendar deadlines and 50/80/100/150 percent escalation.
- `operations_outbound_delivery`, `operations_inbound_receipt`, `operations_integration_failure`: consent-aware delivery, signed-provider receipt identity, retry, replay, and dead letter.
- `operations_approval_request`: requester/approver separation and consequential financial action gate.
- `operations_admin_audit`: append-only before/after evidence with actor, role, scope, request/correlation ID, client, approval reference, and reason.
- `operations_stream_event`: authorization-filtered replay cursor.
- `operations_saved_view`, `operations_push_subscription`: persisted staff preferences and encrypted native tokens.
- `operations_backfill_run`: dry-run/progress/status evidence; event uniqueness is the durable resume cursor.

Large append-only tables have time BRIN indexes. Five-year evidence remains online; resolved items auto-archive after 90 days but remain searchable. At sustained two-year volume, monthly cold-table partition conversion can be performed behind the immutable event/audit interfaces without changing work semantics.

## UX and interaction decisions

`/dashboard/operations` preserves the existing dashboard and adds:

- 14 real KPIs, a six-column priority Kanban, and the paginated operational inbox.
- Server-side search and filters for all specified dimensions; persisted private/shared views, columns, widgets, and subscriptions.
- A detail drawer with shared seen state, assignment, legal transitions, waiting dependency/resumption, notes/mentions, full history, source link, and domain quick-action catalog.
- Confirmed bulk lifecycle actions with impact preview. Optimistic versions prevent lost simultaneous changes.
- Loading, empty, partial failure, reconnect, retry, and no-mock fallback states.
- Spanish and English labels, semantic controls/headers, keyboard-native MUI interactions, responsive layouts, and organization timezone/currency values.

The Expo route `/operations` provides a responsive filtered queue, shared seen state, self-assignment, guarded lifecycle actions, internal notes, thread history, authorized record deep links, pull-to-refresh/offline errors, and encrypted push registration. Push bodies must carry only an opaque work-item ID; the app fetches authorized detail after opening.

## Concurrency and security boundaries

- Every command is server-authorized and scoped by active organization/branch membership. Frontend hiding is supplementary only.
- List, item detail, delta feed, metrics, saved views, failure replay, assignment, mentions, and push registration reapply scope.
- Teacher/Engineer lists require assignment and an allowed operational domain. Security incidents are Admin-only even if an ID is guessed.
- Projection metadata always strips tokens, authorization, signatures, secrets, certificates, keys, PAN/CVV/seed phrases, raw payloads, tax IDs, addresses, email, and phone. Full source routes apply their existing field-level policy.
- Work commands use an expected version and return `409` on a race.
- Audit and domain-event updates/deletes fail in PostgreSQL. Ordinary administration offers no hard delete.
- Provider tokens/certificates are never stored in provider JSON. Push tokens require `tdf.push_encryption_key` and are encrypted with pgcrypto.

## Assumptions and compatibility

- The seeded organization/branch map the current single-organization deployment. They are disabled at migration time and are a compatibility bridge; new tenants receive explicit UUID scope/configuration.
- `America/Guayaquil` and `USD` are seeded defaults, not constants in API computation.
- File-backed legacy contracts have no authoritative relational ID and are not copied into operations. A manual linked work item or the persisted proposal/quote flow is used until contracts migrate to a business table.
- Existing business commands and routes remain unchanged. The new API is additive under `/operations`.

## Implementation plan and future-safe sequence

1. Apply schema with operations disabled and validate health/lag queries.
2. Configure organization/branches, business hours, holidays, scope membership, encryption, and provider registry.
3. Dry-run then execute bounded backfill; drain outbox and reconcile counts.
4. Enable the control center for an Admin/Manager canary group, then Accounting/Reception, then assigned specialists.
5. Activate providers independently only after official sandbox verification. Provider failure never disables core work capture.
6. Review load evidence and partition thresholds quarterly; keep the public API/correlation semantics stable.
