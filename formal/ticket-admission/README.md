# Ticket admission boundary

Scope: `TDF.Ticketing.Admission.admitTicket`, invoked by the existing authenticated event check-in handler.

Precondition: PostgreSQL, the additive admission audit migration, and one `runSqlPool` transaction. The actor comes from server authentication. Event, order and ticket locks remain held through commit. Event ownership must already exist; a scan never claims an unowned event. All ticket identifiers remain scoped to the event.

Invariants: only an issued ticket on a paid order enters; cancellation/refund denies access; only one concurrent caller can consume a ticket; state and audit commit or roll back together. The audit primary key independently prevents a second admission. A cached/downloaded QR is only a bearer credential: screenshots are usable by whoever presents first and cannot be distinguished from the original, but subsequent reuse is rejected online. No offline green-light is introduced.

Credentials: new ticket codes retain the UUID v4 random bits (122 bits), replacing the legacy 12-hex truncation. The parser continues to accept legacy 12-hex codes. QR responses use this opaque code, without holder email, timestamps, or the former hardcoded HMAC key. A QR is not a signed statement of payment; PostgreSQL remains authoritative.

Evidence: `node --test scripts/__tests__/ticket-admission-model.test.mjs` exhausts an eight-state finite abstraction and detects replay, authorization and missing-audit mutants. This is a design check, not a proof of implementation refinement. `test/TicketAdmissionMain.hs` exercises the actual Haskell function on PostgreSQL with eight simultaneous connections, denied states, wrong event/actor, credential rotation and audit-failure rollback. The same harness exercises `Inventory.reserveTicketInventory`, the function used by public checkout, with eight simultaneous buyers for one and two remaining tickets, cross-tier event capacity, and transaction rollback. It does not prove provider payment, full HTTP authentication, staff delegation or offline safety. The production payment and release gates remain separate.

Run `bash scripts/test-ticket-admission.sh` with `TICKET_ADMISSION_TEST_DSN` pointing to a fresh database named exactly `tdf_ticket_admission_test`. The script loads only that disposable fixture and applies the migration twice. CI runs this beside the existing backend tests. Rollback refuses to discard a populated audit; after first admission retain the schema and recover forward.

Transfer boundary: `TDF.Ticketing.Transfer` reuses the existing `ticket_transfer` audit records. Create, accept and cancel lock event → order → ticket → invitation in a single transaction. Acceptance rechecks paid/issued/unused state, current authority, tier permission, invitation expiry and event start; only one recipient can complete it. Completion rotates the ticket credential and retains the accepted actor and timestamp. Original buyer order responses omit transferred credentials; authenticated lists follow current holders. The QR response uses the same ticket snapshot for authorization and rendering. Invitation-list reads recheck ownership in the query.

Invitations are opaque bearer capabilities plus an authenticated session; recipient email is contact information, not verified identity or authentication. Do not describe this as email-bound delivery. New invitations retain UUID v4 entropy. The PostgreSQL harness covers simultaneous acceptance/creation, replay, invalid states, former-holder invitations, old-code rejection and rollback on a rotation uniqueness failure.

Remaining activation gates: the event-specific 13:30 transfer cutoff must be configured in the existing versioned checkout policy using the new nullable `transfer_deadline` (immutable after approval; event start remains an upper bound), adult-recipient acknowledgement and transactional invitation delivery. No transfer feature is advertised as fully configured for event 141. Provider payment, staff delegation, durable communications, full HTTP ownership regression and production release remain separate gates.

Public checkout transfers also enforce the purchased policy’s `transfer_allowed`, deadline and runtime payment state. A disputed payment cannot transfer or check in even if a stale legacy order still says paid. Admission accepts a partially refunded order only for an individual ticket that is still issued and unused; refunded ticket state always denies entry. The additive deadline migration preserves legacy null deadlines and history capture; rollback refuses to discard any configured deadline.

Order quantity uses the existing versioned checkout policy's `max_tickets_per_order`, bounded to 1–100. The additive migration preserves the previous limit of 100 on existing policies. The server rejects excess quantity before creating an order; a PostgreSQL insert trigger independently checks the referenced policy under `FOR SHARE`, so bypassing the frontend or handler cannot create an oversized runtime. Approved/retired limits are immutable, policy history includes the new column, and rollback refuses to remove a configured nondefault limit. Apply the migration before deploying the new backend.

The storefront exposes optional `maxTicketsPerOrder` for gradual client rollout; an older backend omits it and clients retain the legacy limit. UI tests bypass HTML validation to check the application guard and exercise the exact boundary. The real PostgreSQL migration test checks boundary acceptance, oversized rejection with transactional inventory rollback, approval/retirement immutability, migration replay and guarded rollback. These are per-order limits, not proof of a cumulative identity-based buyer quota or a payment-provider purchase. Event 141's approved value is four, but its commercial policy remains inactive/unconfigured until the fiscal and provider gates are resolved.

The shared lock and database constraint choices follow [PostgreSQL row locking](https://www.postgresql.org/docs/17/explicit-locking.html#LOCKING-ROWS) and [constraints](https://www.postgresql.org/docs/17/ddl-constraints.html); cross-table authority uses a trigger, not a CHECK expression that reads another table.

Per-ticket refund projection uses `TDF.Ticketing.Refund` around the existing
`RefundStore`. The request wrapper locks event/order/tickets before creating the
canonical financial reservation, so it shares admission/transfer lock ordering.
A full-ticket selection uses deterministic ID ordering to partition the immutable
order total, including remainder cents, without overflow. Each selected unused,
untransferred ticket becomes `refund_pending`; admission and transfer reject it.
Unknown provider outcomes retain this fence. Only pre-execution canonical
cancellation restores issuance. Verified completion commits ledger, internal
credit note, ticket revocation and one inventory release together. Partial refunds
leave the other tickets paid/issued; a replay cannot release capacity twice.
An immutable allocation audit and partial unique index prevent competing active
allocations. Used history cannot be rolled back or deleted.

The finite two-ticket model covers reservation, admission, processing, cancellation
and completion. Its three deliberate mutants permit a scan during reservation,
release inventory twice, or cancel uncertain processing; each violates an invariant.
Actual PostgreSQL tests execute the Haskell financial and ticket stores, including
concurrent requests/scans/completions and inventory-drift rollback. The provider
fixture uses a synthetic payment and reduced schema; it does not replace official
sandbox or full production-schema/API qualification.

The existing organizer request/approve/reject endpoints now use an immutable
legacy-request to canonical-refund binding for PayPal ticket orders. Amount-only
requests select whole, unused, untransferred tickets in ascending ticket-ID order;
omitting the amount selects the remaining eligible tickets. The organizer can
record a guest's support request under their own authenticated identity. A
different organizer/strict financial admin must approve it; no guest party is
invented. Processing requests cannot be cancelled and the UI offers only a status
query. Explicit ticket selection and guest self-service are not provided by this
amount-only endpoint.

The provider execution claim commits before HTTP. Processing retries cannot POST
again. The existing bounded, quota-controlled refund query verifies the original
capture and completes ledger, legacy request, tickets and inventory in one
transaction. The shared provider transport avoids a second payment integration.
The canonical UUID is also sent as the PayPal refund `custom_id`; only a UUID is
retained in the minimized signed event. A correctly bound signed callback can
complete the specific allocation even before the HTTP response commits its refund
ID. If neither an exact known provider ID nor the opaque canonical correlation
matches, the existing whole-order review fence remains. This missing-correlation
case needs operational reconciliation; it never infers ticket selection from an
amount. [PayPal's refund API](https://developer.paypal.com/api/payments/v2/captures-refund)
defines `custom_id` for reconciliation and `PayPal-Request-Id` for idempotency.

Official sandbox HTTP qualification at source `a058dc0bb80dd04e3341fdbcd0df066b6732701c`
passed two mobile-web purchases, organizer partial/full refunds, authenticated
provider GET confirmation, signed callback replays, QR decoding and retained-ticket
concurrent check-in. The [scoped receipt](../../docs/events/patch-culture-vol-1/ticket-refund-api-sandbox-2026-10-06.json)
distinguishes native qualification from later display/finance corrections and
external test-fund cleanup. Local SMTP acceptance does not prove external delivery.
Protected integration and deployment gates remain before production activation. Provider requests use
the original canonical refund UUID as the stable
[PayPal idempotency key](https://developer.paypal.com/api/rest/reference/idempotency/)
and preserve uncertain results; automatic re-POST with a new identity is forbidden.
