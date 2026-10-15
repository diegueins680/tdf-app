# Music purchases, entitlements and downloads — PAY-MUSIC-DOWNLOAD-001

A signed-in buyer orders a download offered by a published release's availability
rule. The order opens a commerce checkout in the `music_download` domain. When the
checkout becomes paid, database triggers settle the order and grant one entitlement
to the rule's downloadable asset; the buyer then requests short-lived signed URLs
against that entitlement. The feature is behind the `music_releases.commerce` and
`music_releases.public` flags, which the migrations seed as disabled in every
environment. The data model and product decisions are in
[architecture-and-data-model.md](../../docs/music-releases/architecture-and-data-model.md).

## Rules

- **Order.** One order per buyer and `Idempotency-Key`; price, currency, release
  version and rule are copied from the rule when the order is created.
- **Paid.** A `music_download` checkout becomes paid only when a succeeded payment
  attempt carries a provider binding whose merchant, amount, currency and merchant
  reference equal the order's (`music_checkout_require_verified_payment`). The
  handlers record that only after the provider confirmed the payment server to
  server.
- **Entitlement.** The paid transition settles the order and inserts one entitlement
  with a limit of ten downloads (`music_sync_verified_checkout`). The trigger acts
  only on a status change and the insert is an upsert on the order, so recording the
  same payment again grants nothing more. Refund, dispute and chargeback end the
  entitlement.
- **Download.** A download is authorized for the entitlement's buyer while the
  entitlement is active, unexpired, its release is public and its asset satisfies
  the shared downloadable-asset predicate. The entitlement row is locked for the
  whole authorization. A new request identifier is counted against the limit. A
  repeated identifier is answered again without counting only while the URL it
  first produced is still valid (300 seconds); afterwards it is refused with
  `409` and the client must send a new identifier. An identifier belongs to one
  entitlement; a second entitlement presenting it gets `409`.
- **Flag.** Purchases are open only when `music_releases.commerce` is enabled for
  the process environment and for the environment its checkouts are created in.
- **Free grant.** A free rule grants an entitlement with a limit of five downloads
  on first request. Asking again returns the existing grant unchanged.

## Defects repaired (AUTHORITY-058)

1. **Every verified payment failed after the provider had charged the buyer.** The
   payment record was built without its merchant reference
   (`vpProviderReference`), a field added to the shared checkout store after this
   module was written. The compiler accepts that with a warning and the program
   stops when the field is read, so both the PayPal capture and the Datafast
   confirmation ended in an error after a completed charge, with no order settled
   and no entitlement. No test reached this code: the end-to-end script writes the
   payment rows with SQL.
2. **The download limit did not limit.** A repeated request identifier was answered
   with a fresh signed URL forever and never counted, so one counted download gave
   unlimited downloads.
3. **A revoked free grant came back on request.** Requesting a free download again
   set a revoked or expired grant back to active.

## Evidence

- `formal/event-operations/MusicEntitlement.tla`: one order, its checkout, the
  entitlement and three request identifiers over bounded time (limit 2, lifetime 2,
  five instants): 1,160 distinct states, six invariants (`PaidNeedsEvidence`,
  `OneEntitlement`, `LimitHolds`, `UrlsInsideLifetime`, `UrlsEndWithAuthorization`,
  `ActiveWhenIssued`). Six controls each violate one invariant: paid without
  evidence, a grant on every record, no limit, replay without a window, a replay
  signed from the current instant, download without an active entitlement. The grant control removes two guards together (status-change-only
  trigger and upsert); either alone still holds the invariant.
- `tdf-hq/test/TDF/MusicPurchaseSpec.hs` (`scripts/test-music-purchase.sh`,
  PostgreSQL, fully migrated schema, eleven cases): the real purchase handler, the
  real payment record, the real triggers and the real download authorization.
  Reverting the payment repair makes every case that needs a paid order stop with
  `Missing field in record construction vpProviderReference`; reverting the replay
  repair alone fails the replay case; reverting the free-grant repair fails the
  free-grant case.

## Guard against recurrence

The backend executable is now compiled with `-Werror=missing-fields`, so a record
built without one of its fields no longer compiles. All 246 backend modules build
under it; no other module had the fault.

## Open before the feature is enabled

Order creation replay is additionally governed by
[music-purchase-replay.md](music-purchase-replay.md): the same buyer/key must match
the original offer, normalized terms/countries and checkout environment. A
different payload is a conflict even when another transaction reserved the key.

Independent review of the payment flow found problems this change does not repair.
None is introduced here; together they mean `music_releases.commerce` should stay
off until they are closed.

- **Music payments bypass the shared payment runtime.** Attempts are opened
  directly on the checkout store, so they get no payment intent, no route
  validation and no provider-activation check, and the production `checkout.paypal`
  and `checkout.datafast` switches do not apply. The runtime has no `music_download`
  flow, so moving music onto it is new work. This change adds one guard only: the
  commerce flag must be enabled for the checkout environment as well as for the
  process environment.
- **A recorded provider failure cancels the order for good.** Any failed attempt sets
  the checkout to `failed`, and the trigger then sets the order to `cancelled`,
  which no path leaves. This happens on a transient error creating the provider
  order (no money moved), on a Datafast status query that returns anything other
  than success or `000.200.000`, and on a PayPal capture whose result is lost. In
  the last two the provider may hold or later take the money.
- **An entitlement can be active on a cancelled order.** The checkout store accepts
  a verified payment on a `failed` checkout, and the trigger grants the entitlement
  even when the order is already `cancelled`. Two overlapping confirmations, one
  failing and one succeeding, leave checkout paid, entitlement active and order
  cancelled.
- **A failure after the provider has charged leaves no durable record.** A capture
  that completed but then fails validation or binding returns an error and writes
  nothing for staff to reconcile; PayPal webhooks exclude this domain and there is
  no reconciliation job.
- **The client's `Idempotency-Key` is the global attempt key.** Two buyers, or two
  orders, sending the same key for the same operation collide; the second is
  refused before any charge.
- **Provider orders are replayed from the stored binding forever** and the
  checkout's 30-minute expiry is not enforced, so an order whose provider checkout
  expired cannot be paid.
- **The purchase offer does not apply the downloadable-asset predicate**; a
  purchasable rule pointing at an asset that is not ready would take payment and
  then refuse the download. Publication validation may prevent it; not verified.
- **Partial refunds keep the entitlement; a dispute revokes it and nothing restores
  it.** Both need a product decision.
- **No provider was contacted.** PayPal and Datafast calls are untested for this
  domain; the tests start from the point where the provider has answered.

## Limits of this evidence

- The model has one order and no failed or cancelled state. Its statement that an
  active entitlement implies a paid order holds only without failed attempts (see
  above).
- A replay inside the window signs from the original authorization time, so its URL
  and reported `expiresAt` end with the first one: one counted download gives access
  for 300 seconds. Signed URLs are bearer links; the limit counts authorizations, not
  bytes transferred.
- The repeated-payment case is absorbed by the checkout store before the trigger
  runs; the trigger's own upsert is not exercised.
- The fixture inserts a published release with triggers bypassed. Publication,
  validation and the downloadable-asset predicate are covered by their own tests.
- The test group leaves its parties, orders, checkouts and download events in the
  shared test database (they are append-only or deletion-restricted) and withdraws
  its releases.
- Entitlements granted by staff have no order; download authorization checks the
  entitlement, not the order.
