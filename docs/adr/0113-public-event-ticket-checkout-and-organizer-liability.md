# ADR 0113: Public event ticket checkout and organizer liability

Status: accepted

Date: 2026-08-18

## Context

ADR 0112 removed payment fabrication from the authenticated ticketing surface and defined the
payment-evidence boundary. A public buyer journey still needed atomic capacity, authoritative fees,
Datafast/PayPal, guest tracking, ticket issuance, and organizer accounting without replacing the
existing event order, promotion, transfer, refund, waitlist, QR, and check-in domain.

The existing four-percent fee concept has two economically different parts: the amount added to the
buyer and the amount deducted from organizer proceeds. Treating the full capture as TDF revenue
would overstate revenue and erase the organizer obligation.

## Decision

- Keep `event_ticket_order` as the domain order and link it one-to-one to the canonical checkout.
  The runtime snapshots the approved policy, tier, quantity, promotion, integer-minor-unit price,
  buyer and organizer fee allocations, tax, currency, terms, and hold expiry.
- Public checkout is available only for a publicly eligible event with an active positive-price
  tier, an approved active event policy, and the environment-scoped `commerce.event_tickets` flag.
  Migrated fee policies are inactive drafts; migration never approves a commercial rate.
- Serialize creation on the event, then lock the tier and promotion. Expire prior holds before
  capacity checks and reserve event capacity, tier inventory, and the promotion claim in the same
  transaction. Expiry releases each exactly once.
- Protect status and provider actions with a server-keyed pseudorandom lookup capability whose
  SHA-256 digest is stored. The key comes only from the secret manager, and stable derivation permits
  safe idempotent response recovery without retaining the plaintext token. URLs never contain the
  token. Apply an atomic event-scoped rate limit keyed by an HMAC of the normalized buyer email and
  return enumeration-resistant customer-safe errors.
- Use shared Datafast and PayPal attempts and immutable provider bindings. Provider status, amount,
  currency, environment, merchant, internal order, event, and resource must all match. A late
  verified payment after hold expiry opens reconciliation and does not issue inventory.
- Serialize issuance on the runtime row. Only a paid canonical checkout and paid ticket runtime may
  create ticket rows. A partial unique index permits only one `issued` fulfillment audit per order,
  so callback retries cannot issue or notify twice.
- Freeze every runtime price, fee, policy, terms, identity, and expiry field after creation. Later
  catalog edits cannot change or strand the accepted checkout; only the independent payment and
  fulfillment states may advance, and direct unpaid issuance is rejected in PostgreSQL.
- Post the captured total as cash, TDF's buyer-plus-organizer fees as platform revenue, tax as tax
  liability, and organizer proceeds as `liability.event_organizer_payable`. The accounting entry is
  a payable, not evidence of settlement. `commerce.event_ticket_settlements` stays disabled until a
  dual-controlled, evidence-backed settlement workflow exists.
- The bilingual public routes are `/eventos/:eventId/entradas` and the capability-protected
  `/eventos/:eventId/orden/:orderId`. A browser return remains `processing` until the server verifies
  payment, and ticket codes are absent until separate fulfillment reaches `issued`.

## Alternatives considered

### Create an independent generic ticket order

Rejected. It would duplicate tier inventory, promotions, transfers, refund links, QR issuance, and
check-in authority. The canonical checkout is shared; fulfillment remains domain-owned.

### Count `quantity_sold` only after capture

Rejected. Concurrent buyers could oversell. Expiring holds reserve capacity without claiming
payment and release it safely when abandoned.

### Recognize the full capture as ticket revenue

Rejected. For third-party events, organizer proceeds are a liability. Settlement requires separate
operator evidence and approval.

### Trust a provider redirect or approval callback

Rejected. Browser-controlled returns are navigation only. The server must verify the stored
provider resource and immutable commercial binding.

## Consequences

- One TDF-owned staging pilot can be enabled per event only after policy approval and provider
  sandbox verification. Production rows and organizer settlement remain disabled.
- Free/complimentary tickets remain outside this paid guest flow until an explicit no-payment
  entitlement contract exists.
- Existing event orders and provider identifiers remain unchanged; no historical payment is
  inferred or backfilled.
- The buyer email is normalized and bound to the immutable order, but mailbox ownership is not yet
  verified and no verified-email recovery flow exists. The production domain flag must remain off
  until that customer-identity and recovery control is implemented and exercised.
- Mobile generated API types are synchronized, but the existing native buyer UI remains the legacy
  authenticated flow until it adopts the same guest token and verified-payment contract.
