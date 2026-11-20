# `/marketplace` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server.hs` (marketplace sections), `Server/ServiceStorefront.hs`, `Commerce/MarketplaceOperations.hs` and the marketplace runtime SQL on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Mounting and guards**
- Public (`MarketplaceAPI`, no AuthProtect): cart, cart items, checkout, the three rails, PayPal capture, manual-payment evidence and customer requests. All are registered in `formal/system/public-routes.json` (cart: public-intake; cart/{cartId}/*: capability-path; orders/* and paypal/capture: capability-header).
- Admin (`MarketplaceAdminAPI`, AuthProtect): `requireMarketplaceAccess` = ModuleOps or ModuleAdmin; deposit settlements also need ModuleInvoicing; manual-payment review needs ModuleInvoicing only.
- `Idempotency-Key` is validated by `ServiceStorefront.validateIdempotencyKey` (16..128 visible ASCII). `X-Order-Lookup-Token` is compared in constant time against `marketplace_order_checkout_runtime.lookup_token_hash`; a missing runtime row or mismatch answers 404.
- `marketplace_order.status` becomes `paid` only through `trg_marketplace_sync_verified_checkout`; the same trigger mirrors `processing` and `failed` onto the order.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| cart, cart items, checkout | MKT-CART-ROUTES-001 | new |
| stripe payment-intent (503), datafast checkout, paypal create, paypal capture | MKT-PAYMENT-RAILS-001 | new (critical) |
| manual-payment evidence, customer requests | MKT-BUYER-REQUESTS-001 | new |
| order update, fulfillment, rental, rental-terms | MKT-OPS-ORDERS-001 | new |
| manual-payment review, customer-request review, deposit settlements and their review | MKT-OPS-REVIEWS-001 | new (critical) |

`PAY-CHECKOUT-001/002` (cart arithmetic and checkout lines) and `PAY-CAPTURE-RECON-001` (capture admission) cover parts of these routes; none specified them directly. `/service-marketplace` bookings and escrow release answer 503 as SYS-ESCROW-001/002 require; `POST /service-marketplace/bookings/{id}/complete` still writes a completed booking and is not covered by either.

## Defect fixed on 2026-11-07

**Every `PUT /marketplace/orders/{orderId}` on a canonical order answered 500.** Canonical checkout creates the order with status `awaiting_payment`, and the sync trigger later writes `processing`; neither is in `normalizeMarketplaceOrderStatus`, so `validateStoredMarketplaceOrderStatus` rejected the stored status with "Stored marketplace order status is invalid" before any operator edit. `validateStoredMarketplaceOrderStatus` now accepts the canonical statuses (`holding`, `awaiting_payment`, `processing`) as current state; they remain refused as update targets. Tested in `tdf-hq/test/TDF/ServerSpec.hs`.

## Open behaviours for the owner to decide

**Payments**
- If `bindProviderResource` fails after the remote Datafast or PayPal resource was created, its id is not persisted and a retry creates a second provider resource.
- Manual-payment review is gated by the invoicing module alone, weaker than deposit settlements (ops access plus invoicing).
- `captureLegacyPaypalOrder` is unreachable (the token check requires a runtime row).

**Orders**
- Operators may set a canonical order to `cancelled` or `failed` without releasing its reservation hold or checkout; only `paid` is fenced.
- Rental terms are approved by the operator who writes them.
- Anonymous cart creation is unbounded; cart-item availability checks are unlocked until checkout.
- Whether a verified deposit settlement propagates `deposit_status` to `refunded` is enforced only in SQL.
