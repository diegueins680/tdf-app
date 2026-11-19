# `/merch` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the handlers on main `f615c862`: `tdf-hq/src/TDF/Server/Merch.hs`, `tdf-hq/src/TDF/Commerce/Merch.hs`, `tdf-hq/src/TDF/Commerce/RefundStore.hs` and `tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql`.

This describes what the code does today. New requirements written from it have status `implemented but unspecified`.

**Authentication and guards**
- Carts, checkout, order issues and order cancellation are **public** (`MerchPublicAPI`) and are protected by a capability: the `X-Cart-Lookup-Token` or `X-Order-Lookup-Token` must hash-match the stored token.
- Favorites, seller and admin routes are behind `AuthProtect "bearer-token"`.
- Seller routes use `requireStorePermission`: an accepted `merch_store_member` holding the named `can_*` permission. There is no role check and no expiry check beyond the invitation status.
- Admin routes use `requireAdmin` = `hasStrictAdminAccess`.
- Public, cart, checkout, refund and favorite routes check feature flags (503 when off); seller routes do not.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| carts create, put item, delete item | MKT-MERCH-CART-001 | new |
| carts checkout | PAY-MERCH-CHECKOUT-001 | new (critical) |
| favorites, order issues, order cancel | MKT-MERCH-BUYER-001 | new |
| seller applications, store update, members, policies, shipping zones | MKT-MERCH-SELLER-STORE-001 | new |
| products create/update/images/status, variant stock | MKT-MERCH-SELLER-CATALOG-001 | new |
| fulfillment, seller issues | MKT-MERCH-SELLER-ORDERS-001 | new |
| admin store review, product review, admin issues | MKT-MERCH-ADMIN-REVIEW-001 | new |
| admin refunds create, refund status | PAY-MERCH-REFUND-001 | new (critical) |
| settlements create, status, payment evidence | PAY-MERCH-SETTLEMENT-001 | new (critical) |

No existing requirement specified any of these routes as an HTTP contract. `PAY-CHECKOUT-003` specifies the money arithmetic inside checkout and is referenced by `PAY-MERCH-CHECKOUT-001`.

## Open behaviours for the owner to decide

**Money**
- Refund approval moves no money: no execution path for merch refunds exists in this module, so approved refunds stay approved until something else executes them.
- A refund can be requested on an order already settled to the seller; clawback is manual.
- Orders whose refund failed (and needs reconciliation) are eligible for settlement.
- The settlement period compares `created_at` with date-only bounds at UTC midnight.

**Stock and catalogue integrity**
- A product update upserts variants with the request's `stock_on_hand`, bypassing the version-guarded stock route and its reserved-plus-sold floor.
- Variants omitted from a product update stay active; a client-supplied variant id owned by another store silently updates nothing.
- Product status changes and fulfillment updates read then write without a status guard, so a stale transition can overwrite an administrator's review or a concurrent change.
- Product images are stored with `scan_status = 'clean'` without a scanner.

**Permissions**
- A `can_settings` holder can grant `can_settings` and `can_finance` to anyone and revoke any other manager; there is no owner-only escalation guard.
- Self-acceptance of an invitation ignores its expiry, and a settings holder can set a pending member to accepted without the invitee.
- Store membership checks ignore `expires_at` and `revoked_at` beyond the invitation status.
- A single administrator approves a store (no two-person rule); the settlement two-person rule only excludes the preparer, so the approver may also record payment.

**Idempotency and status codes**
- Checkout idempotency is scoped to (store, key), not (cart, key), so two buyers reusing a key at one store collide.
- The buyer limit counts prior orders whose payment is still pending, including abandoned holds.
- Policy versions, product creation, settlements and cancellation surface unique-key races as 500 instead of 409.
- The settlement external payment reference is globally unique, so a bank reference legitimately reused is refused.

**Notifications**
- No invitee, buyer or fan notification is sent by any of these handlers.
