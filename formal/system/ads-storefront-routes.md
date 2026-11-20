# `/ads`, `/service-marketplace` and `/services/storefront` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server.hs` (ads and service-marketplace sections), `CampaignAutomation.hs`, `Server/ServiceStorefront.hs`, `Commerce/CheckoutStore.hs` and `Commerce/ProviderEventStore.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- `/ads` admin routes: `requireModule ModuleAdmin` (403). `POST /ads/inquiry` is public-intake under an `Idempotency-Key`.
- `/service-marketplace`: bearer session only; bookings and escrow release answer 503 (SYS-ESCROW-001/002).
- `/services/storefront`: order creation is public-intake under an `Idempotency-Key`; the order routes are capability-header under `X-Order-Lookup-Token`; the PayPal webhook is a signed provider callback verified with PayPal's `verify-webhook-signature` on the raw body.
- A `ServerError` thrown with `throwIO` outside `runDB` is **not** caught by servant-server 0.20.3 (`runHandler` does not `try`); the failure boundary turns it into a generic 500. Checked empirically on 2026-11-07.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| campaigns, ads, examples | ADS-ADMIN-CATALOG-001 | new |
| automations install, enroll, status, enrollment status | ADS-AUTOMATION-001 | new (critical) |
| inquiry | ADS-PUBLIC-INQUIRY-001 | new |
| service-marketplace ads, slots | SVCMKT-PROVIDER-ADS-001 | new |
| service-marketplace bookings, escrow release | SYS-ESCROW-001, SYS-ESCROW-002 | reviewed |
| service-marketplace bookings complete | SVCMKT-BOOKING-COMPLETE-001 | new |
| storefront order, revision, stripe (503) | STORE-PUBLIC-ORDER-001 | new |
| storefront datafast, paypal create, manual-payment, paypal capture | STORE-PAYMENT-RAILS-001 | new (critical) |
| storefront paypal webhook | STORE-PAYPAL-WEBHOOK-001 | new (critical) |

`PAY-CAPTURE-RECON-001` and `PAY-PROVIDER-INBOX-001` already cover the capture and webhook admission paths and hold.

## Defect fixed on 2026-11-07

**Every refusal of the service-marketplace ad, slot and completion routes answered 500.** `createServiceAd`, `createServiceAdSlot` and `completeServiceMarketplaceBooking` raised their 400/403/404/409 refusals with `throwIO` inside a raw `runSqlPool`, which servant does not catch. They now run under `runDB`, whose `tryServerError` returns the thrown status. (The same class of bug was fixed in the public booking transactions; see `bookings-routes.md`.)

## Open behaviours for the owner to decide

**Authorization**
- **Decided 2026-11-10.** Service-marketplace ads and slots need a staff-assigned provider role (`validateServiceAdProvider`); Customer, Fan and Student accounts are refused. Still open: there is no provider approval record, earlier ads remain listed, and booking completion accepts the bare Admin role.

**Concurrency and status codes**
- Automation install and enrollment, provider ads and slots, and storefront revisions race into unique constraints and answer 500 instead of 409 or a per-party rejection.
- Unknown automations and enrollments answer 400 rather than 404.
- Two concurrent first payment creations on a storefront order can both create a provider checkout.

**Intake**
- Every ad inquiry creates a new party even when the contact already exists.
- A replay of storefront order creation does not return the lookup token; the disabled Stripe route never checks it.
