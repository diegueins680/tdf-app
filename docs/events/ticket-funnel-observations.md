# Ticket funnel browser observations

The existing PostHog client now observes public event-card impressions, event
detail views, valid ticket selection, validated checkout submission, provider
session creation, server-confirmed payment, issued-ticket display and successful
staff check-in. These are reusable event routes; no PATCH CULTURE identifier or
commercial configuration is embedded in the instrumentation.

`ticketing_` prefixes the nine requested phase names: `event_impression`,
`event_view`, `ticket_selected`, `checkout_started`, `payment_initiated`,
`payment_completed`, `ticket_issued`, `ticket_opened`, and `check_in`.

Impressions require IntersectionObserver to report at least half the card visible;
unsupported browsers omit that observation. Selection requires available checkout,
a real tier, and a quantity within stock and policy limits. Opening an existing
order never counts as selection. Checkout-start records a validated submission,
which can still fail server admission. Payment initiation follows the backend
provider response, not merely a button click. Prepared, failed and confirmed-no-charge
responses still represent an initiated attempt and deduplicate per order/provider. A browser return or approval callback
is not payment completion. Completion requires the fetched checkout to report
`paid`; ticket phases additionally require `issued` fulfillment and an issued
ticket. Revoked or unissued ticket displays do not count as ticket opens. Check-in
observations require the successful server response to identify a checked-in ticket
with a timestamp. No observation changes payment, inventory or authorization.

Payloads contain only the public event/tier identifiers, bounded quantity, known
provider, promotion-presence boolean, web platform and optionally bounded source,
medium and campaign codes from the existing attribution store. Direct event/checkout
landing parameters are captured before observation; a new campaign replaces stale
attribution, including a revisit whose observation is deduplicated. Only the
canonical public event path is retained, never a private
order path or lookup query. Disabled analytics does not write attribution. They explicitly
exclude buyer/holder details, order/ticket IDs, QR codes, credentials, raw referral
or promotion values, landing URLs, and financial amounts. Campaign labels must
remain non-personal codes; this syntax filter cannot detect a person's name hidden
inside an otherwise valid label. Existing SDK sanitization and opt-out behavior
still apply. No new analytics service, project or consent policy is introduced.

Session storage holds a bounded 200-observation deduplication cache. Order/ticket
scope is local only. Multiple mounted components merge the current cache; refresh
preserves observations within that session. SDK/storage failures do not interrupt
purchase or admission. This is best-effort browser telemetry, not a durable outbox
or an exactly-once cross-tab ledger. Without configured analytics, capture and
storage operations are skipped. Observations can be missing with blocked scripts,
opt-out, storage exhaustion or a closed browser. Cache eviction/new sessions can
repeat historical observations.

Every payload marks `evidence: browser_observation`. In particular, `ticket_issued`
means the browser observed issued state, not the instant the backend committed
issuance. `ticket_opened` is per-order display, not one financial sale per ticket.
Canonical order/payment/refund ledgers remain the source for GMV, fees, net revenue
and refunds; canonical admission records remain the source for attendance. Browser
counts cannot replace them or establish campaign revenue attribution. Reliable
server-side attribution joins and vendor-ingestion verification remain pending.

Regression tests cover viewport visibility, refresh/component deduplication,
disabled analytics, storage/SDK failures, payload exclusion, quantity rejection,
pending/failed payment returns, successful provider initiation and server-paid
versus actually issued ticket states. These component tests use synthetic API
responses; they are not official provider or production-ingestion evidence. The
separate official payment qualification retains its own exact-source receipts.

Primary references: [PostHog event ingestion and deduplication](https://github.com/PostHog/posthog.com/blob/master/contents/docs/data/events.mdx),
[PostHog privacy and opt-out controls](https://github.com/PostHog/posthog.com/blob/master/contents/docs/product-analytics/privacy.mdx),
and [PostHog API](https://posthog.com/docs/api). A successful `capture` call alone
does not establish delivery, and adding a browser `$insert_id` does not turn the
financial flow into an exactly-once provider ledger.
