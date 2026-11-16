# Commerce, legacy money and integration mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/Server/ProviderExecution.hs`, `Commerce/ProviderReconciliation.hs`, `Commerce/ProviderEventStore.hs`, `ContentStorage.hs`, `Calendar/Models.hs` and the payments, invoices, receipts, packages, academy, calendar, drive, Tidal and MCP sections of `Server.hs` and `ServerExtra.hs` on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Guards**
- Hosted payment sessions are capability-header (`X-Checkout-Lookup-Token`); the PlaceToPay notification is signature-verified; PayPhone callbacks (both paths) are classified untrusted and must match an immutable binding before the inbox row is stored, and the worker queries the provider before any money moves.
- Legacy money routes use module gates only (Admin, Invoicing, Packages); calendar routes need a strict administrator; storage routes are owner-scoped; `/mcp` is public-diagnostic.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| commerce payment-sessions | PAY-HOSTED-SESSION-001 | new (critical) |
| provider-notifications placetopay, payphone, NotificacionPago | PAY-HOSTED-NOTIFICATIONS-001 | new (critical) |
| payments, invoices, receipts, packages/purchases | PAY-LEGACY-RECORDS-001 | new (critical) |
| academy enroll, progress | ACADEMY-ACCOUNT-001 | new |
| calendar auth-url, tokens, sync | INTEG-GOOGLE-CALENDAR-001 | new (critical) |
| drive token, refresh, upload; storage delete, destination | MEDIA-STORAGE-WRITES-001 | new |
| tidal-agent, ads/assist | AI-STAFF-ASSISTANTS-001 | new |
| mcp | AUTH-PUBLIC-001 | reviewed |

PAY-RETRY-001, PAY-ADAPTER-BINDING-001, PAY-PROVIDER-INBOX-001, PAY-INVOICE-001, PKG-PURCHASE-EXPIRY-001 and MEDIA-DRIVE-001 hold on these routes.

## Open behaviours for the owner to decide

**Money (critical)**
- **Decided 2026-11-10.** A bound hosted-provider event that ends as a dead letter without an exception of its own now leaves one open `provider_event_dead_letter` reconciliation exception, written in the dead-letter transaction behind the attempt fence. Still open: the worker admits money with `recordVerifiedPayment`, so a refused admission is recorded at the dead letter (up to eight attempts later), not in the refusing transaction; unbound events record nothing, by design.
- Legacy payments, invoices and package purchases record money or entitlement with no canonical checkout, attempt or ledger linkage and no request idempotency.

**Secrets**
- **Decided 2026-11-10.** Google Calendar tokens are sealed with AES-256-GCM under `GOOGLE_CALENDAR_TOKEN_KEY`; nothing is written without the key. Still open: rows written earlier keep plaintext until their next refresh (no backfill), and key rotation is not supported.

**Defaults and status codes**
- Legacy Drive uploads with no explicit visibility and no destination default to anyone-with-link sharing.
- Academy enrollment and the legacy money routes answer 500 on unique or foreign-key races.
- MEDIA-STORAGE-001 is still registered as specified but missing although `ContentStorage.hs` implements it.
- `/NotificacionPago` is an undocumented alias of the PayPhone handler.
