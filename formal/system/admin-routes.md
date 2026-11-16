# `/admin` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the handlers on main `6033b84e`:
- `tdf-hq/src/TDF/ServerAdmin.hs`
- the course handlers in `tdf-hq/src/TDF/Server.hs`
- `tdf-hq/src/TDF/Server/ServiceStorefront.hs`
- `tdf-hq/src/TDF/Server/CommerceOperations.hs`
- `tdf-hq/src/TDF/Server/RecordsIngestion.hs`

This describes what the code does today. It is **not** an owner specification: new requirements written from it have status `implemented but unspecified`.

Every route is authenticated: `AuthProtect "bearer-token"` sits in front of `ProtectedAPI`. There are two kinds of authorization:
- **Strict administrator:** `ensureStrictAdmin` / `hasStrictAdminAccess`. The caller has the Admin role, every role is strict-admin scoped, and the role grants are coherent.
- **Admin module:** `ensureModule ModuleAdmin` or `validateModuleAccess ModuleAdmin`. This also admits StudioManager and Webmaster.

AUTH-ADMIN-GUARD-001 and `scripts/check-admin-guards.py` check only `ServerAdmin.adminServer` and `recordsIngestionServer`. The course, storefront and commerce-operations `/admin` handlers fall outside that CI sweep.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| `PATCH users/{userId}` | ID-SESSION-001 | reviewed |
| `POST rag/refresh` | PRIV-RAG-002 | reviewed |
| enrichment runs, run update, artist rerun | OPS-ENRICH-001 | reviewed |
| enrichment sources, suggestion and suggestion-set decisions, identity decisions | ART-ENRICH-REVIEW-001 | reviewed |
| enrichment media, suggestions | ART-PROFILE-MEDIA-001 | reviewed |
| `POST domo/quotes/{quoteId}/cancel`, storefront refund approve | PAY-DOMO-CANCEL-001 | named |
| `POST social/accounts/{accountId}/delivery` | AUTH-SOCIAL-OAUTH-001 | named |
| `POST users` | ID-ADMIN-USER-CREATE-001 | new |
| user WhatsApp send and resend | MSG-ADMIN-WHATSAPP-001 | new |
| registered-user email broadcast, email test | MSG-ADMIN-EMAIL-001 | new |
| `DELETE logs`, `POST seed` | OPS-ADMIN-MAINTENANCE-001 | new |
| dropdowns | DATA-DROPDOWN-001 | new |
| brain entries | OPS-BRAIN-001 | new |
| records-ingestion control, sources, runs | REC-INGESTION-ADMIN-001 | new |
| `POST social/unhold` | MSG-SOCIAL-UNHOLD-001 | new |
| artist profiles, payout onboarding link, releases, promotions | ART-ADMIN-CATALOG-001 | new |
| enrichment identity-candidates `POST` | ART-ENRICH-IDENTITY-001 | new |
| courses and registration status, notes, receipts, follow-ups | COURSE-ADMIN-001 | new (critical) |
| storefront orders, reconcile, refunds, packages, refund reconcile | PAY-STOREFRONT-ADMIN-001 | new (critical) |
| commerce provider-event replay | PAY-PROVIDER-EVENT-REPLAY-001 | new (critical) |

## Defects fixed on 2026-11-07

1. **Storefront order update could revert a payment.**
   - Problem: `PUT /admin/services/storefront/orders/{orderId}` read the order and then replaced the whole row in a separate transaction. A capture, webhook or refund written in between could have its status, paid time and capture or checkout ids reverted.
   - Fix: one transaction writes only status, deliverables URL and notes. The write is conditional on the status that was checked (409 otherwise), and the status is stored normalized.
   - Test: PostgreSQL test in `ProviderRetrySpec`.
2. **Unhold could make the bot answer a message twice.**
   - Problem: `POST /admin/social/unhold` by external id also reset messages already answered (`sent`) to pending, so the auto-reply worker could answer them again.
   - Fix: answered messages are excluded.
   - Test: `ServerAdminSpec`.
3. **Route checker missed routes named before a colon.**
   - Problem: `scripts/check-route-requirements.py` ignored a route written as "POST /a/{id}/cancel:" because of the trailing colon.
   - Fix: trailing prose punctuation is stripped.
   - Effect: the Domo admin and public cancellation routes are now attributed to PAY-DOMO-CANCEL-001.

## Open behaviours for the owner to decide

**Messaging consent**
- No consent, opt-out or 24-hour-window check applies to administrator WhatsApp sends and resends.
- The registered-user email broadcast checks no marketing consent and can include deactivated users.
- The test email lets StudioManager and Webmaster send arbitrary branded email with any link.

**Course registrations**
- A paid course registration can be cancelled with no refund.
- A legacy registration can be marked paid on the strength of a receipt URL alone.
- Receipts and system follow-ups can be edited or deleted without audit.

**Payments and payouts**
- Storefront order reconciliation exceptions do not link to their checkout in the reconciliation report.
- Concurrent Stripe Connect onboarding can create orphan accounts.

**Admin-module powers**
- StudioManager and Webmaster can rename any party, give it an artist profile and create a payout account.
- Clearing logs is open to non-strict Admin-module roles.

**Artist enrichment**
- Approving an identity candidate can grant the Artist role to any party the administrator names.
- Suggestion and identity decisions are not conditional writes, so a concurrent decision can overwrite a terminal one.
- Enrichment validation errors answer 500.

**Data integrity**
- `user_credential` has no unique key on `party_id`.
- Welcome emails carry the initial password in clear text.
