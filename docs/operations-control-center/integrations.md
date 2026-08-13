# Integration registry and activation

No provider is enabled by the migration. `operations_provider_config` seeds an explicit EC/USD sandbox row for every adapter with `enabled=false` and `activationState=credentials_required`. Core transaction capture, the inbox, assignment, SLA, audit, and manual work continue if every provider is disabled.

| Provider/channel | Inbound boundary | Outbound boundary | Release state |
| --- | --- | --- | --- |
| Public web / authenticated web / mobile | Existing validated REST commands; business-table triggers | Existing app API | Core-ready; enable organization feature after migration |
| WhatsApp Cloud API | Existing Meta HMAC-SHA256 raw-body verification, provider external ID, persisted message, opt-out handling | Existing official Cloud API transport/history | Adapter-ready; disabled until Meta app/number/templates are verified |
| Instagram / Facebook messaging | Existing Meta signed webhook and persisted external ID; uncertain identities become uncorrelated work | Existing official Graph API service where configured | Adapter-ready; disabled until app permissions/review are verified |
| Stripe | Existing five-minute signed-webhook replay window and idempotency keys | Existing PaymentIntent/refund clients | Adapter-ready only where the merchant account/jurisdiction permits |
| PayPal / Datafast | Existing checkout/capture business paths; their committed order/payment changes create work | Existing provider clients | Sandbox verification required before enabling operations delivery |
| Bank transfer | Receipt upload persists against registration/payment context | Staff verification and existing payment command | Core-ready; verification remains role-gated |
| Email | Existing application mail service and persisted business actions | Existing mail service | Operations-template dispatcher is disabled until templates/from-domain are approved |
| Google Calendar | Existing OAuth/token validation and booking synchronization | Existing Calendar API | Disabled until OAuth consent/client credentials are installed |
| Mobile push | Authenticated device registration; opaque authorized work-item deep link | Encrypted OS device-token registry | Code-ready; APNs/FCM/EAS credentials and `tdf.push_encryption_key` required |
| SMS | Provider registry, delivery attempts, consent/rate-limit/dead-letter model | No merchant selected | Disabled; selecting and sandbox-verifying an official provider is required |
| PayPhone | Provider registry and payment event model | No approved merchant credentials in repository | Disabled; sandbox contract verification required |
| Cryptocurrency | Provider registry; no key material accepted | Non-custodial/approved provider only | Disabled; provider/compliance decision required |
| Ecuador SRI | Existing invoice command boundary and strict result validation | See SRI section | Disabled for production activation pending direct offline conformance |

## Common adapter contract

An inbound adapter must perform, in order:

1. Read the exact raw request body with a strict size limit.
2. Validate official signature/authenticity and a bounded replay timestamp before JSON/XML parsing.
3. Insert `operations_inbound_receipt` with provider event ID, payload digest, verification result, and redacted metadata. The unique provider key returns the previous result on replay.
4. Correlate to a party and source record only on deterministic evidence. Otherwise create an `uncorrelated_inbound` thread and require a staff correlation action.
5. Commit the source business command and durable event in one transaction. Provider retries cannot duplicate payment or work effects.
6. Redact tokens, headers, raw bodies, contact values, tax identity, and payment data from logs and work metadata.

An outbound adapter claims a persisted `operations_outbound_delivery`, verifies organization/provider/country/currency enablement, consent and opt-out, approved template, and rate limit. A completed internal transaction is never rolled back by delivery. Retry uses exponential backoff and jitter; terminal attempts create `operations_integration_failure`. Admin replay requires Manager/Admin permission, a reason, a request ID, and immutable audit.

## Provider secret policy

`operations_provider_config.configuration` rejects common secret keys. Store secrets in the deployment secret manager and inject only named references. Never persist PAN, CVV, private keys, seed phrases, certificate passwords, OAuth refresh tokens, webhook secrets, or production payloads in operations tables. Logs include correlation IDs and provider/error codes only.

## Ecuador SRI activation boundary

Authoritative sources are the official [SRI electronic invoicing portal](https://www.sri.gob.ec/facturacion-electronica) and its current offline technical specification. As of this implementation review, the official page publishes technical sheet version 2.34 (July 2026), covering XML/XSD documents, XAdES-BES signing, reception/authorization web services, test/production environments, and credit/debit notes.

The repository's pre-existing `scripts/lib/sri-invoice.mjs` automates the public Facturador SRI browser. That is not an approved server-to-server adapter for this control center and must not be enabled as production proof. `TDF.Invoice.SRI` remains a strict external-process boundary: it validates request shapes, totals, issued status, 49-digit authorization keys, invoice numbering, and redacts failures. Production activation additionally requires replacing the process with a direct offline adapter that passes every checklist item below; until then the `sri` provider row stays disabled and the UI must report configuration unavailable, never success.

Required direct-adapter conformance:

- Generate the correct current invoice/credit-note/debit-note XML schema and modulo-11 access key.
- Validate XML with the official XSD before signing.
- Sign with XAdES-BES using a valid Ecuadorian certificate outside the repository; passwords come from the secret manager and never argv/log/JSON storage.
- Submit to the official reception service for the selected test/production environment, persist `RECIBIDA`/`DEVUELTA`, then query authorization with bounded retries.
- Persist original XML, signed XML, authorization response, authorization number/time, environment, access key, and RIDE object-store reference immutably.
- Treat authorized/paid documents as immutable and issue approved credit/debit notes for correction.
- Pass official test-environment cases for accepted, returned, transient outage, duplicate access key, invalid signature/certificate, and authorization retry.

This documented disabled state is intentional: no credential, certificate, merchant approval, or absent adapter is represented as a successful integration.

## Disabling a provider

Set only its registry row to `enabled=false`. Leave capture and the control center enabled. In-flight attempts remain persisted; stop new claims, let current calls time out, mark retryable failure, and retain dead-letter replay. Re-enable only after a health check and sandbox smoke test. This isolates outages without losing internal work.
