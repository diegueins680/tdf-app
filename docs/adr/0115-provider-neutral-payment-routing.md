# ADR 0115: provider-neutral payment routing with verified activation

**Status:** Accepted (execution amendment 2026-09-14)
**Date:** 2026-09-11

## Context

TDF sells direct goods/services and operates marketplace-like flows. Existing product modules had Datafast, PayPal, Stripe, and manual paths with different method lists and repeated provider calls. Documentation capability, a configured-looking environment variable, and an executable merchant account are different facts. Optimistic fallback can duplicate a charge, and direct marketplace collection can create custody/regulatory risk.

## Decision

Use a canonical payment domain and exact provider adapter boundary. Routing requires an environment-specific provider account whose feature, credential-validation, and contract-approval gates are true, plus exact verified payment method and method-capability evidence. Runtime configuration is checked again before a route appears or an attempt starts.

Product flow, method, currency, amount, buyer country, operation, and required capabilities are inputs. Marketplace creation additionally requires connected accounts, split settlement, and seller payouts. The shared PlaceToPay/PayPhone create, status, notification, and authenticated-query executor is now present. Operational availability still requires complete runtime configuration, a ready account, approved contract, validated credentials, exact method-capability evidence, and environment flags. PlaceToPay additionally requires an explicit account/site payment-method ID mapping so a method selection cannot open a broader hosted checkout.

All monetary values use integer minor units. An attempt binds immutably to its canonical intent and provider resource. Cross-provider fallback is allowed only when the first provider was not contacted, rejected before resource creation, or authoritatively confirmed that no charge exists. Timeout, network loss, unverified return, or unknown provider state requires reconciliation.

Web and mobile consume server capability decisions. Paid mobile ticket checkout delegates to the canonical responsive web flow; the app does not maintain a second card executor.

Provider callbacks are typed by immutable evidence. PlaceToPay SHA-256 notifications are `signature_verified`; PayPhone's published external notification has no documented cryptographic signature and is `untrusted_callback`. Both are only triggers for an authenticated query that must match the stored merchant, external resource, merchant reference, amount, currency, checkout, attempt, and intent.

## Consequences

- Unsupported or unverified methods fail closed in UI and API.
- Capability documentation can be catalogued safely without activation.
- A missing secret, webhook ID, encryption key, contract flag, or executor removes the method from availability.
- A missing PlaceToPay method-code mapping removes only that method from availability.
- Existing product-specific provider calls remain compatible but must pass the canonical attempt gate; consolidating their HTTP execution is follow-up work.
- Legacy product checkout selectors continue to omit PlaceToPay and PayPhone until the shared return/app-switch/restore UX is wired; backend executability does not imply a usable client flow.
- No automatic failover occurs after an ambiguous result, trading a small availability cost for duplicate-charge safety.
- Marketplace checkout remains unavailable until a provider-managed fund flow is contracted and verified.

## Rejected alternatives

- Provider choice hard-coded in product modules: creates divergent states and UI/API claims.
- Enable from environment-variable presence: does not prove credential validity or contract entitlement.
- Retry another provider after timeout: can create duplicate charges.
- TDF-held seller balances followed by manual payout: an internal ledger does not make custody or aggregation compliant.
- Immediate replacement of every legacy endpoint: too risky for historical references and active product flows; use compatible gates and staged consolidation.
