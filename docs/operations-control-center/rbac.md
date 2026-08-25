# Operations RBAC and field-access matrix

All access is default-deny and enforced in `TDF.Operations.Server`. Active party membership in the requested organization and branch is required before role evaluation.

| Role | Visible work by default | Mutations | Restricted fields |
| --- | --- | --- | --- |
| Admin | All organization/branch work, including security | Full operations, configuration, replay, dual approval | Secrets/payment credentials are never projected |
| Manager / StudioManager | All operational work except security incidents | Assign, prioritize with reason, replay, request/approve | Financial detail still follows source-route permission |
| Accounting | Invoice, payment, marketplace-payment, registration payment context | Finance queue, notes, lifecycle, approval | No provider secrets/PAN/CVV/raw payload |
| Reception | Registration, party, booking, invoice/payment indicators, inbound requests, quote/event | Queue/assignment/lifecycle; no unrestricted refund | No tax secrets, margins, payment credentials |
| Teacher | Assigned registration/booking/project/event only | Safe assigned-work lifecycle and notes | No unrestricted financial/tax/contact metadata |
| Engineer | Assigned booking/maintenance/project/event only | Safe assigned-work lifecycle and notes | Payment-clearance indicator only |
| Maintenance | Maintenance, stock warnings, booking impact, manual work | Safe asset workflow and notes | No financial/tax/contact metadata |
| ReadOnly | Broad non-security operational view | None | Same projection redaction |

Field policy:

- Operational metadata strips credentials, signatures, certificates, private keys, card data, seeds, raw provider payloads, tax IDs, addresses, phones, and emails for every role.
- Contact/legal/payment details are fetched only from the existing source endpoint after that endpoint reauthorizes the actor.
- Search, KPI counts, replay feed, saved/shared views, and failure queue use the same organization/branch/role predicates as list/detail.
- Push contains only an opaque work-item ID and generic category. Device tokens are encrypted at rest.
- Cross-tenant cache keys include operation, scope, filters, and actor authorization context; this release does not introduce shared server caches.

Consequential actions require explicit UI confirmation. Refund, reversal/void, financial chargeback resolution, paid/near-term cancellation, credit/debit note, issued-document change, privacy erasure, permanent deletion, and configured threshold actions require a separate requester and approver. PostgreSQL and the API both prevent self-approval.
