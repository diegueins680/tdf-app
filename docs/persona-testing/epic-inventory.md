# Risk-based epic inventory

Priority combines user impact, affected reach, business value, security/data-integrity exposure, and practical testability. Equal priority numbers indicate intentionally shared urgency.

| Priority | Epic | Risk | Stories | Why now |
|---:|---|---|---:|---|
| 1 | EP-01 — Identity, authentication, onboarding, and sessions | critical | 3 | Every protected conversion depends on reliable identity and recovery. |
| 1 | EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation | critical | 7 | Incorrect authority or idempotency can cause duplicate charges or financial misstatement. |
| 1 | EP-17 — Public-to-operational-to-financial cross-epic lifecycles | critical | 1 | Revenue is realized only when public intent reaches operations and reconciliation without state gaps. |
| 2 | EP-05 — Studio services, reservations, orders, sessions, and Live Sessions | critical | 7 | Scheduling conflicts and incomplete payment state directly affect operations and revenue. |
| 2 | EP-14 — Finance, reports, refunds, reconciliation, and audit trails | critical | 6 | Financial state requires verified evidence, dual control, and durable auditability. |
| 2 | EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations | critical | 5 | Administrative errors can affect every user and must be backend-enforced. |
| 3 | EP-08 — Public events, tickets, checkout, issuance, and validation | critical | 3 | Public conversion and fraud-resistant entry depend on authoritative ticket state. |
| 4 | EP-07 — Marketplace sales, rentals, availability, tracking, cancellations, and disputes | critical | 4 | Money, inventory, custody, deposits, and multi-party ownership must remain consistent. |
| 4 | EP-12 — Label, releases, assets, tracks, DDEX, partners, delivery, and status | critical | 4 | Rights, unreleased media, metadata, and external delivery failures carry contractual risk. |
| 5 | EP-03 — Search and discovery | high | 6 | Discoverability is the entry to audience, lead, and revenue journeys. |
| 5 | EP-16 — Accessibility, localization, privacy, help, and recovery | high | 9 | Inclusive, comprehensible recovery determines whether journeys are independently completable. |
| 6 | EP-02 — Profiles, multi-role identities, portfolios, and public pages | high | 5 | Profiles drive discovery while exposing ownership and privacy boundaries. |
| 7 | EP-10 — Domo discovery, quotes, availability, booking, and follow-up | high | 1 | A public lead must retain context through quote, deposit, booking, and operations. |
| 7 | EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody | high | 6 | Availability and custody must prevent unsafe or conflicting operations. |
| 8 | EP-06 — School, courses, trials, registrations, schedules, and attendance | high | 3 | Seat inventory, schedules, student privacy, and minor consent intersect. |
| 9 | EP-11 — CRM contacts, companies, leads, activities, assignments, and conversion | high | 2 | Lead provenance and assignment affect conversion and private contact data. |
| 10 | EP-04 — Community, contacts, messaging, events, and collaboration | high | 6 | Multi-user interactions create moderation and isolation risks. |
