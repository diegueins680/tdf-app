# Product and implementation inventory

## Architecture and authorities

| Surface | Implemented architecture | Test authority |
|---|---|---|
| `tdf-hq` | Haskell, Servant, Persistent and PostgreSQL; API composition, state transitions, role/module/feature/action enforcement, audit/provider records and seeds | Backend integration tests plus authoritative database state |
| `tdf-hq-ui` | React, TypeScript, Vite, MUI, React Query and generated API types; public and protected React Router surfaces | Jest/component tests and Playwright browser journeys |
| `tdf-mobile` | Expo/React Native submodule, Expo Router, React Query, local persistence and generated feature registry/API types | Jest/component tests and Detox on configured iOS simulator |
| Feature/permission model | `tdf-hq/assets/feature-registry.json` with a generated mobile copy; role/module/action matrix | Registry audit, backend action checks and negative direct-API tests |
| Commerce | Domain-linked checkout, immutable snapshots, provider attempts/events, holds, refunds/reconciliation concepts spread across services, marketplace, courses, tickets and Domo | Verified provider evidence and database ledgers, never browser-return status alone |

The feature registry currently contains 137 records. The router audit resolves 156 web routes and 44 Expo Router routes; 37 product features have a native mobile treatment. The refreshed backend parser expands 548 concrete endpoints: 493 map to features, while 40 require explicit product/security disposition. The current role–module–feature–action–platform matrix contains 3,014 rows for 11 representative role fixtures and 17 actions.

## Implemented product areas

- Identity: registration/login surfaces, Google OAuth, password reset/recovery, sessions, onboarding, preferences and multi-role session snapshots.
- Profiles and discovery: people/artists, venues, professionals, portfolio/media, public profiles, claims, reviews/favorites, city/profession/genre/service filters, search suggestions and classifieds.
- Community: contacts, chat/messaging, social events, moments, live broadcasts, collaboration/invitations, reports and external-event research behind a feature flag.
- Studio and operations: public service catalog, room/availability/booking paths, orders, sessions, pipelines, operational assignments, inventory, maintenance, reports and Live Sessions.
- School: instructors/students, public courses, trials, registrations, seat holds, checkout, schedules and attendance.
- Marketplace: listings, carts, sales, dated rentals, availability/holds, orders, custody/condition evidence, cancellation, dispute and settlement-related state.
- Events/tickets: public event discovery/storefront, tier inventory/holds, checkout, issuance, tracking, refunds, transfers/waitlists and validation records.
- Payments/finance: Datafast, PayPal and Stripe-related paths, provider attempts/events, immutable bindings, idempotency, ledger/receipt concepts, refund operations and reconciliation exceptions.
- Domo: public discovery/inquiry, staff quote lifecycle, authoritative quote checkout, deposit gate, booking and operational follow-up.
- CRM/admin: contacts, companies, leads, activities, assignment/conversion, users, roles, modules, settings, CMS/catalog revisions, feature explorer, access requests, diagnostics and audit.
- Label/distribution: artists, projects, contracts, recordings/releases/assets/tracks/metadata, DDEX inbox/validation/partners, imports, delivery models and status tracking.
- Integrations: OAuth/Google Drive and social clients, email/notifications/WhatsApp links, payment providers, streaming/social surfaces and distribution partner abstractions.

## Public entry points

The implemented public route families include home/platform pages, registration/login/reset, directory search and public profile/venue detail, community/event discovery, public event tickets, marketplace, services and booking/order tracking, courses/trials, Live Sessions, feedback/support/WhatsApp, Records content and Domo discovery/quote checkout. Public read does not imply public mutation: checkout, ownership and protected continuation must cross an explicit authentication or capability boundary.

## Partial, concealed or disconnected areas

| Area | Repository truth | Program disposition |
|---|---|---|
| DDEX storage, preview/download, import-plan/commit, export and catalog read-through | Multiple handlers return `501`; previously documented on 2026-08-06 and in the revenue architecture audit | High release gate; actions remain incomplete/concealed until storage, authorization, rollback, idempotency and partner contract tests pass |
| DDEX partner delivery/acknowledgement/DSR/royalty/payout | Models/architecture exist without a certified live partner lifecycle | External/manual gate; no real distribution |
| Social event discovery | Beta behind `EVENT_DISCOVERY_ENABLED` | Test both enabled/disabled; retain moderation/source-quality gate |
| Native contracts authoring/detail | Placeholder/incomplete | Concealed; do not represent as successful native capability |
| Native messages, release authoring, teacher, intern and broad admin workflows | Web fallbacks or no native equivalent | Explicit parity backlog, not an invisible native route |
| Seven formerly false mobile routes | Registry originally pointed to Expo routes that did not exist | Home remains a truthful `external-web` continuation; six completed directory search/detail/management families now resolve to real native routes |
| Forty backend endpoints | No current feature/action/interface disposition | High permission/discoverability audit gate; inventory is in `pending-backend-capabilities.csv` |
| OpenAPI coverage | Existing architecture audit found major commercial routes absent from the canonical contract | Contract coverage gap until backend compilation/generation and client diff can be run |
| Detox | iOS simulator only, hard-coded iPhone 16 device; existing flow depends on an app build and isolated auth configuration | Not executed locally; Android configuration is absent |

## Documentation reconciliation

The 2026-08-06 feature audit was acknowledged rather than treated as fresh discovery. It reported 115 features, 125 web routes, 35 mobile routes, 408 backend endpoints and 2,530 matrix rows, with zero unresolved dispositions. Repository growth and a parser limitation made that packet stale. The refreshed 2026-08-21 packet reports 137 features, 156 actual web routes, 44 mobile routes, 548 expanded endpoints and 3,014 matrix rows. It also refuses to hide 40 newly visible unresolved endpoints.

The 2026-08-13 revenue architecture audit and ADRs 0100–0114 already identify false-success risks, verified-event authority, versioned money/holds, marketplace custody, course seats, ticket checkout and Domo quote/payment invariants. This program treats those as prior work, uses them in scenario acceptance criteria and does not relabel them as novel findings.

GitHub pull requests and open issues were inspected before local changes. Open PR #194 and open issues #128/#130 did not describe these persona-program fixes; no issue, branch, commit, push or PR was created because the required separate authorization has not been requested or granted.

## Risk prioritization

Payment authority/idempotency, authentication/session recovery, cross-epic financial reconciliation, studio conflicts, finance and administration are first because they combine high reach with financial, privacy or data-integrity consequences. Tickets, marketplace and distribution follow because they add scarcity, custody, rights and multi-party state. Discovery/accessibility and profiles rank ahead of lower-frequency collaboration because they control entry and conversion. The exact story counts and rationale are in [epic inventory](epic-inventory.md).
