# Event operations repository audit — 2026-09-14

## Scope and evidence standard

This is a read-only audit of baseline commit
`17a33eca11d585d84435af85340beece9b51d14e` (`origin/main` as available locally). Evidence names
tracked files, schemas, handlers, tests, and deployment controls. Presence of a file is not treated
as proof that an end-to-end workflow works. No production API, database, credential, charge, payout,
deployment, merge, or remote GitHub mutation was performed.

The repository's prior `FORMAL_VERIFICATION.md` and `npm run verify:formal` describe deterministic
JS reachability checks and a heuristic source audit. They are useful executable checks, but the
baseline contained no tracked `.tla`, `.cfg`, or `.als` models. This phase adds bounded TLC/Alloy
models without relabeling the prior checks as theorem proving.

## Repository and delivery topology

| Area | Evidence | Finding |
|---|---|---|
| Default branch | `.git` refs and `git worktree list`; `HEAD`/`origin/main` at `17a33eca1` | Default/current branch is `main`; primary worktree is dirty with unrelated music-release changes. Work isolated in a new worktree. |
| Backend | `tdf-hq/package.yaml`, `tdf-hq/tdf-hq.cabal`, `tdf-hq/src/TDF/API.hs`, `tdf-hq/src/TDF/Server.hs` | Haskell/Servant/Persistent/PostgreSQL service, built with Stack/GHC. |
| Web | `tdf-hq-ui/package.json`, `tdf-hq-ui/src` | React + TypeScript + Vite + MUI + React Query. |
| Mobile | `tdf-mobile` gitlink and mobile package in the checked-out primary worktree | Expo/React Native is a separate git submodule. The clean worktree does not initialize it; this phase does not mutate mobile. |
| E2E | `playwright.config.mjs`, `e2e/` | Playwright browser test infrastructure exists. |
| CI | `.github/workflows/ci.yml`, `build.yml` | Path-classified repository/UI/mobile/backend gates; backend uses PostgreSQL/pgvector and migration/concurrency tests. Build/publish follows CI on `main`. |
| Deployment | `fly.toml`, Dockerfiles, `scripts/production-release.mjs` | Fly production path exists. Event discovery and several newer domains are disabled by default; no deployment was attempted. |
| Migrations | `tdf-hq/sql`, `tdf-hq/db/migrations`, `scripts/production-migrations.json` | Multiple migration eras coexist. Production manifest is checksum pinned; new changes must include safe apply/rollback verification and manifest integration only at release review. |

## Tool availability

| Tool | Observed result |
|---|---|
| Git | Available; local branches/worktrees work. Remote `git remote show origin` failed DNS resolution. |
| GitHub CLI | Installed (`gh 2.95`), but active credentials were reported invalid. No branch push or PR can be verified. |
| Stack | `3.7.1`. |
| PostgreSQL client | `psql 16.10`. |
| Node/npm | Node `24.8.0`, npm `11.6.0`. |
| Playwright | `1.59.1` resolved through the existing Node environment. |
| Docker | Binary present; daemon socket denied inside the sandbox. |
| Java | `/usr/bin/java` was an unavailable macOS stub. Homebrew OpenJDK install failed on outdated Apple Command Line Tools. A downloaded bottle was extracted temporarily and reported OpenJDK `21.0.12.1`. |
| TLC | Not installed initially. Official `tla2tools.jar` 1.7.2 downloaded temporarily and checksum verified. Completed bounded runs are recorded in `formal/event-operations/README.md`. |
| Alloy | Not installed initially. Official Alloy 6.2.0 distribution JAR downloaded temporarily and checksum recorded. SAT4J scenario/assertion runs completed. |

## Existing domains and reuse decisions

### Follow-up capability verification

During the fourth branch, read-only GitHub access recovered after sandbox escalation.
`gh repo view diegueins680/tdf-app --json defaultBranchRef,nameWithOwner` returned `main`;
`gh api repos/diegueins680/tdf-app/commits/main --jq .sha` and
`git ls-remote --heads origin main` both returned the audited
`17a33eca11d585d84435af85340beece9b51d14e`. The earlier DNS/auth observations above are
historical, not a current assertion that remote delivery is impossible. Publication evidence,
if completed, belongs in the delivery report. No production permissions were exercised.

### Events, venues, RSVP, tickets, virtual/live

- `TDF.Models.SocialEventsModels` already owns `SocialEvent`, artists, RSVP, event invitations,
  moments/comments, live broadcast, ticket tiers/orders/tickets, budgets, finance entries, and basic
  logistics. Preserve `SocialEventId` and APIs.
- A `SocialEvent` has one optional `organizerPartyId`, one venue, one event type/workflow state, UTC
  timestamps, optional timezone, price cents/currency, and metadata. Multiple owners/coproducers,
  sessions/spaces, revisions, and field visibility are absent.
- `TDF.SocialEventLifecycle` recognizes `planning`, `announced`, `on_sale`, `live`, `postponed`,
  `unavailable`, `out_of_scope`, `completed`, and `cancelled`. Catalog seed/trigger data in
  `2026-08-14_catalog_integrity.sql` permits broad direct jumps and does not encode the required
  approvals or contextual authority.
- Existing public ticket checkout is comparatively strong: ADRs 0112/0113 and migrations such as
  `2026-08-18_public_event_ticket_checkout_runtime.sql` separate browser return from provider
  evidence, reserve capacity, use idempotency, and preserve organizer liability. Reuse it.
- Virtual venue, live session/broadcast, streaming routes, rooms, and venue explorer features exist,
  but are not normalized as multi-session event spaces.

### Logistics, tasks, assets, calendars

- `2026-07-21_event_logistics.sql`, `TDF.EventLogisticsRoutes`, and the web `EventLogisticsPage`
  implement plans, viewer/editor members, places, activities, assignees, dependencies, route
  verification, checkpoints, and delivery deduplication.
- Activity version checks exist, but entity update and assignment/dependency replacement happen in
  separate database calls. Dependency reachability is checked in Haskell before insertion. Two
  concurrent writers can therefore race, and task completion does not enforce prerequisites.
- Assignments do not express RACI; deletion can erase operational history; collaborator removal does
  not atomically protect required responsibility coverage.
- `Room`, `Resource`, `Asset`, checkout/audit/maintenance/stock, and external-calendar mappings exist.
  Some records use free-text party/resource references, and asset/resource calendars are duplicated.
  Normalize through adapters and canonical resource allocation rather than replace them.

### Discovery, opportunities, reputation

- Directory schemas and handlers already cover profiles, professions/skills/services, instruments,
  genres, languages, rates in minor units, service areas/city/radius, remote/travel preferences,
  equipment summaries, visibility/moderation, classifieds/opportunities/applications, saved search,
  chat context, and reputation/availability scores.
- Reuse directory discovery and classifieds for the event ecosystem. Missing pieces include a
  documented explainable rank decomposition, event-working-condition filters, conflict-aware
  availability, privacy-safe certification verification, and event engagement linkage.
- Reviews have a useful verified-interaction gate. Extend the qualifying interaction to completed
  canonical engagements; do not create another ratings table.

### Availability and bookings

- Existing `Resource`/`Booking` models support person, room, and equipment resources with tentative,
  confirmed, in-progress, completed, cancelled, and no-show states.
- `2026-08-16_service_booking_checkout_runtime.sql` and unified commerce migrations route allocation
  through PostgreSQL exclusion constraints. The repository includes a public-booking HTTP
  concurrency test. This is the primary reservation primitive to reuse.
- Event participants, venues, rooms, and assets are not consistently bound to canonical resources.
  Buffer intervals, capacity greater than one, explicit override policy/reason, multi-day travel,
  and event engagement constraints are incomplete.

### Contracts, payments, disputes, accounting

- Unified checkout/provider attempts/inbox, refunds/disputes, holds, idempotency, ledger postings,
  provider adapters, verification gates, payout/settlement scaffolding, and feature flags exist in
  the `commerce_*` migrations and `TDF.Commerce.*`. Exact minor units and disabled-by-default
  providers are reusable strengths.
- Event budget lines and finance entries use cents/currency but lack versioned approval thresholds,
  committed-versus-actual procurement, and engagement settlement linkage.
- The generic contract API is not a production-safe engagement domain: `TDF.Contracts.Server`
  stores JSON files under `contracts/store`, the “send” endpoint can return success without sending,
  and the simple 2025 contracts table lacks event links, immutable versions, party consent,
  milestones, cancellation, and disputes. `base_contract.schema.json` contains a placeholder.
  These flows must be disabled or replaced behind authenticated canonical contracts; generated
  templates are not legal advice.

### Authorization, invitations, collaboration, notifications

- Authentication is party-based and catalog security supplies role/module/action assignments, but
  event authorization is mostly organizer/admin or logistics viewer/editor. Resource/field scopes,
  temporary grants, separation of duties, and stale/offline re-evaluation are absent.
- `claimOrRequireEventManager` can claim an event with a null organizer. Migration must resolve
  legacy ownership explicitly before removing this compatibility behavior.
- Event invitation handlers use visible-event lookup. Creation and update paths do not consistently
  require organizer authority; a user who can view an event may be able to create/mutate an
  invitation. The model has no external-email token hash, expiry, one-use constraint, version, or
  immutable conversion audit. This is a critical authorization gap.
- Directory invitations are stronger (expiry, idempotency/fingerprint, status version) but target
  existing profiles and are a separate collaboration mechanism. Extract/reuse their patterns.
- Core chat is direct-party oriented. Event activity/comments exist, but there is no unified
  event/task decision thread. Notifications are primarily basic in-app records plus domain-specific
  outboxes; canonical preferences, quiet hours, escalation, retry/dead-letter, and push/email parity
  are incomplete.

## Highest-priority risks

| Severity | Risk | Required treatment |
|---|---|---|
| Critical | Visible-event authorization may permit unauthorized invitation creation/mutation. | Add contextual owner/delegate guard immediately in implementation phase; secure token lifecycle and regression tests. |
| Critical | Generic contracts can report a fake “sent” state and lack authenticated immutable consent. | Disable externally meaningful success; replace with versioned DB-backed engagement contracts before UI exposure. |
| High | Existing lifecycle jumps bypass the required approvals/readiness/settlement semantics. | Add a compatibility-mapped canonical transition service and DB guard; do not rewrite historical state IDs. |
| High | Logistics cycle/completion validation is race-prone and split across transactions. | Serialize graph mutation, enforce expected version, and add concurrency/DB tests. |
| High | Organizer-null claiming can transfer legacy event control opportunistically. | Backfill reviewed owners, log claims, restrict compatibility window, then remove. |
| High | Resources are duplicated/inconsistently linked, making conflict prevention incomplete. | Introduce stable resource bindings and route all confirmed allocations through the exclusion calendar. |
| High | Payment foundations are strong but event contract/milestone/payout coupling is missing. | Keep event payouts disabled; require verified provider evidence, ledger balance, approvals, and reconciliation. |
| Medium | Free-text party/resource references impair authorization and data integrity. | Migrate to typed foreign keys with compatibility projections and exception reports. |
| Medium | Search ranking is not fully explainable and availability-aware. | Persist public factor contributions and policy version; exclude sensitive attributes. |
| Medium | Offline/version conflict policy is not uniform across web/mobile. | Define command envelope and server-side reauthorization; surface non-mergeable conflicts. |
| Medium | Migration families and model layers overlap. | Additive, checksum-pinned migrations, explicit rollback scripts, dual-read comparison, no destructive backfill. |

## Baseline verification status

The formal models added in this branch were executed and pass within documented bounds. Existing
repository unit, Haskell, browser, mobile, migration, and CI suites have not yet been claimed as a
full baseline for this phase. They must be executed proportionally with each implementation slice.
GitHub CI could not be queried or triggered because remote access/authentication was unavailable.
