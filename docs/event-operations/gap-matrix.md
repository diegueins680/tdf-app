# Event operations gap matrix

Classification is about the audited baseline at `17a33eca1`, not planned work. `Reusable` means the
capability has a credible implementation primitive; it does not mean every requested journey is
complete. `Modify` means retain identity/data but strengthen it. `Missing` requires implementation.
`Consolidate` identifies overlapping sources of truth. `Deferred` includes a concrete dependency.

| ID | Requirement | Classification | Evidence and decision |
|---|---|---|---|
| EO-001 | Existing event aggregate and physical venue | Existing and reusable | `SocialEvent`, `Venue`, public/detail APIs and UIs exist. Preserve IDs/routes. |
| EO-002 | User or organization ownership | Existing but requiring modification | One optional `organizerPartyId` uses Party, but lacks explicit ownership history and minimum-owner constraint. |
| EO-003 | Multiple owners/coproducers and delegated/time-bounded permission | Missing and requiring implementation | Logistics members are only viewer/editor; no canonical scoped grant interval or coproduction relation. |
| EO-004 | Single/multi-day dates, sessions, stages, rooms, multiple venues | Existing but requiring modification | Event has one start/end/venue; rooms/places/live sessions exist separately. Add session/space bindings. |
| EO-005 | Physical, virtual, hybrid attendance | Existing but requiring modification | Event types, live broadcast/streaming/virtual venue exist but are not normalized to session attendance modes. |
| EO-006 | IANA timezone, UTC persistence, Guayaquil default | Existing but requiring modification | UTC timestamps and optional timezone exist; deterministic validation/round-trip/recurrence is incomplete. |
| EO-007 | Visibility scopes and field-level control | Existing but requiring modification | Public/private event/directory flags exist; role/assigned/field-level scopes are absent. |
| EO-008 | Versioned event information and immutable audit | Missing and requiring implementation | Some version columns/activity events exist; no canonical event revision or append-only event audit. |
| EO-009 | Required 14-state lifecycle and controlled extensions | Existing but requiring modification | Catalog lifecycle exists with 9 different, broad states. Add compatibility mapping, guards, approvals, effects. |
| EO-010 | Lifecycle rollback/compensation across tickets/contracts/tasks/bookings/payouts | Missing and requiring implementation | Domain-specific cancellation exists; no orchestrated transition consequence contract. |
| EO-011 | Venues/rooms/stages/virtual venues/site plans | Existing but requiring modification | Venue, Room, logistics Place, virtual/live domains exist; typed event-space/site-plan revision missing. |
| EO-012 | Schedule/timeline/milestones/run of show | Existing but requiring modification | Logistics activities and calendars exist; full run-of-show semantics, relative schedule, gates incomplete. |
| EO-013 | Technical production/riders/backline/AV/power/network/streaming | Missing and requiring implementation | Attachments/free-text activities can approximate it; no typed reusable requirements/workflows. |
| EO-014 | Equipment/assets/stock/reservation/custody/damage/return | Existing but requiring modification | Asset, inventory, checkout, audit, maintenance and booking resources exist; event/resource links and conflict policy need normalization. |
| EO-015 | Staffing/shifts | Missing and requiring implementation | People resources/bookings and tasks exist; shifts, coverage/capacity, breaks and check-in are absent. |
| EO-016 | Vendors/quotes/procurement/PO/approvals | Missing and requiring implementation | Directory services, expenses and generic commercial quotes exist; canonical event procurement/PO approval chain absent. |
| EO-017 | Transport/routing/parking/loading/accommodation/hospitality/catering | Existing but requiring modification | Logistics places/routes/checkpoints exist; typed requirements, assignments, vendors and budgets incomplete. |
| EO-018 | Permits/insurance/accessibility/health/safety/security/emergency | Missing and requiring implementation | Can be represented as free-text tasks only; needs protected typed requirements, approval/evidence and incident linkage. |
| EO-019 | Budgets, estimates/commitments/actuals/income/variance/thresholds | Existing but requiring modification | Event budget lines/finance entries use cents; no versioned thresholds, commitments, approval or ledger reconciliation. |
| EO-020 | Risks/contingencies/incidents/follow-up | Existing but requiring modification | Operations/incidents and logistics alerts exist; event risk register/response/follow-up relation incomplete. |
| EO-021 | Documents/revisions/approval/visibility | Existing but requiring modification | Attachments/private assets exist in several domains; canonical event document version/approval/access model absent. |
| EO-022 | Versioned/clonable event templates plus realistic seeds | Missing and requiring implementation | No full task/RACI/dependency/budget/gate template aggregate. Music and non-music seeds must be data, not code branches. |
| EO-023 | Workstreams/groups/tasks/subtasks/checklists/milestones/views | Existing but requiring modification | Logistics activities and Kanban/checklist UI foundations exist; PR 21 adds internal aggregate revision tracking for task/RACI commands. Public editing, hierarchy/views/history remain incomplete. |
| EO-024 | Complete RACI, approvers/watchers/escalation | Missing and requiring implementation | Existing assignment is party/external contact without RACI semantics. |
| EO-025 | Dependencies/blockers/deadlines/reminders/recurrence/effort | Existing but requiring modification | Dependencies/priority/dates exist; transactional DAG, blockers, recurrence, deterministic timezone and effort incomplete. |
| EO-026 | Evidence/comments/acceptance/approval/reject/reopen/complete | Existing but requiring modification | Event comments/attachments and activity status exist separately; acceptance and approval workflow/history incomplete. |
| EO-027 | Dependency-gated completion with audited override | Missing and requiring implementation | Current completion does not enforce prerequisites; precheck is not transactionally safe. |
| EO-028 | Exactly one A, required R, no silent orphan | Missing and requiring implementation | Formally specified here; requires DB/command constraints and collaborator-removal transaction. |
| EO-029 | Discovery across full event ecosystem | Existing but requiring modification | Directory roles/profiles/services/equipment cover much of the ecosystem; venue/supplier/event engagement integration incomplete. |
| EO-030 | Opportunity publication/direct invitation/apply/shortlist | Existing but requiring modification | Directory classifieds/applications/invitations exist; must link to event/engagement and strengthen authorization. |
| EO-031 | Search filters: role, geo, availability, rate, reputation, experience, language, equipment, conditions | Existing but requiring modification | Most profile/rate/geo/language/equipment signals exist; conflict-aware availability/conditions/certification filters incomplete. |
| EO-032 | Explainable, non-discriminatory, privacy-safe ranking | Missing and requiring implementation | Scores exist but no complete persisted factor explanation/policy version/sensitive-feature allowlist. |
| EO-033 | Availability calendar and tentative expiring holds | Existing but requiring modification | Resources/bookings and commerce holds exist; bind all event actors/resources and expose policy-safe availability. |
| EO-034 | Exclusive/capacity conflict prevention under concurrency | Existing but requiring modification | PostgreSQL exclusion allocation and an HTTP concurrency test exist; buffers/capacity/overrides/event binding incomplete. |
| EO-035 | Multi-day/cross-zone/travel/setup/soundcheck/teardown/recovery buffers | Missing and requiring implementation | No single canonical occupied-interval calculator and property suite. |
| EO-036 | Idempotent booking/hold side effects | Existing and reusable | Commerce idempotency, provider inbox and reservation primitives exist; engagement commands must adopt them. |
| EO-037 | Complete 13-step hiring/engagement lifecycle | Existing but requiring modification | Listings, applications, bookings, contracts, commerce, reviews exist as disconnected slices. Introduce one engagement orchestrator. |
| EO-038 | Versioned offers/contracts and exact consent | Missing and requiring implementation | Current file-backed contract API lacks secure versions/party acceptance and contains placeholder/fake-send behavior. |
| EO-039 | Cancellation/no-show/partial/replacement/dispute evidence | Existing but requiring modification | Booking no-show, refunds/disputes and provider evidence exist; engagement policies and replacement links incomplete. |
| EO-040 | Feedback only after verified engagement | Existing but requiring modification | Verified directory interaction gate exists; extend to canonical completed engagement. |
| EO-041 | Quotes/deposits/milestones/fees/taxes/multicurrency/exact values | Existing but requiring modification | Unified commerce and exact minor units are reusable; event engagement pricing/tax/milestone allocation incomplete. |
| EO-042 | Refund/dispute/chargeback/payout/reconciliation/immutable ledger | Existing but requiring modification | Canonical commerce/provider operations/ledger foundations exist; event payout/settlement gates remain incomplete. |
| EO-043 | Verified provider event; browser redirect not success | Existing and reusable | Ticket/service commerce ADRs and provider adapters enforce this pattern; engagement must use same boundary. |
| EO-044 | Provider flags, sandbox only, disabled-by-default | Existing and reusable | Canonical provider lifecycle starts providers disabled and uses environment-scoped flags. No live calls authorized. |
| EO-045 | RBAC + contextual event/resource/relationship/state/time authorization | Existing but requiring modification | Party roles/modules exist; event scopes, field policy, time bounds and separation of duties missing. |
| EO-046 | Retention/export/correction/deletion and tamper-resistant audit | Missing and requiring implementation | Fragmented audit records exist; no unified privacy workflow or insert-only event audit policy. |
| EO-047 | Secure guest invitation/account conversion/idempotent replay | Existing but requiring modification | Directory invitation offers partial patterns; event invitation authorization/token/expiry/conversion are unsafe/incomplete. |
| EO-048 | Event/task chat, activity, mentions, decisions, attachments | Existing but requiring modification | Direct chat and event comments/activity exist; scoped thread/decision model and inherited visibility incomplete. |
| EO-049 | In-app/email/push preferences, quiet hours, retries/dedupe/DLQ | Existing but requiring modification | Basic notifications and domain outboxes exist; canonical delivery policy/quiet hours/DLQ observability incomplete. |
| EO-050 | Calendar/iCal/webhooks | Existing but requiring modification | Calendar mappings and webhook/provider patterns exist; event/session/RACI feeds and subscriber policy incomplete. |
| EO-051 | Versioned writes/autosave/offline queue/conflict resolution | Existing but requiring modification | Some OCC/offline client patterns exist; no canonical command envelope or safety-critical conflict policy. |
| EO-052 | Coherent accessible web event workspace | Existing but requiring modification | Existing event route now has an exact-task RACI read subview linked from logistics, with context-fenced receipts. Rich task commands, workspace navigation, budget/contracts/audit coverage, mobile and comprehensive a11y remain incomplete; see `task-view-contract.md`. |
| EO-053 | Coherent accessible mobile event workspace | Existing but requiring modification | Mobile event/create/detail/ticket/directory/booking screens exist; submodule not initialized in clean worktree and parity is incomplete. |
| EO-054 | Spanish default, English fallback, locale/jurisdiction configuration | Existing but requiring modification | Spanish-first UI patterns exist; timezone/currency/tax/address/legal configuration needs consolidation and tests. |
| EO-055 | Typed APIs, generated clients, workers, observability | Existing but requiring modification | Servant types, generated clients, workers/logging/metrics patterns exist; new commands must follow them. |
| EO-056 | Reversible migrations, constraints/indexes, compatibility rollout | Existing but requiring modification | Strong migration test/manifest patterns exist; overlapping migration/model eras increase risk and require additive dual-read rollout. |
| EO-057 | Formal TLA+/PlusCal and Alloy models | Missing and requiring implementation | Baseline had only lightweight JS/YAML checks. This branch adds bounded models and verified results before feature code. |
| EO-058 | Executable contracts/model/property/concurrency/auth/e2e/a11y/performance tests | Existing but requiring modification | Bounded models, canonical DB/HTTP boundaries and task read browser fixtures now have phased coverage. PR 20 verifies fixture isolation and desktop/phone task journeys with unchanged limits. Full-stack journeys, native mobile, complete accessibility and controlled performance coverage remain; do not weaken gates. |
| EO-059 | Production legal/accounting/security activation | Deferred | Requires qualified human review, provider certification, production credentials/authority and operational readiness. Must remain disabled. |
| EO-060 | GitHub feature branches/PRs/checks | Deferred | Local branch/worktree created; DNS and invalid `gh` authentication prevent verifiable push/PR/check operations. |
| EO-061 | Live charges/refunds/payouts/deployment | Deferred | Explicitly prohibited by the request; sandbox/test providers only. |
| EO-062 | Duplicate event/invitation/resource/contract systems | Duplicated and requiring consolidation | Social events vs related schedule/live types, event vs directory invitations, Asset/Room/Resource calendars, and file/DB contracts need canonical adapters and migration, not another aggregate. |

## Delivery implication

Phase 1–2 can close EO-057 and make EO-001–062 traceable, but it does not make the application
end-to-end complete. Implementation must proceed in dependency order: authorization/audit/lifecycle,
then transactional task/RACI and resource bindings, then engagement/contracts, then finance,
collaboration/offline, and finally web/mobile completeness and hardening.

## Incremental shared-profile audit (PR 16)

EO-031–032, EO-052 and EO-058 remain incomplete. The [artist-follow repair](pr-16-artist-follow-continuity.md)
restores explicit consent and local return continuity, with stale-context and retry tests,
without introducing another profile or engagement system. Existing `fanFollowArtist`
also auto-creates bidirectional club-member `PartyFollow` relationships; this requires
separate consent/privacy modification and verification. Its policy is not validated by
the artist-click model or synthetic browser fixture. FanHub's separate five failing
onboarding tests remain a concrete integration gap, not silently replaced expectations.

PR 17 resolves that five-test FanHub baseline within a bounded compatibility repair:
current-session canonical eligibility, explicit empty exit, validated terminal receipts,
safe recovery and named loading states. See [PR 17 results](pr-17-fanhub-onboarding.md).
This updates the incremental checkpoint, not the audited baseline classification above;
shared hub follow/profile mutation fences, club consent and complete event UX remain gaps.
