# Event operations domain glossary

| Term | Canonical meaning |
|---|---|
| Party | Existing TDF identity boundary representing a user-controlled or organization-controlled actor. Authorization and money never key directly on display names or email addresses. |
| Event | The existing `social_event` record extended by event-operations relations; no second event aggregate is introduced. |
| Owner | A party with durable ownership authority over an event. Ownership is explicit and auditable. |
| Coproducer | A party related to an event with explicit scopes. Coproduction is not ownership and grants no implicit global organization role. |
| Grant | Revocable, event/resource-scoped authority with a validity interval and provenance. |
| Event revision | Immutable snapshot of material event information. A mutable projection points to the current revision. |
| Session | A scheduled segment of an event, persisted as UTC instants plus an IANA presentation timezone. |
| Space | A physical venue/room/stage or virtual venue/channel assigned to a session. |
| Workstream | Top-level operational partition such as production, hospitality, security, finance, or broadcast. |
| Actionable task | Work item whose completion affects readiness and therefore requires Responsible and Accountable coverage. |
| Task aggregate revision | Monotonic storage counter for one canonical task's activity, policy, RACI, outgoing dependencies and overrides. Distinct from the legacy activity version, event-wide write fence and immutable audit history. Matching it is not authorization or proof of current temporal readiness. |
| RACI | Responsible does the work; exactly one Accountable party owns the outcome; Consulted participates bidirectionally; Informed receives relevant updates. |
| Dependency | Directed “blocked task depends on prerequisite” relation. The graph must remain acyclic. |
| Override | Exceptional authorized transition with a non-empty reason, policy reference, actor, time, and immutable audit evidence. |
| Opportunity | Searchable request for event work or supply; it reuses directory/classified and profile discovery rather than becoming corporate HR. |
| Engagement | Negotiated relationship among an event, hiring party, provider party, scope, schedule, and accepted contract version. |
| Hold | Expiring, non-final claim over a person, space, asset, or other resource. |
| Booking | Confirmed or terminal reservation whose occupied interval includes configured setup/travel/recovery buffers. |
| Exclusive resource | Resource whose capacity policy rejects overlapping confirmed allocations unless an explicit override policy permits one. |
| Contract version | Immutable offer/terms payload. Material amendment creates a new version and invalidates prior consent for confirmation purposes. |
| Milestone | Contractual deliverable or approval gate; distinct from an operational task even when linked to one. |
| Checkout | Existing provider-neutral commercial intent and payment-attempt boundary. Browser return is never payment evidence. |
| Settlement | Reconciled allocation of captured money, fees, tax liabilities, refunds, disputes, and payable/payout obligations. |
| Audit event | Append-only security/operations fact describing actor, command, before/after state, reason, correlation, and timestamp. |
| Projection | Rebuildable read model derived from authoritative event, audit, booking, contract, and accounting facts. |
| Offline command | Idempotent, version-conditioned user intent queued on a supported client and later synchronized or surfaced as a conflict. |
| Visibility | Public, unlisted, private, internal/team, role-only, assigned-only, or field-specific disclosure policy. |
| Template | Versioned plan definition containing relative dates, task/RACI/dependency rules, budgets, checklists, requirements, and approval gates. Instantiation freezes its source version. |
