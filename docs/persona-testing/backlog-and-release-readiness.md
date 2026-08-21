# Prioritized backlog, coverage gaps and release readiness

## Implemented fixes and coverage

| Finding | Repository-side improvement | Regression evidence | Rollback consideration |
|---|---|---|---|
| PT-001 | Replaced seven nonexistent native destinations with truthful responsive-web fallbacks; regenerated the mobile registry | Feature audit and mobile registry/Jest pass | Revert registry/generated copy together; never restore false native routes without real Expo screens |
| PT-002 | Normalize malformed catalog responses in theme and locale providers | Theme unit test and both Playwright stories | Guard is fallback-only; rollback would restore blank-shell risk |
| PT-003 | Mode-aware accessible primary/secondary contrast and contained-button overrides | Axe serious/critical checks across representative viewports/browsers | Theme-only; visual review recommended, but contrast must remain compliant |
| PT-005 | Date-configurable audit packets and parameterized Servant alias expansion | Current packet generated; 40 unresolved rows remain a deliberate gate | Historical packet is preserved; remove new packet only with replacement evidence |
| PT-009 | Shared public shell exclusively owns the main landmark | E2E count assertion and updated directory unit test | Child pages must not restore nested mains while rendered under the shell |
| PT-010 | Added Playwright fixtures/config, artifact policy, cross-browser CI and aggregate quality gating | 15 passed/2 expected project-specific skips locally | CI job can be removed independently, but doing so reopens the coverage gap |
| PT-011 | Logout deactivates all valid bearer/cookie tokens presented before expiring the browser cookie; OpenAPI and generated clients describe the behavior | Focused handler regression plus PostgreSQL stale-cookie replay and token-state assertions | No schema migration; reverting reopens account-session replay risk |
| Persona infrastructure | 26 personas, 78 scenarios, JSON/CSV/Markdown generator, validator and guarded Haskell seed | Persona program tests plus repository quality gate | Seed is opt-in/additive; use a disposable DB rather than attempting destructive record deletion |

No schema migration was necessary. The seed uses existing models and functions. The logout behavior changed without breaking its HTTP path/status, and the OpenAPI description/security alternatives plus both generated clients were updated. GHC 9.10.3 built the backend; the final full Hspec run passed 2,420 examples with 0 failures, and the focused session regression passed 2/2. PostgreSQL migration, seed, idempotency, login, session, logout and token-state checks passed locally.

## Immediate release-blocking gates

1. Preserve the passing opt-in persona seed/idempotency and PT-011 logout-token regression in CI using a disposable PostgreSQL service; never log fixture passwords or tokens.
2. Resolve or deliberately classify all 40 pending endpoint dispositions (PT-004), with handler/action/record-scope tests. The default feature-audit command must pass without the opt-in.
3. Run isolated PostgreSQL rollback and all standalone domain runtime migration scripts touched by the release baseline. Forward backend migrations completed locally, but rollback was not exercised.
4. Keep DDEX import/export/delivery concealed and production-disabled (PT-008). It is not release-ready as a distribution promise.
5. Do not enable payment/provider/refund/reconciliation paths without sandbox verification, webhook/signature tests, idempotency evidence, ledger/database assertions and provider certification.
6. Do not create seed personas outside disposable local/test environments. The runtime guard and secret requirements are mandatory.

## Short-term improvements

| Priority | Work | Impact / reach / value / risk / effort | Acceptance |
|---:|---|---|---|
| 1 | Add backend/API E2E harness for registration, session expiry, reset and profile completion | Very high reach; identity/security; medium effort | UI + API + DB + fake inbox lifecycle, duplicate/expired/direct-request cases |
| 2 | Automate public ticket offer through fake provider event, issuance, cross-device retrieval, replay denial, refund and reconciliation | Revenue/fraud; high effort | One economic effect, authoritative states, fake delivery and full audit trail |
| 3 | Automate public service/studio and Domo lead-to-quote-to-booking-to-finance lifecycles | Revenue/operations; high effort | Availability conflict, quote expiry, fake deposit, completion/refund/reconciliation |
| 4 | Triage npm advisories by runtime reachability | Broad security assurance; medium effort | Patched or owner/date/mitigation documented; no forced blind upgrade |
| 5 | Expand Playwright to public tickets, marketplace, services, courses and Domo | High conversion coverage; medium effort | Critical Chromium/Firefox/WebKit; phone/tablet; axe; request/console assertions |
| 6 | Add iOS Detox fixture login and Android Detox configuration | Mobile reliability/parity; medium/high effort | Stable simulator/emulator selection, no real OAuth, artifact retention, zero retry masking |
| 7 | Complete locale matrix | International conversion/accessibility; medium effort | English and representative Latin American formats, provider/error/catalog fallback |

## Larger redesigns

- Complete private DDEX storage, conflict-safe imports, partner-profiled delivery/acknowledgements and rights/royalty boundaries before exposing distribution success.
- Create a reusable disposable backend test environment with PostgreSQL, local inbox/outbox and fake adapters for payments, OAuth, messaging, WhatsApp, ticket delivery and distribution.
- Decide which web fallbacks justify native investment using demand and task criticality, prioritizing messaging, directory discovery, high-frequency operational work and state-safe commerce continuation.
- Consolidate human-readable cross-domain order/payment/refund/reconciliation timelines without merging domain ownership or treating browser callbacks as authority.

## Future experiments and human research

- Test public terminology and trust cues with real Ecuadorian visitors/customers, then compare international English users.
- Compare guest-first versus early-registration ticket/service checkout without changing financial authority.
- Evaluate whether role labels or task-based navigation better support multi-role creators and staff.
- Test native-to-web transition messaging and return-to-app behavior on real iOS/Android devices.
- Use the [human protocol](human-usability-protocol.md); calculate SUS/UMUX-Lite only from real, consented participant responses.

## Release-readiness conclusions by critical journey

| Journey | Conclusion | Required next evidence |
|---|---|---|
| Registration | **Not demonstrated** | UI mutation, duplicate/invalid data, local verification inbox, DB and audit |
| Authentication/login | **Conditional** | Rejected browser login/redirect and successful PostgreSQL PER-02 password login/backend session authority passed; OAuth, rate limit and broader roles remain |
| Password recovery/session expiry/logout | **Conditional** | Logout now revokes the presented token and rejects stale-cookie replay; expiry, password reset, role-revocation propagation, shared-device browser history and audit remain |
| Profile completion/multi-role | **Not demonstrated** | Ownership and role-composition integration, public/private projection |
| Public discovery | **Conditional** | Isolated city slice/reflow/axe passed; live index breadth, localization and network profiles remain |
| Lead capture/CRM conversion | **Not demonstrated** | Public consent through staff assignment/conversion/audit and duplicate recovery |
| Reservations/studio services | **Not demonstrated** | Real local DB, concurrent availability, hold/order/session completion/cancellation |
| Courses/trials/attendance | **Not demonstrated** | Seat/schedule/guardian/teacher lifecycle and paid enrollment evidence |
| Marketplace sale/rental | **Not demonstrated** | Buyer/seller custody, holds, disputes, cancellation, refund and settlement |
| Ticketing | **Conditional UI/component evidence only** | Public guest offer/hold and mobile cancellation/idempotency pass with mocks; backend capacity/payment/issuance/delivery/validation/refund remains |
| Checkout/payment confirmation | **Not demonstrated end to end** | Datafast/PayPal/Stripe sandbox events, signature/amount/currency/idempotency/database assertions |
| Refunds/reconciliation | **Not demonstrated** | Two-actor controls, provider evidence, ledger and exception resolution |
| Notifications | **Not demonstrated** | Local outbox/inbox, consent, dedupe, delivery failure/retry and audit |
| Domo | **Not demonstrated** | Inquiry, authoritative quote, availability, deposit, booking and follow-up/reconciliation |
| Music distribution | **Blocked / not release-ready** | PT-008 implementation, fake-partner contract, certified external partner and production gate |

## Coverage-gap inventory

- 73 of 78 persona stories have no recorded execution evidence. The five recorded results cover partial slices, not every acceptance step; one is a direct PostgreSQL observation rather than full web/mobile automation.
- Backend source changes compile locally; the final 2,420-example suite and focused 2-example session regression pass. The actual persona seed lifecycle ran twice against disposable PostgreSQL with exact fixture/role/credential assertions.
- Targeted HLint retains seven legacy hints, and the installed Ormolu version proposes broad baseline reformatting beginning with `Seed.hs`; formatting is not green and should be normalized in a dedicated change with an agreed formatter version.
- Authoritative persona party/credential/role/token state and forward backend migration output were observed; domain audit rows, notification outboxes, rollback and financial/inventory state were not.
- No real network throttling, offline browser recovery, large dataset, performance budget or long-running soak was executed.
- Firefox/WebKit run only the critical auth case; commerce paths are not cross-browser.
- Automated axe covers only login/directory serious/critical rules; it cannot establish full accessibility.
- No Android native E2E and no iOS Detox execution.
- The changed logout OpenAPI operation and both web/mobile generated clients were regenerated; full contract completeness beyond that operation was not re-certified.
- No external provider or human participant evidence exists.

## Manual/external gates

Explicit authorization is required before staging accounts/data, provider sandbox transactions that may incur cost, emails/messages/social publishing, ticket delivery, OAuth with live accounts or any distribution submission. Separate production approval and provider certification are required before real payments or releases. Human testing requires consent, privacy handling, accessibility accommodations and the minor safeguards in the protocol.

## Highest-value next actions

1. Move the now-passing disposable PostgreSQL persona seed/logout lifecycle into CI and complete endpoint-disposition work.
2. Stand up a reusable local PostgreSQL + fake-provider environment and automate one ticket journey from public offer through reconciliation.
3. Follow with studio/Domo and marketplace lifecycles because they combine conversion, scarce inventory and financial state.
4. Triage dependency advisories and expand permission/ownership negatives for the 40 pending endpoints.
5. Run the real-participant protocol only after the critical workflows are operational enough to avoid measuring known dead ends.
