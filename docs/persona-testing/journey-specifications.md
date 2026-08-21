# Detailed persona journey specifications

Execution labels are evidence claims: `verified-automated`, `direct-observation`, `specified-not-executed`, `blocked-environment`, `blocked-external`, or `simulated-hypothesis`.

## ST-001 — Register from a protected intent and recover an expired session

- **Epic / feature:** EP-01 — Identity, authentication, onboarding, and sessions · `auth.login`
- **Persona:** PER-01 — Elena Paredes
- **Goal and business value:** Start from a protected call to action, register through the UI, verify locally, let the session expire, recover the account, and return to the original intent. Register from a protected intent and recover an expired session protects every protected conversion depends on reliable identity and recovery..
- **Roles / permission:** None · public then own account
- **Environment:** mobile-web, responsive-web · budget Android phone · es-EC · intermittent 3G with 600 ms latency
- **Accessibility profile:** 200% text zoom
- **Test:** web-e2e · `PW-PER-01-AUTH` · **verified-automated**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-01; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Scenario records carry fixture namespace st-001 and correlation id tdf-persona-st-001.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Elena Paredes (PER-01) is in the isolated initial state with None roles
- **When:** When they start from a protected call to action, register through the UI, verify locally, let the session expire, recover the account, and return to the original intent.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-01 plus scenario namespace ST-001; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Start from a protected call to action, register through the UI, verify locally, let the session expire, recover the account, and return to the original intent.
4. Exercise edge cases: invalid email; duplicate registration; expired reset token; direct protected URL. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-001, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Register from a protected intent and recover an expired session” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-01 change under correlation id tdf-persona-st-001.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-01, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-001 namespaced data in the disposable database.
- Deactivate per-01.elena@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** docs/persona-testing/execution-report.md#pw-per-01-auth Verified protected-intent redirect, rejected login recovery, landmark integrity, serious/critical axe scan, console and request cleanliness. UI registration, verification email, expiry and reset-token lifecycle still require an integrated backend environment.

## ST-002 — Explore Quito and Cuenca content without an account

- **Epic / feature:** EP-03 — Search and discovery · `directory.search`
- **Persona:** PER-01 — Elena Paredes
- **Goal and business value:** Use public navigation and search with city, profession, genre, event, and venue terms without scripted navigation. Explore Quito and Cuenca content without an account protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** None · public read
- **Environment:** mobile-web, responsive-web · budget Android phone · es-EC · intermittent 3G with 600 ms latency
- **Accessibility profile:** 200% text zoom
- **Test:** web-e2e · `PW-PER-01-DISCOVERY` · **verified-automated**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-01; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Scenario records carry fixture namespace st-002 and correlation id tdf-persona-st-002.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Elena Paredes (PER-01) is in the isolated initial state with None roles
- **When:** When they use public navigation and search with city, profession, genre, event, and venue terms without scripted navigation.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-01 plus scenario namespace ST-002; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Use public navigation and search with city, profession, genre, event, and venue terms without scripted navigation.
4. Exercise edge cases: slow response; zero results; stale filter; unknown synonym. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-002, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Explore Quito and Cuenca content without an account” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-01 change under correlation id tdf-persona-st-002.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-01, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-002 namespaced data in the disposable database.
- Deactivate per-01.elena@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** docs/persona-testing/execution-report.md#pw-per-01-discovery Verified city search, state preservation, detail navigation, 320 CSS-pixel reflow and automated axe checks with an isolated API. Profession, genre, event and venue terms against a real local database remain unexecuted.

## ST-003 — Inspect and hold a public ticket without signing in

- **Epic / feature:** EP-08 — Public events, tickets, checkout, issuance, and validation · `public.tickets`
- **Persona:** PER-01 — Elena Paredes
- **Goal and business value:** Open a public event offer, compare tiers, accept versioned terms, create an isolated guest hold, review the server total, and confirm that a hold is neither payment nor issuance. Inspect and hold a public ticket without signing in protects public conversion and fraud-resistant entry depend on authoritative ticket state..
- **Roles / permission:** None · public storefront and capability-scoped guest order
- **Environment:** mobile-web, responsive-web · budget Android phone · es-EC · intermittent 3G with 600 ms latency
- **Accessibility profile:** 200% text zoom
- **Test:** web-e2e · `PW-PER-01-TICKET-OFFER` · **verified-automated**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-01; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Scenario records carry fixture namespace st-003 and correlation id tdf-persona-st-003.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Elena Paredes (PER-01) is in the isolated initial state with None roles
- **When:** When they open a public event offer, compare tiers, accept versioned terms, create an isolated guest hold, review the server total, and confirm that a hold is neither payment nor issuance.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-01 plus scenario namespace ST-003; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open a public event offer, compare tiers, accept versioned terms, create an isolated guest hold, review the server total, and confirm that a hold is neither payment nor issuance.
4. Exercise edge cases: sold-out tier; 200% zoom; back navigation; stale availability. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-003, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Inspect and hold a public ticket without signing in” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-01 change under correlation id tdf-persona-st-003.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-01, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-003 namespaced data in the disposable database.
- Deactivate per-01.elena@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** docs/persona-testing/execution-report.md#pw-per-01-ticket-offer Verified public tier visibility, versioned-term acceptance, idempotent guest hold request, capability-scoped tracker, authoritative server total and explicit unpaid/unissued state with no provider enabled. Sold-out, stale-capacity and back-navigation edge cases remain unexecuted.

## ST-004 — Complete a fan profile and log out safely

- **Epic / feature:** EP-01 — Identity, authentication, onboarding, and sessions · `profile.completion`
- **Persona:** PER-02 — Mateo Ruiz
- **Goal and business value:** Complete required profile fields, select consent preferences, log out, and verify protected state is no longer recoverable from history. Complete a fan profile and log out safely protects every protected conversion depends on reliable identity and recovery..
- **Roles / permission:** Fan, Customer · own profile
- **Environment:** native-ios, responsive-web, native-mobile · iPhone · es-EC · stable 4G
- **Accessibility profile:** reduced motion
- **Test:** web-mobile-e2e · `E2E-PER-02-PROFILE` · **direct-observation**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-02; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- Saved Quito preference
- Scenario records carry fixture namespace st-004 and correlation id tdf-persona-st-004.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Mateo Ruiz (PER-02) is in the isolated initial state with Fan, Customer roles
- **When:** When they complete required profile fields, select consent preferences, log out, and verify protected state is no longer recoverable from history.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-02 plus scenario namespace ST-004; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Complete required profile fields, select consent preferences, log out, and verify protected state is no longer recoverable from history.
4. Exercise edge cases: missing required field; duplicate submit; shared-device back button; expired session. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-004, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Complete a fan profile and log out safely” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-02 change under correlation id tdf-persona-st-004.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-02, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-004 namespaced data in the disposable database.
- Deactivate per-02.mateo@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** docs/persona-testing/execution-report.md#run-020 Verified deterministic account/role seeding, successful password login, backend-composed session, logout token revocation and stale-cookie denial. Profile editing, consent preferences, browser/mobile history, expiry and the full UI journey remain unexecuted.

## ST-005 — Purchase, retrieve, and validate a ticket

- **Epic / feature:** EP-08 — Public events, tickets, checkout, issuance, and validation · `tickets.checkout`
- **Persona:** PER-02 — Mateo Ruiz
- **Goal and business value:** Select a tier, create a hold, complete a sandbox payment, retrieve the issued ticket on another device, and validate it once. Purchase, retrieve, and validate a ticket protects public conversion and fraud-resistant entry depend on authoritative ticket state..
- **Roles / permission:** Fan, Customer · own order; organizer validates
- **Environment:** native-ios, backend-api · iPhone · es-EC · stable 4G
- **Accessibility profile:** reduced motion
- **Test:** api-integration · `API-PER-02-TICKET-LIFECYCLE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-02; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- Saved Quito preference
- Scenario records carry fixture namespace st-005 and correlation id tdf-persona-st-005.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Mateo Ruiz (PER-02) is in the isolated initial state with Fan, Customer roles
- **When:** When they select a tier, create a hold, complete a sandbox payment, retrieve the issued ticket on another device, and validate it once.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-02 plus scenario namespace ST-005; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Select a tier, create a hold, complete a sandbox payment, retrieve the issued ticket on another device, and validate it once.
4. Exercise edge cases: double tap; lost create response; second validation; delivery failure. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-005, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Purchase, retrieve, and validate a ticket” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-02 change under correlation id tdf-persona-st-005.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-02, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-005 namespaced data in the disposable database.
- Deactivate per-02.mateo@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-006 — Recover from a cancelled payment sheet

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `payments.provider-events`
- **Persona:** PER-02 — Mateo Ruiz
- **Goal and business value:** Cancel the sandbox payment sheet, verify the reservation is released, retry with the same intent, and wait for verified provider evidence. Recover from a cancelled payment sheet protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** Fan, Customer · own checkout
- **Environment:** native-ios, native-mobile · iPhone · es-EC · stable 4G
- **Accessibility profile:** reduced motion
- **Test:** mobile-jest · `MOB-PER-02-TICKET-IDEMPOTENCY` · **verified-automated**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-02; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- Saved Quito preference
- Scenario records carry fixture namespace st-006 and correlation id tdf-persona-st-006.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Mateo Ruiz (PER-02) is in the isolated initial state with Fan, Customer roles
- **When:** When they cancel the sandbox payment sheet, verify the reservation is released, retry with the same intent, and wait for verified provider evidence.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-02 plus scenario namespace ST-006; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Cancel the sandbox payment sheet, verify the reservation is released, retry with the same intent, and wait for verified provider evidence.
4. Exercise edge cases: ambiguous client result; duplicate webhook; late webhook; provider timeout. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-006, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Recover from a cancelled payment sheet” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-02 change under correlation id tdf-persona-st-006.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-02, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-006 namespaced data in the disposable database.
- Deactivate per-02.mateo@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** docs/persona-testing/execution-report.md#mob-per-02-ticket-idempotency Verified cancellation release, ambiguous-result pending guidance, idempotency-key reuse/rotation, one-retry bound and privacy of storage keys with mocks. No real or sandbox provider was contacted; verified webhook reconciliation remains an external/integration gate.

## ST-007 — Discover an artist and event entirely in English

- **Epic / feature:** EP-03 — Search and discovery · `public.platform`
- **Persona:** PER-03 — Claire Morgan
- **Goal and business value:** Navigate public home, directory, artist, venue, and event pages in English and retain locale across links. Discover an artist and event entirely in English protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** Fan, Customer · public read
- **Environment:** desktop-web, responsive-web · MacBook · en-CA · stable broadband
- **Accessibility profile:** high contrast, deuteranopia
- **Test:** web-e2e · `PW-PER-03-EN-DISCOVERY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-03; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified English-language account
- Scenario records carry fixture namespace st-007 and correlation id tdf-persona-st-007.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Claire Morgan (PER-03) is in the isolated initial state with Fan, Customer roles
- **When:** When they navigate public home, directory, artist, venue, and event pages in English and retain locale across links.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-CA.

**Steps**

1. Reset and load PER-03 plus scenario namespace ST-007; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Navigate public home, directory, artist, venue, and event pages in English and retain locale across links.
4. Exercise edge cases: missing translation; browser locale mismatch; deep link; empty state. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-007, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Discover an artist and event entirely in English” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-CA, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-03 change under correlation id tdf-persona-st-007.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-03, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-007 namespaced data in the disposable database.
- Deactivate per-03.claire@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-008 — Track a cross-border refund without assuming browser success

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `refunds.customer-status`
- **Persona:** PER-03 — Claire Morgan
- **Goal and business value:** Request cancellation, see pending refund state, receive verified provider update, and confirm final status and audit timeline. Track a cross-border refund without assuming browser success protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** Fan, Customer · own order and refund
- **Environment:** desktop-web, backend-api · MacBook · en-CA · stable broadband
- **Accessibility profile:** high contrast, deuteranopia
- **Test:** backend-integration · `BE-PER-03-REFUND` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-03; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified English-language account
- Scenario records carry fixture namespace st-008 and correlation id tdf-persona-st-008.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Claire Morgan (PER-03) is in the isolated initial state with Fan, Customer roles
- **When:** When they request cancellation, see pending refund state, receive verified provider update, and confirm final status and audit timeline.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-CA.

**Steps**

1. Reset and load PER-03 plus scenario namespace ST-008; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Request cancellation, see pending refund state, receive verified provider update, and confirm final status and audit timeline.
4. Exercise edge cases: browser return only; duplicate callback; provider decline; retry after timeout. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-008, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Track a cross-border refund without assuming browser success” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-CA, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-03 change under correlation id tdf-persona-st-008.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-03, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-008 namespaced data in the disposable database.
- Deactivate per-03.claire@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-009 — Use contrast-safe, non-color status communication

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `localization.accessibility`
- **Persona:** PER-03 — Claire Morgan
- **Goal and business value:** Review order and refund states with high contrast and simulated deuteranopia, using text and icons rather than color alone. Use contrast-safe, non-color status communication protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Fan, Customer · own records
- **Environment:** desktop-web · MacBook · en-CA · stable broadband
- **Accessibility profile:** high contrast, deuteranopia
- **Test:** axe-manual · `A11Y-PER-03-STATUS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-03; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified English-language account
- Scenario records carry fixture namespace st-009 and correlation id tdf-persona-st-009.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Claire Morgan (PER-03) is in the isolated initial state with Fan, Customer roles
- **When:** When they review order and refund states with high contrast and simulated deuteranopia, using text and icons rather than color alone.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-CA.

**Steps**

1. Reset and load PER-03 plus scenario namespace ST-009; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Review order and refund states with high contrast and simulated deuteranopia, using text and icons rather than color alone.
4. Exercise edge cases: forced colors; 200% zoom; reduced motion; English error fallback. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-009, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Use contrast-safe, non-color status communication” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-CA, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-03 change under correlation id tdf-persona-st-009.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-03, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-009 namespaced data in the disposable database.
- Deactivate per-03.claire@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-010 — Complete and submit an artist profile

- **Epic / feature:** EP-02 — Profiles, multi-role identities, portfolios, and public pages · `directory.profiles.manage`
- **Persona:** PER-04 — Valeria Cedeño
- **Goal and business value:** Edit biography, roles, city, genres, portfolio and consent; preview privately; submit; then verify the public projection. Complete and submit an artist profile protects profiles drive discovery while exposing ownership and privacy boundaries..
- **Roles / permission:** Artist, Fan, Customer · own profile
- **Environment:** native-android, responsive-web, backend-api · Android phone · es-EC · variable 4G
- **Accessibility profile:** None
- **Test:** web-api-integration · `INT-PER-04-ARTIST-PROFILE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-04; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft profile
- Fictional EP Neblina de Sal
- Scenario records carry fixture namespace st-010 and correlation id tdf-persona-st-010.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Valeria Cedeño (PER-04) is in the isolated initial state with Artist, Fan, Customer roles
- **When:** When they edit biography, roles, city, genres, portfolio and consent; preview privately; submit; then verify the public projection.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-04 plus scenario namespace ST-010; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Edit biography, roles, city, genres, portfolio and consent; preview privately; submit; then verify the public projection.
4. Exercise edge cases: oversized upload; missing rights consent; duplicate submit; unpublished direct URL. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-010, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Complete and submit an artist profile” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-04 change under correlation id tdf-persona-st-010.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-04, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-010 namespaced data in the disposable database.
- Deactivate per-04.valeria@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-011 — Book a studio session through confirmed completion

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `studio.booking`
- **Persona:** PER-04 — Valeria Cedeño
- **Goal and business value:** Choose service, room and time, create a hold, pay in sandbox, confirm reservation, attend the session, and close the order. Book a studio session through confirmed completion protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** Artist, Fan, Customer · own booking; staff operates
- **Environment:** native-android, backend-api · Android phone · es-EC · variable 4G
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-04-STUDIO-LIFECYCLE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-04; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft profile
- Fictional EP Neblina de Sal
- Scenario records carry fixture namespace st-011 and correlation id tdf-persona-st-011.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Valeria Cedeño (PER-04) is in the isolated initial state with Artist, Fan, Customer roles
- **When:** When they choose service, room and time, create a hold, pay in sandbox, confirm reservation, attend the session, and close the order.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-04 plus scenario namespace ST-011; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Choose service, room and time, create a hold, pay in sandbox, confirm reservation, attend the session, and close the order.
4. Exercise edge cases: conflicting hold; stale quote; payment cancellation; notification failure. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-011, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Book a studio session through confirmed completion” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-04 change under correlation id tdf-persona-st-011.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-04, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-011 namespaced data in the disposable database.
- Deactivate per-04.valeria@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-012 — Submit a fictional EP for distribution validation

- **Epic / feature:** EP-12 — Label, releases, assets, tracks, DDEX, partners, delivery, and status · `label.releases`
- **Persona:** PER-04 — Valeria Cedeño
- **Goal and business value:** Create release, add tracks and contributors, upload fake assets, run validation, correct errors, approve, and stop before external delivery. Submit a fictional EP for distribution validation protects rights, unreleased media, metadata, and external delivery failures carry contractual risk..
- **Roles / permission:** Artist, Fan, Customer · owned release only
- **Environment:** native-android, backend-api · Android phone · es-EC · variable 4G
- **Accessibility profile:** None
- **Test:** contract-integration · `API-PER-04-RELEASE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-04; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft profile
- Fictional EP Neblina de Sal
- Scenario records carry fixture namespace st-012 and correlation id tdf-persona-st-012.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Valeria Cedeño (PER-04) is in the isolated initial state with Artist, Fan, Customer roles
- **When:** When they create release, add tracks and contributors, upload fake assets, run validation, correct errors, approve, and stop before external delivery.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-04 plus scenario namespace ST-012; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create release, add tracks and contributors, upload fake assets, run validation, correct errors, approve, and stop before external delivery.
4. Exercise edge cases: duplicate ISRC; territory conflict; missing rights; provider unavailable. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-012, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Submit a fictional EP for distribution validation” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-04 change under correlation id tdf-persona-st-012.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-04, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-012 namespaced data in the disposable database.
- Deactivate per-04.valeria@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-013 — Compose Artist and Producer capabilities predictably

- **Epic / feature:** EP-02 — Profiles, multi-role identities, portfolios, and public pages · `auth.multi-role`
- **Persona:** PER-05 — Bruno Azevedo
- **Goal and business value:** Sign in once, inspect role-specific destinations, perform an owned artist edit and assigned producer action, then attempt an unrelated record. Compose Artist and Producer capabilities predictably protects profiles drive discovery while exposing ownership and privacy boundaries..
- **Roles / permission:** Artist, Producer, Customer · explicit union with record scope
- **Environment:** tablet-web, backend-api · Android tablet · en-US · stable Wi-Fi
- **Accessibility profile:** keyboard with tablet cover
- **Test:** role-integration · `ROLE-PER-05-COMPOSITION` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-05; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Producer portfolio
- Two private project drafts
- Scenario records carry fixture namespace st-013 and correlation id tdf-persona-st-013.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Bruno Azevedo (PER-05) is in the isolated initial state with Artist, Producer, Customer roles
- **When:** When they sign in once, inspect role-specific destinations, perform an owned artist edit and assigned producer action, then attempt an unrelated record.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-05 plus scenario namespace ST-013; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Sign in once, inspect role-specific destinations, perform an owned artist edit and assigned producer action, then attempt an unrelated record.
4. Exercise edge cases: role revocation mid-session; direct URL; cached menu; object ownership bypass. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-013, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Compose Artist and Producer capabilities predictably” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-05 change under correlation id tdf-persona-st-013.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-05, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-013 namespaced data in the disposable database.
- Deactivate per-05.bruno@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-014 — Find collaborators with persistent filters

- **Epic / feature:** EP-03 — Search and discovery · `directory.search`
- **Persona:** PER-05 — Bruno Azevedo
- **Goal and business value:** Search by city, profession, role and genre; open a result; return; save the search; and continue on another device. Find collaborators with persistent filters protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** Artist, Producer, Customer · public results; own saved search
- **Environment:** tablet-web, responsive-web · Android tablet · en-US · stable Wi-Fi
- **Accessibility profile:** keyboard with tablet cover
- **Test:** web-e2e · `PW-PER-05-COLLAB-SEARCH` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-05; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Producer portfolio
- Two private project drafts
- Scenario records carry fixture namespace st-014 and correlation id tdf-persona-st-014.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Bruno Azevedo (PER-05) is in the isolated initial state with Artist, Producer, Customer roles
- **When:** When they search by city, profession, role and genre; open a result; return; save the search; and continue on another device.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-05 plus scenario namespace ST-014; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Search by city, profession, role and genre; open a result; return; save the search; and continue on another device.
4. Exercise edge cases: accent variation; no exact profession; stale result; offline resume. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-014, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Find collaborators with persistent filters” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-05 change under correlation id tdf-persona-st-014.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-05, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-014 namespaced data in the disposable database.
- Deactivate per-05.bruno@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-015 — Invite a collaborator without exposing private projects

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `social.collaboration`
- **Persona:** PER-05 — Bruno Azevedo
- **Goal and business value:** Create a collaboration request, invite one artist, accept as the counterpart, exchange scoped messages, and close the collaboration. Invite a collaborator without exposing private projects protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** Artist, Producer, Customer · participants only
- **Environment:** tablet-web, backend-api · Android tablet · en-US · stable Wi-Fi
- **Accessibility profile:** keyboard with tablet cover
- **Test:** api-integration · `API-PER-05-COLLAB` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-05; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Producer portfolio
- Two private project drafts
- Scenario records carry fixture namespace st-015 and correlation id tdf-persona-st-015.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Bruno Azevedo (PER-05) is in the isolated initial state with Artist, Producer, Customer roles
- **When:** When they create a collaboration request, invite one artist, accept as the counterpart, exchange scoped messages, and close the collaboration.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-05 plus scenario namespace ST-015; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create a collaboration request, invite one artist, accept as the counterpart, exchange scoped messages, and close the collaboration.
4. Exercise edge cases: self invite; duplicate invite; revoked participant; unrelated project API. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-015, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Invite a collaborator without exposing private projects” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-05 change under correlation id tdf-persona-st-015.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-05, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-015 namespaced data in the disposable database.
- Deactivate per-05.bruno@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-016 — Find a drummer despite terminology differences

- **Epic / feature:** EP-03 — Search and discovery · `directory.search`
- **Persona:** PER-06 — Nicolás Jaramillo
- **Goal and business value:** Search using colloquial and formal Spanish terms, filter by Loja and remote work, and compare result relevance. Find a drummer despite terminology differences protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** Artist, Songwriter, Customer · public read
- **Environment:** desktop-web, responsive-web · Windows laptop · es-EC · slow rural broadband
- **Accessibility profile:** None
- **Test:** web-e2e · `PW-PER-06-SEARCH` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-06; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Public songwriter profile
- Scenario records carry fixture namespace st-016 and correlation id tdf-persona-st-016.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Nicolás Jaramillo (PER-06) is in the isolated initial state with Artist, Songwriter, Customer roles
- **When:** When they search using colloquial and formal Spanish terms, filter by Loja and remote work, and compare result relevance.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-06 plus scenario namespace ST-016; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Search using colloquial and formal Spanish terms, filter by Loja and remote work, and compare result relevance.
4. Exercise edge cases: diacritics; synonym mismatch; empty city; slow suggestions. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-016, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Find a drummer despite terminology differences” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-06 change under correlation id tdf-persona-st-016.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-06, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-016 namespaced data in the disposable database.
- Deactivate per-06.nicolas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-017 — Publish a classified and manage applications

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `directory.classifieds.manage`
- **Persona:** PER-06 — Nicolás Jaramillo
- **Goal and business value:** Create a draft, preview, publish, receive a fictional application, accept one applicant, reject another, and archive the post. Publish a classified and manage applications protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** Artist, Songwriter, Customer · owner manages; applicants see own response
- **Environment:** desktop-web, backend-api · Windows laptop · es-EC · slow rural broadband
- **Accessibility profile:** None
- **Test:** api-integration · `API-PER-06-CLASSIFIED` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-06; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Public songwriter profile
- Scenario records carry fixture namespace st-017 and correlation id tdf-persona-st-017.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Nicolás Jaramillo (PER-06) is in the isolated initial state with Artist, Songwriter, Customer roles
- **When:** When they create a draft, preview, publish, receive a fictional application, accept one applicant, reject another, and archive the post.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-06 plus scenario namespace ST-017; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create a draft, preview, publish, receive a fictional application, accept one applicant, reject another, and archive the post.
4. Exercise edge cases: duplicate submit; expired post; unauthorized edit; reported applicant. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-017, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Publish a classified and manage applications” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-06 change under correlation id tdf-persona-st-017.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-06, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-017 namespaced data in the disposable database.
- Deactivate per-06.nicolas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-018 — Block and report unwanted contact

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `support.reporting`
- **Persona:** PER-06 — Nicolás Jaramillo
- **Goal and business value:** Block a fictional contact, report the interaction with minimal evidence, confirm messaging stops, and inspect recovery guidance. Block and report unwanted contact protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Artist, Songwriter, Customer · own safety controls; moderators only see case
- **Environment:** desktop-web, backend-api · Windows laptop · es-EC · slow rural broadband
- **Accessibility profile:** None
- **Test:** security-integration · `SEC-PER-06-REPORT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-06; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Public songwriter profile
- Scenario records carry fixture namespace st-018 and correlation id tdf-persona-st-018.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Nicolás Jaramillo (PER-06) is in the isolated initial state with Artist, Songwriter, Customer roles
- **When:** When they block a fictional contact, report the interaction with minimal evidence, confirm messaging stops, and inspect recovery guidance.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-06 plus scenario namespace ST-018; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Block a fictional contact, report the interaction with minimal evidence, confirm messaging stops, and inspect recovery guidance.
4. Exercise edge cases: duplicate report; blocked direct API; attachment redaction; report service failure. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-018, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Block and report unwanted contact” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-06 change under correlation id tdf-persona-st-018.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-06, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-018 namespaced data in the disposable database.
- Deactivate per-06.nicolas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-019 — Plan and complete an assigned production session

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `studio.pipeline`
- **Persona:** PER-07 — Camila Viteri
- **Goal and business value:** Convert an assigned lead into a quote, reserve resources, assign staff, complete the session, and close its operational pipeline. Plan and complete an assigned production session protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** Producer, AandR, Customer · assigned CRM and scheduling records
- **Environment:** desktop-web, backend-api · MacBook · es-EC · stable broadband
- **Accessibility profile:** reduced motion
- **Test:** backend-integration · `BE-PER-07-PRODUCTION` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-07; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned fictional lead
- Draft production booking
- Scenario records carry fixture namespace st-019 and correlation id tdf-persona-st-019.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Camila Viteri (PER-07) is in the isolated initial state with Producer, AandR, Customer roles
- **When:** When they convert an assigned lead into a quote, reserve resources, assign staff, complete the session, and close its operational pipeline.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-07 plus scenario namespace ST-019; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Convert an assigned lead into a quote, reserve resources, assign staff, complete the session, and close its operational pipeline.
4. Exercise edge cases: resource conflict; reassignment; stale status; partial session. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-019, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Plan and complete an assigned production session” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-07 change under correlation id tdf-persona-st-019.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-07, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-019 namespaced data in the disposable database.
- Deactivate per-07.camila@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-020 — Qualify and convert an assigned lead

- **Epic / feature:** EP-11 — CRM contacts, companies, leads, activities, assignments, and conversion · `crm.leads`
- **Persona:** PER-07 — Camila Viteri
- **Goal and business value:** Open only an assigned lead, record consented activity, qualify it, convert it to customer and opportunity, and preserve provenance. Qualify and convert an assigned lead protects lead provenance and assignment affect conversion and private contact data..
- **Roles / permission:** Producer, AandR, Customer · assigned lead scope
- **Environment:** desktop-web, backend-api · MacBook · es-EC · stable broadband
- **Accessibility profile:** reduced motion
- **Test:** role-integration · `ROLE-PER-07-LEAD` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-07; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned fictional lead
- Draft production booking
- Scenario records carry fixture namespace st-020 and correlation id tdf-persona-st-020.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Camila Viteri (PER-07) is in the isolated initial state with Producer, AandR, Customer roles
- **When:** When they open only an assigned lead, record consented activity, qualify it, convert it to customer and opportunity, and preserve provenance.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-07 plus scenario namespace ST-020; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open only an assigned lead, record consented activity, qualify it, convert it to customer and opportunity, and preserve provenance.
4. Exercise edge cases: unassigned lead API; duplicate conversion; missing consent; role revocation. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-020, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Qualify and convert an assigned lead” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-07 change under correlation id tdf-persona-st-020.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-07, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-020 namespaced data in the disposable database.
- Deactivate per-07.camila@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-021 — Review a release package as A&R

- **Epic / feature:** EP-12 — Label, releases, assets, tracks, DDEX, partners, delivery, and status · `label.release-validation`
- **Persona:** PER-07 — Camila Viteri
- **Goal and business value:** Inspect contributors, rights and assets; return actionable validation findings; approve a corrected package; and verify history. Review a release package as A&R protects rights, unreleased media, metadata, and external delivery failures carry contractual risk..
- **Roles / permission:** Producer, AandR, Customer · assigned catalog records
- **Environment:** desktop-web, backend-api · MacBook · es-EC · stable broadband
- **Accessibility profile:** reduced motion
- **Test:** api-contract · `API-PER-07-AR-REVIEW` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-07; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned fictional lead
- Draft production booking
- Scenario records carry fixture namespace st-021 and correlation id tdf-persona-st-021.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Camila Viteri (PER-07) is in the isolated initial state with Producer, AandR, Customer roles
- **When:** When they inspect contributors, rights and assets; return actionable validation findings; approve a corrected package; and verify history.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-07 plus scenario namespace ST-021; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Inspect contributors, rights and assets; return actionable validation findings; approve a corrected package; and verify history.
4. Exercise edge cases: missing contract; unauthorized asset URL; stale approval; malformed metadata. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-021, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Review a release package as A&R” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-07 change under correlation id tdf-persona-st-021.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-07, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-021 namespaced data in the disposable database.
- Deactivate per-07.camila@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-022 — Inspect an assigned artist contract with keyboard only

- **Epic / feature:** EP-02 — Profiles, multi-role identities, portfolios, and public pages · `label.artist-contracts`
- **Persona:** PER-08 — Sofía Rojas
- **Goal and business value:** Navigate to an assigned artist and contract with keyboard landmarks, review access history, then attempt an unassigned contract URL. Inspect an assigned artist contract with keyboard only protects profiles drive discovery while exposing ownership and privacy boundaries..
- **Roles / permission:** AandR, LabelRep · assigned label scope
- **Environment:** desktop-web · Windows desktop · es-CO · corporate broadband
- **Accessibility profile:** keyboard-only
- **Test:** a11y-security · `A11Y-PER-08-CONTRACT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-08; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional label Prisma Norte
- Draft release PN-001
- Scenario records carry fixture namespace st-022 and correlation id tdf-persona-st-022.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Sofía Rojas (PER-08) is in the isolated initial state with AandR, LabelRep roles
- **When:** When they navigate to an assigned artist and contract with keyboard landmarks, review access history, then attempt an unassigned contract URL.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-CO.

**Steps**

1. Reset and load PER-08 plus scenario namespace ST-022; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Navigate to an assigned artist and contract with keyboard landmarks, review access history, then attempt an unassigned contract URL.
4. Exercise edge cases: focus loss; expired session; unassigned object; download link replay. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-022, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Inspect an assigned artist contract with keyboard only” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-CO, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-08 change under correlation id tdf-persona-st-022.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-08, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-022 namespaced data in the disposable database.
- Deactivate per-08.sofia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-023 — Validate, import, and deliver a DDEX record

- **Epic / feature:** EP-12 — Label, releases, assets, tracks, DDEX, partners, delivery, and status · `label.ddex`
- **Persona:** PER-08 — Sofía Rojas
- **Goal and business value:** Import a synthetic DDEX document, deduplicate entities, fix validation errors, approve delivery, and use a fake partner adapter. Validate, import, and deliver a DDEX record protects rights, unreleased media, metadata, and external delivery failures carry contractual risk..
- **Roles / permission:** AandR, LabelRep · assigned catalog; delivery approval as configured
- **Environment:** desktop-web, backend-api · Windows desktop · es-CO · corporate broadband
- **Accessibility profile:** keyboard-only
- **Test:** contract-integration · `API-PER-08-DDEX` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-08; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional label Prisma Norte
- Draft release PN-001
- Scenario records carry fixture namespace st-023 and correlation id tdf-persona-st-023.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Sofía Rojas (PER-08) is in the isolated initial state with AandR, LabelRep roles
- **When:** When they import a synthetic DDEX document, deduplicate entities, fix validation errors, approve delivery, and use a fake partner adapter.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-CO.

**Steps**

1. Reset and load PER-08 plus scenario namespace ST-023; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Import a synthetic DDEX document, deduplicate entities, fix validation errors, approve delivery, and use a fake partner adapter.
4. Exercise edge cases: XXE-like payload safely rejected; duplicate import; partner timeout; invalid territory. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-023, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Validate, import, and deliver a DDEX record” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-CO, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-08 change under correlation id tdf-persona-st-023.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-08, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-023 namespaced data in the disposable database.
- Deactivate per-08.sofia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-024 — Confirm finance remains unavailable to A&R

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `finance.label-report`
- **Persona:** PER-08 — Sofía Rojas
- **Goal and business value:** Attempt finance navigation, a direct report URL, and corresponding API endpoints while retaining legitimate catalog access. Confirm finance remains unavailable to A&R protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** AandR, LabelRep · deny finance
- **Environment:** desktop-web, backend-api · Windows desktop · es-CO · corporate broadband
- **Accessibility profile:** keyboard-only
- **Test:** role-integration · `ROLE-PER-08-DENY-FINANCE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-08; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional label Prisma Norte
- Draft release PN-001
- Scenario records carry fixture namespace st-024 and correlation id tdf-persona-st-024.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Sofía Rojas (PER-08) is in the isolated initial state with AandR, LabelRep roles
- **When:** When they attempt finance navigation, a direct report URL, and corresponding API endpoints while retaining legitimate catalog access.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-CO.

**Steps**

1. Reset and load PER-08 plus scenario namespace ST-024; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Attempt finance navigation, a direct report URL, and corresponding API endpoints while retaining legitimate catalog access.
4. Exercise edge cases: guessed ID; cached response; alternate HTTP verb; export endpoint. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-024, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Confirm finance remains unavailable to A&R” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-CO, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-08 change under correlation id tdf-persona-st-024.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-08, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-024 namespaced data in the disposable database.
- Deactivate per-08.sofia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-025 — Publish course availability and manage its lifecycle

- **Epic / feature:** EP-06 — School, courses, trials, registrations, schedules, and attendance · `school.courses`
- **Persona:** PER-09 — Andrés Molina
- **Goal and business value:** Create course schedule, publish seats, receive registrations, teach sessions, record attendance, and close the course. Publish course availability and manage its lifecycle protects seat inventory, schedules, student privacy, and minor consent intersect..
- **Roles / permission:** Teacher, Artist · own/assigned course and students
- **Environment:** tablet-web, backend-api · iPad · es-EC · stable Wi-Fi
- **Accessibility profile:** large touch targets
- **Test:** backend-integration · `BE-PER-09-COURSE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-09; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Guitar Fundamentals course
- Three fictional students
- Scenario records carry fixture namespace st-025 and correlation id tdf-persona-st-025.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Andrés Molina (PER-09) is in the isolated initial state with Teacher, Artist roles
- **When:** When they create course schedule, publish seats, receive registrations, teach sessions, record attendance, and close the course.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-09 plus scenario namespace ST-025; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create course schedule, publish seats, receive registrations, teach sessions, record attendance, and close the course.
4. Exercise edge cases: schedule conflict; seat overbooking; duplicate attendance; cancelled class. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-025, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Publish course availability and manage its lifecycle” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-09 change under correlation id tdf-persona-st-025.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-09, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-025 namespaced data in the disposable database.
- Deactivate per-09.andres@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-026 — Maintain separate teacher and artist profile contexts

- **Epic / feature:** EP-02 — Profiles, multi-role identities, portfolios, and public pages · `profiles.teacher`
- **Persona:** PER-09 — Andrés Molina
- **Goal and business value:** Edit teaching specialties and artist portfolio, preview both public projections, and verify fields do not leak between contexts. Maintain separate teacher and artist profile contexts protects profiles drive discovery while exposing ownership and privacy boundaries..
- **Roles / permission:** Teacher, Artist · own profiles
- **Environment:** tablet-web, backend-api · iPad · es-EC · stable Wi-Fi
- **Accessibility profile:** large touch targets
- **Test:** api-integration · `API-PER-09-MULTIPROFILE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-09; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Guitar Fundamentals course
- Three fictional students
- Scenario records carry fixture namespace st-026 and correlation id tdf-persona-st-026.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Andrés Molina (PER-09) is in the isolated initial state with Teacher, Artist roles
- **When:** When they edit teaching specialties and artist portfolio, preview both public projections, and verify fields do not leak between contexts.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-09 plus scenario namespace ST-026; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Edit teaching specialties and artist portfolio, preview both public projections, and verify fields do not leak between contexts.
4. Exercise edge cases: role removal; unpublished profile; shared upload; direct API patch. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-026, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Maintain separate teacher and artist profile contexts” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-09 change under correlation id tdf-persona-st-026.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-09, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-026 namespaced data in the disposable database.
- Deactivate per-09.andres@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-027 — Restrict teacher access to assigned students

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `school.student-privacy`
- **Persona:** PER-09 — Andrés Molina
- **Goal and business value:** Review assigned roster and attendance, then attempt another teacher's student through search, direct URL, and API. Restrict teacher access to assigned students protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Teacher, Artist · assigned students only
- **Environment:** tablet-web, backend-api · iPad · es-EC · stable Wi-Fi
- **Accessibility profile:** large touch targets
- **Test:** security-integration · `SEC-PER-09-STUDENTS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-09; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Guitar Fundamentals course
- Three fictional students
- Scenario records carry fixture namespace st-027 and correlation id tdf-persona-st-027.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Andrés Molina (PER-09) is in the isolated initial state with Teacher, Artist roles
- **When:** When they review assigned roster and attendance, then attempt another teacher's student through search, direct URL, and API.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-09 plus scenario namespace ST-027; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Review assigned roster and attendance, then attempt another teacher's student through search, direct URL, and API.
4. Exercise edge cases: reassignment; cached roster; export; archived enrollment. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-027, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Restrict teacher access to assigned students” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-09 change under correlation id tdf-persona-st-027.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-09, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-027 namespaced data in the disposable database.
- Deactivate per-09.andres@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-028 — Create a minor account with fictional guardian approval

- **Epic / feature:** EP-01 — Identity, authentication, onboarding, and sessions · `auth.minor-consent`
- **Persona:** PER-10 — Lucía Torres
- **Goal and business value:** Begin registration, provide minimum fictional data, send consent to a local inbox, approve as the fictional guardian, and activate the account. Create a minor account with fictional guardian approval protects every protected conversion depends on reliable identity and recovery..
- **Roles / permission:** Student, Customer · pending account then consented own account
- **Environment:** mobile-web, responsive-web, backend-api · shared Android phone · es-EC · prepaid 4G
- **Accessibility profile:** None
- **Test:** web-api-integration · `INT-PER-10-CONSENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-10; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional guardian consent pending
- Scenario records carry fixture namespace st-028 and correlation id tdf-persona-st-028.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Lucía Torres (PER-10) is in the isolated initial state with Student, Customer roles
- **When:** When they begin registration, provide minimum fictional data, send consent to a local inbox, approve as the fictional guardian, and activate the account.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-10 plus scenario namespace ST-028; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Begin registration, provide minimum fictional data, send consent to a local inbox, approve as the fictional guardian, and activate the account.
4. Exercise edge cases: no consent; expired consent link; duplicate guardian response; age boundary. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-028, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Create a minor account with fictional guardian approval” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-10 change under correlation id tdf-persona-st-028.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-10, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-028 namespaced data in the disposable database.
- Deactivate per-10.lucia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-029 — Request and attend a trial lesson

- **Epic / feature:** EP-06 — School, courses, trials, registrations, schedules, and attendance · `school.trials`
- **Persona:** PER-10 — Lucía Torres
- **Goal and business value:** Browse a public course, request an available trial, obtain guardian approval, schedule with a teacher, attend, and convert to enrollment. Request and attend a trial lesson protects seat inventory, schedules, student privacy, and minor consent intersect..
- **Roles / permission:** Student, Customer · own consented trial; assigned teacher
- **Environment:** mobile-web, backend-api · shared Android phone · es-EC · prepaid 4G
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-10-TRIAL` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-10; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional guardian consent pending
- Scenario records carry fixture namespace st-029 and correlation id tdf-persona-st-029.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Lucía Torres (PER-10) is in the isolated initial state with Student, Customer roles
- **When:** When they browse a public course, request an available trial, obtain guardian approval, schedule with a teacher, attend, and convert to enrollment.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-10 plus scenario namespace ST-029; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Browse a public course, request an available trial, obtain guardian approval, schedule with a teacher, attend, and convert to enrollment.
4. Exercise edge cases: slot conflict; seat hold expiry; duplicate conversion; teacher cancellation. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-029, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Request and attend a trial lesson” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-10 change under correlation id tdf-persona-st-029.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-10, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-029 namespaced data in the disposable database.
- Deactivate per-10.lucia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-030 — Protect a minor on a shared device

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `auth.logout`
- **Persona:** PER-10 — Lucía Torres
- **Goal and business value:** Sign out, navigate browser history, reopen the app, and verify sensitive schedule and message data require fresh authentication. Protect a minor on a shared device protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Student, Customer · deny after logout
- **Environment:** mobile-web · shared Android phone · es-EC · prepaid 4G
- **Accessibility profile:** None
- **Test:** security-e2e · `SEC-PER-10-SHARED-DEVICE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-10; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional guardian consent pending
- Scenario records carry fixture namespace st-030 and correlation id tdf-persona-st-030.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Lucía Torres (PER-10) is in the isolated initial state with Student, Customer roles
- **When:** When they sign out, navigate browser history, reopen the app, and verify sensitive schedule and message data require fresh authentication.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-10 plus scenario namespace ST-030; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Sign out, navigate browser history, reopen the app, and verify sensitive schedule and message data require fresh authentication.
4. Exercise edge cases: offline cache; stale service worker; refresh token; direct API. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-030, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Protect a minor on a shared device” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-10 change under correlation id tdf-persona-st-030.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-10, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-030 namespaced data in the disposable database.
- Deactivate per-10.lucia@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-031 — Complete and resubmit an assigned internship task

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `internships.tasks`
- **Persona:** PER-11 — Martina Salazar
- **Goal and business value:** Open an assigned task, save an interrupted draft, submit fake evidence, receive revision feedback, resubmit, and close it. Complete and resubmit an assigned internship task protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** Intern · assigned internship record
- **Environment:** desktop-web, backend-api · Chromebook · es-EC · shared Wi-Fi
- **Accessibility profile:** None
- **Test:** api-integration · `API-PER-11-INTERNSHIP` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-11; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Active fictional placement
- Two assigned tasks
- Scenario records carry fixture namespace st-031 and correlation id tdf-persona-st-031.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Martina Salazar (PER-11) is in the isolated initial state with Intern roles
- **When:** When they open an assigned task, save an interrupted draft, submit fake evidence, receive revision feedback, resubmit, and close it.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-11 plus scenario namespace ST-031; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open an assigned task, save an interrupted draft, submit fake evidence, receive revision feedback, resubmit, and close it.
4. Exercise edge cases: duplicate submit; oversized evidence; unassigned task; offline recovery. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-031, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Complete and resubmit an assigned internship task” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-11 change under correlation id tdf-persona-st-031.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-11, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-031 namespaced data in the disposable database.
- Deactivate per-11.martina@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-032 — View operational context without changing inventory

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `operations.assignments`
- **Persona:** PER-11 — Martina Salazar
- **Goal and business value:** Open an assignment-linked equipment record, inspect safe context, and attempt create, edit, delete, and export operations. View operational context without changing inventory protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** Intern · read only when assignment grants it
- **Environment:** desktop-web, backend-api · Chromebook · es-EC · shared Wi-Fi
- **Accessibility profile:** None
- **Test:** role-integration · `ROLE-PER-11-OPS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-11; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Active fictional placement
- Two assigned tasks
- Scenario records carry fixture namespace st-032 and correlation id tdf-persona-st-032.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Martina Salazar (PER-11) is in the isolated initial state with Intern roles
- **When:** When they open an assignment-linked equipment record, inspect safe context, and attempt create, edit, delete, and export operations.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-11 plus scenario namespace ST-032; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open an assignment-linked equipment record, inspect safe context, and attempt create, edit, delete, and export operations.
4. Exercise edge cases: direct API; alternate verb; guessed ID; role change. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-032, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “View operational context without changing inventory” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-11 change under correlation id tdf-persona-st-032.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-11, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-032 namespaced data in the disposable database.
- Deactivate per-11.martina@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-033 — Reject intern access to administration

- **Epic / feature:** EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations · `admin.users`
- **Persona:** PER-11 — Martina Salazar
- **Goal and business value:** Attempt menu discovery, search, direct admin route, user API, diagnostics, and integration settings. Reject intern access to administration protects administrative errors can affect every user and must be backend-enforced..
- **Roles / permission:** Intern · deny and conceal
- **Environment:** desktop-web, backend-api · Chromebook · es-EC · shared Wi-Fi
- **Accessibility profile:** None
- **Test:** role-integration · `ROLE-PER-11-ADMIN-DENY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-11; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Active fictional placement
- Two assigned tasks
- Scenario records carry fixture namespace st-033 and correlation id tdf-persona-st-033.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Martina Salazar (PER-11) is in the isolated initial state with Intern roles
- **When:** When they attempt menu discovery, search, direct admin route, user API, diagnostics, and integration settings.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-11 plus scenario namespace ST-033; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Attempt menu discovery, search, direct admin route, user API, diagnostics, and integration settings.
4. Exercise edge cases: cached menu; deep link; GraphQL not applicable; error information leak. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-033, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Reject intern access to administration” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-11 change under correlation id tdf-persona-st-033.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-11, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-033 namespaced data in the disposable database.
- Deactivate per-11.martina@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-034 — Create a customer booking and hand it to operations

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `studio.booking`
- **Persona:** PER-12 — Karla Benítez
- **Goal and business value:** Find/create customer, quote an available service, place a reservation, collect no raw card data, confirm sandbox evidence, and hand off the session. Create a customer booking and hand it to operations protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** Reception · CRM and scheduling; no finance approval
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-heavy
- **Test:** backend-integration · `BE-PER-12-RECEPTION-BOOKING` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-12; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned shift
- Fictional caller lead
- Scenario records carry fixture namespace st-034 and correlation id tdf-persona-st-034.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Karla Benítez (PER-12) is in the isolated initial state with Reception roles
- **When:** When they find/create customer, quote an available service, place a reservation, collect no raw card data, confirm sandbox evidence, and hand off the session.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-12 plus scenario namespace ST-034; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Find/create customer, quote an available service, place a reservation, collect no raw card data, confirm sandbox evidence, and hand off the session.
4. Exercise edge cases: duplicate call; room conflict; lost provider response; customer correction. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-034, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Create a customer booking and hand it to operations” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-12 change under correlation id tdf-persona-st-034.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-12, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-034 namespaced data in the disposable database.
- Deactivate per-12.karla@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-035 — Capture and convert a public inquiry

- **Epic / feature:** EP-11 — CRM contacts, companies, leads, activities, assignments, and conversion · `crm.leads`
- **Persona:** PER-12 — Karla Benítez
- **Goal and business value:** Receive a fictional public inquiry, verify consent/source, assign activity, qualify it, convert it, and preserve the public attribution. Capture and convert a public inquiry protects lead provenance and assignment affect conversion and private contact data..
- **Roles / permission:** Reception · CRM records
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-heavy
- **Test:** api-integration · `API-PER-12-LEAD` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-12; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned shift
- Fictional caller lead
- Scenario records carry fixture namespace st-035 and correlation id tdf-persona-st-035.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Karla Benítez (PER-12) is in the isolated initial state with Reception roles
- **When:** When they receive a fictional public inquiry, verify consent/source, assign activity, qualify it, convert it, and preserve the public attribution.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-12 plus scenario namespace ST-035; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Receive a fictional public inquiry, verify consent/source, assign activity, qualify it, convert it, and preserve the public attribution.
4. Exercise edge cases: duplicate webhook; missing consent; invalid phone; reassignment. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-035, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Capture and convert a public inquiry” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-12 change under correlation id tdf-persona-st-035.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-12, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-035 namespaced data in the disposable database.
- Deactivate per-12.karla@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-036 — Trace a lead through booking and reconciliation

- **Epic / feature:** EP-17 — Public-to-operational-to-financial cross-epic lifecycles · `lead-to-reconciliation`
- **Persona:** PER-12 — Karla Benítez
- **Goal and business value:** Follow one correlation ID from public lead through quote, booking, payment evidence, session completion, invoice, and accounting reconciliation. Trace a lead through booking and reconciliation protects revenue is realized only when public intent reaches operations and reconciliation without state gaps..
- **Roles / permission:** Reception · stage-specific actors
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-heavy
- **Test:** cross-epic-integration · `INT-PER-12-LEAD-REVENUE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-12; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Assigned shift
- Fictional caller lead
- Scenario records carry fixture namespace st-036 and correlation id tdf-persona-st-036.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Karla Benítez (PER-12) is in the isolated initial state with Reception roles
- **When:** When they follow one correlation ID from public lead through quote, booking, payment evidence, session completion, invoice, and accounting reconciliation.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-12 plus scenario namespace ST-036; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Follow one correlation ID from public lead through quote, booking, payment evidence, session completion, invoice, and accounting reconciliation.
4. Exercise edge cases: partial conversion; notification failure; refund; stale operational status. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-036, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Trace a lead through booking and reconciliation” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-12 change under correlation id tdf-persona-st-036.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-12, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-036 namespaced data in the disposable database.
- Deactivate per-12.karla@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-037 — Resolve a room conflict with an audited override

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `studio.calendar`
- **Persona:** PER-13 — Fernando Lema
- **Goal and business value:** Detect overlapping holds, reject unsafe overwrite, select an alternative or enter an authorized reasoned override, notify parties, and close the session. Resolve a room conflict with an audited override protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** StudioManager · studio management
- **Environment:** desktop-web, backend-api · desktop workstation · es-EC · stable LAN
- **Accessibility profile:** 150% zoom
- **Test:** backend-integration · `BE-PER-13-CONFLICT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-13; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Two rooms
- One conflicting hold
- Unassigned session
- Scenario records carry fixture namespace st-037 and correlation id tdf-persona-st-037.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Fernando Lema (PER-13) is in the isolated initial state with StudioManager roles
- **When:** When they detect overlapping holds, reject unsafe overwrite, select an alternative or enter an authorized reasoned override, notify parties, and close the session.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-13 plus scenario namespace ST-037; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Detect overlapping holds, reject unsafe overwrite, select an alternative or enter an authorized reasoned override, notify parties, and close the session.
4. Exercise edge cases: stale calendar; simultaneous update; missing reason; notification outage. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-037, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Resolve a room conflict with an audited override” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-13 change under correlation id tdf-persona-st-037.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-13, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-037 namespaced data in the disposable database.
- Deactivate per-13.fernando@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-038 — Assign staff and equipment through return

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `operations.assignments`
- **Persona:** PER-13 — Fernando Lema
- **Goal and business value:** Assign available staff and equipment, record checkout, reassign a conflict, record return condition, and close custody. Assign staff and equipment through return protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** StudioManager · studio operations
- **Environment:** desktop-web, backend-api · desktop workstation · es-EC · stable LAN
- **Accessibility profile:** 150% zoom
- **Test:** backend-integration · `BE-PER-13-ASSIGNMENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-13; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Two rooms
- One conflicting hold
- Unassigned session
- Scenario records carry fixture namespace st-038 and correlation id tdf-persona-st-038.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Fernando Lema (PER-13) is in the isolated initial state with StudioManager roles
- **When:** When they assign available staff and equipment, record checkout, reassign a conflict, record return condition, and close custody.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-13 plus scenario namespace ST-038; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Assign available staff and equipment, record checkout, reassign a conflict, record return condition, and close custody.
4. Exercise edge cases: maintenance block; double assignment; late return; missing evidence. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-038, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Assign staff and equipment through return” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-13 change under correlation id tdf-persona-st-038.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-13, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-038 namespaced data in the disposable database.
- Deactivate per-13.fernando@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-039 — Reconcile operational and revenue reports

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `finance.reports`
- **Persona:** PER-13 — Fernando Lema
- **Goal and business value:** Compare session, booking, invoice, provider evidence and refund records, investigate a mismatch, reconcile it, and export a sanitized report. Reconcile operational and revenue reports protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** StudioManager · manager report scope
- **Environment:** desktop-web, backend-api · desktop workstation · es-EC · stable LAN
- **Accessibility profile:** 150% zoom
- **Test:** integration · `INT-PER-13-REPORT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-13; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Two rooms
- One conflicting hold
- Unassigned session
- Scenario records carry fixture namespace st-039 and correlation id tdf-persona-st-039.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Fernando Lema (PER-13) is in the isolated initial state with StudioManager roles
- **When:** When they compare session, booking, invoice, provider evidence and refund records, investigate a mismatch, reconcile it, and export a sanitized report.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-13 plus scenario namespace ST-039; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Compare session, booking, invoice, provider evidence and refund records, investigate a mismatch, reconcile it, and export a sanitized report.
4. Exercise edge cases: unverified event; duplicate payment; partial refund; export failure. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-039, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Reconcile operational and revenue reports” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-13 change under correlation id tdf-persona-st-039.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-13, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-039 namespaced data in the disposable database.
- Deactivate per-13.fernando@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-040 — Block damaged equipment until verified repair

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `operations.maintenance`
- **Persona:** PER-14 — Óscar Guamán
- **Goal and business value:** Report damage with fake evidence, change item to unavailable, reject new reservations, complete repair, verify condition, and restore availability. Block damaged equipment until verified repair protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** Maintenance · maintenance-owned operations
- **Environment:** native-android, native-mobile, backend-api · rugged Android phone · es-EC · dead zones inside studio
- **Accessibility profile:** protanopia, large touch targets
- **Test:** mobile-api-integration · `INT-PER-14-MAINTENANCE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-14; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Microphone EQ-TEST-014
- Open maintenance task
- Scenario records carry fixture namespace st-040 and correlation id tdf-persona-st-040.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Óscar Guamán (PER-14) is in the isolated initial state with Maintenance roles
- **When:** When they report damage with fake evidence, change item to unavailable, reject new reservations, complete repair, verify condition, and restore availability.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-14 plus scenario namespace ST-040; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Report damage with fake evidence, change item to unavailable, reject new reservations, complete repair, verify condition, and restore availability.
4. Exercise edge cases: offline draft; duplicate report; unauthorized restore; existing future booking. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-040, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Block damaged equipment until verified repair” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-14 change under correlation id tdf-persona-st-040.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-14, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-040 namespaced data in the disposable database.
- Deactivate per-14.oscar@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-041 — Prevent booking unsafe inventory

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `studio.availability`
- **Persona:** PER-14 — Óscar Guamán
- **Goal and business value:** Attempt to reserve a service requiring blocked equipment from UI and direct API, then choose a safe alternative and complete the reservation. Prevent booking unsafe inventory protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** Maintenance · read scheduling context; no customer order ownership
- **Environment:** native-android, backend-api · rugged Android phone · es-EC · dead zones inside studio
- **Accessibility profile:** protanopia, large touch targets
- **Test:** api-integration · `API-PER-14-AVAILABILITY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-14; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Microphone EQ-TEST-014
- Open maintenance task
- Scenario records carry fixture namespace st-041 and correlation id tdf-persona-st-041.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Óscar Guamán (PER-14) is in the isolated initial state with Maintenance roles
- **When:** When they attempt to reserve a service requiring blocked equipment from UI and direct API, then choose a safe alternative and complete the reservation.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-14 plus scenario namespace ST-041; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Attempt to reserve a service requiring blocked equipment from UI and direct API, then choose a safe alternative and complete the reservation.
4. Exercise edge cases: stale availability; concurrent unblock; cached quote; partial resource set. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-041, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Prevent booking unsafe inventory” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-14 change under correlation id tdf-persona-st-041.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-14, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-041 namespaced data in the disposable database.
- Deactivate per-14.oscar@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-042 — Recover a maintenance draft after connectivity loss

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `mobile.resilience`
- **Persona:** PER-14 — Óscar Guamán
- **Goal and business value:** Enter a condition report, lose connectivity before submission, reopen, review preserved data, submit once, and confirm status without color dependence. Recover a maintenance draft after connectivity loss protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Maintenance · own draft and assigned item
- **Environment:** native-android, native-mobile · rugged Android phone · es-EC · dead zones inside studio
- **Accessibility profile:** protanopia, large touch targets
- **Test:** detox · `DTX-PER-14-OFFLINE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-14; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Microphone EQ-TEST-014
- Open maintenance task
- Scenario records carry fixture namespace st-042 and correlation id tdf-persona-st-042.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Óscar Guamán (PER-14) is in the isolated initial state with Maintenance roles
- **When:** When they enter a condition report, lose connectivity before submission, reopen, review preserved data, submit once, and confirm status without color dependence.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-14 plus scenario namespace ST-042; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Enter a condition report, lose connectivity before submission, reopen, review preserved data, submit once, and confirm status without color dependence.
4. Exercise edge cases: process restart; duplicate retry; large attachment; protanopia. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-042, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Recover a maintenance draft after connectivity loss” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-14 change under correlation id tdf-persona-st-042.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-14, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-042 namespaced data in the disposable database.
- Deactivate per-14.oscar@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-043 — Preview and publish bilingual CMS content

- **Epic / feature:** EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations · `admin.cms`
- **Persona:** PER-15 — Paula Espinosa
- **Goal and business value:** Draft Spanish and English content, preview without public visibility, validate links, publish once, inspect audit, and roll back to the prior version. Preview and publish bilingual CMS content protects administrative errors can affect every user and must be backend-enforced..
- **Roles / permission:** Webmaster · CMS administration
- **Environment:** desktop-web, responsive-web, backend-api · Linux laptop · es-EC · stable broadband
- **Accessibility profile:** keyboard-only
- **Test:** web-api-integration · `INT-PER-15-CMS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-15; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft bilingual announcement
- Scenario records carry fixture namespace st-043 and correlation id tdf-persona-st-043.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Paula Espinosa (PER-15) is in the isolated initial state with Webmaster roles
- **When:** When they draft Spanish and English content, preview without public visibility, validate links, publish once, inspect audit, and roll back to the prior version.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-15 plus scenario namespace ST-043; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Draft Spanish and English content, preview without public visibility, validate links, publish once, inspect audit, and roll back to the prior version.
4. Exercise edge cases: missing translation; duplicate publish; stale edit; broken link. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-043, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Preview and publish bilingual CMS content” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-15 change under correlation id tdf-persona-st-043.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-15, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-043 namespaced data in the disposable database.
- Deactivate per-15.paula@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-044 — Inspect diagnostics without exposing secrets

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `admin.diagnostics`
- **Persona:** PER-15 — Paula Espinosa
- **Goal and business value:** Open status and integration diagnostics, search rendered content and API payloads for secret-shaped values, and verify actionable but safe errors. Inspect diagnostics without exposing secrets protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Webmaster · webmaster diagnostics only
- **Environment:** desktop-web, backend-api · Linux laptop · es-EC · stable broadband
- **Accessibility profile:** keyboard-only
- **Test:** security-integration · `SEC-PER-15-DIAGNOSTICS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-15; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft bilingual announcement
- Scenario records carry fixture namespace st-044 and correlation id tdf-persona-st-044.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Paula Espinosa (PER-15) is in the isolated initial state with Webmaster roles
- **When:** When they open status and integration diagnostics, search rendered content and API payloads for secret-shaped values, and verify actionable but safe errors.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-15 plus scenario namespace ST-044; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open status and integration diagnostics, search rendered content and API payloads for secret-shaped values, and verify actionable but safe errors.
4. Exercise edge cases: provider error body; stack trace; token query string; downloaded log. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-044, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Inspect diagnostics without exposing secrets” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-15 change under correlation id tdf-persona-st-044.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-15, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-044 namespaced data in the disposable database.
- Deactivate per-15.paula@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-045 — Verify published content across public entry points

- **Epic / feature:** EP-03 — Search and discovery · `public.content`
- **Persona:** PER-15 — Paula Espinosa
- **Goal and business value:** Open the published item from home, search and direct URL in both locales and representative viewports, then verify rollback consistency. Verify published content across public entry points protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** Webmaster · public read
- **Environment:** desktop-web, responsive-web · Linux laptop · es-EC · stable broadband
- **Accessibility profile:** keyboard-only
- **Test:** web-e2e · `PW-PER-15-PUBLIC-CMS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-15; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft bilingual announcement
- Scenario records carry fixture namespace st-045 and correlation id tdf-persona-st-045.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Paula Espinosa (PER-15) is in the isolated initial state with Webmaster roles
- **When:** When they open the published item from home, search and direct URL in both locales and representative viewports, then verify rollback consistency.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-15 plus scenario namespace ST-045; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open the published item from home, search and direct URL in both locales and representative viewports, then verify rollback consistency.
4. Exercise edge cases: CDN stale state; missing locale; mobile viewport; offline cache. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-045, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Verify published content across public entry points” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-15 change under correlation id tdf-persona-st-045.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-15, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-045 namespaced data in the disposable database.
- Deactivate per-15.paula@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-046 — Assign and revoke multi-role access

- **Epic / feature:** EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations · `admin.users`
- **Persona:** PER-16 — Irene Cárdenas
- **Goal and business value:** Assign fictional roles, verify exact modules, revoke one role, expire sessions, verify denial in UI/direct URL/API, and inspect the audit trail. Assign and revoke multi-role access protects administrative errors can affect every user and must be backend-enforced..
- **Roles / permission:** Admin · strict administrator
- **Environment:** desktop-web, backend-api · MacBook · es-EC · stable broadband
- **Accessibility profile:** screen reader orientation
- **Test:** role-integration · `ROLE-PER-16-ROLE-LIFECYCLE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-16; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional suspended user
- Failed sandbox webhook
- Scenario records carry fixture namespace st-046 and correlation id tdf-persona-st-046.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Irene Cárdenas (PER-16) is in the isolated initial state with Admin roles
- **When:** When they assign fictional roles, verify exact modules, revoke one role, expire sessions, verify denial in UI/direct URL/API, and inspect the audit trail.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-16 plus scenario namespace ST-046; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Assign fictional roles, verify exact modules, revoke one role, expire sessions, verify denial in UI/direct URL/API, and inspect the audit trail.
4. Exercise edge cases: self-demotion; duplicate role; stale token; strict-admin ambiguity. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-046, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Assign and revoke multi-role access” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-16 change under correlation id tdf-persona-st-046.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-16, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-046 namespaced data in the disposable database.
- Deactivate per-16.irene@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-047 — Replay a failed sandbox provider event safely

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `admin.provider-events`
- **Persona:** PER-16 — Irene Cárdenas
- **Goal and business value:** Inspect an encrypted dedupe record, replay one failed sandbox event, prevent duplicate effects, and reconcile the target order. Replay a failed sandbox provider event safely protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** Admin · strict admin or configured finance operator
- **Environment:** desktop-web, backend-api · MacBook · es-EC · stable broadband
- **Accessibility profile:** screen reader orientation
- **Test:** backend-integration · `BE-PER-16-PROVIDER-REPLAY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-16; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional suspended user
- Failed sandbox webhook
- Scenario records carry fixture namespace st-047 and correlation id tdf-persona-st-047.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Irene Cárdenas (PER-16) is in the isolated initial state with Admin roles
- **When:** When they inspect an encrypted dedupe record, replay one failed sandbox event, prevent duplicate effects, and reconcile the target order.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-16 plus scenario namespace ST-047; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Inspect an encrypted dedupe record, replay one failed sandbox event, prevent duplicate effects, and reconcile the target order.
4. Exercise edge cases: tampered signature; duplicate event; out-of-order event; browser return without webhook. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-047, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Replay a failed sandbox provider event safely” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-16 change under correlation id tdf-persona-st-047.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-16, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-047 namespaced data in the disposable database.
- Deactivate per-16.irene@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-048 — Review security events with screen-reader semantics

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `admin.audit`
- **Persona:** PER-16 — Irene Cárdenas
- **Goal and business value:** Navigate audit filters, status and detail dialogs using screen-reader landmarks and keyboard, export a sanitized subset, and close focus correctly. Review security events with screen-reader semantics protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Admin · strict administrator
- **Environment:** desktop-web · MacBook · es-EC · stable broadband
- **Accessibility profile:** screen reader orientation
- **Test:** axe-manual · `A11Y-PER-16-AUDIT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-16; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional suspended user
- Failed sandbox webhook
- Scenario records carry fixture namespace st-048 and correlation id tdf-persona-st-048.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Irene Cárdenas (PER-16) is in the isolated initial state with Admin roles
- **When:** When they navigate audit filters, status and detail dialogs using screen-reader landmarks and keyboard, export a sanitized subset, and close focus correctly.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-16 plus scenario namespace ST-048; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Navigate audit filters, status and detail dialogs using screen-reader landmarks and keyboard, export a sanitized subset, and close focus correctly.
4. Exercise edge cases: empty state; large result; redacted field; modal focus return. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-048, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Review security events with screen-reader semantics” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-16 change under correlation id tdf-persona-st-048.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-16, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-048 namespaced data in the disposable database.
- Deactivate per-16.irene@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-049 — Claim and verify a fictional venue page

- **Epic / feature:** EP-02 — Profiles, multi-role identities, portfolios, and public pages · `directory.venue-claim`
- **Persona:** PER-17 — Ana Beltrán
- **Goal and business value:** Submit a venue claim with fake evidence, keep changes private during review, approve as moderator, update owned details, and revoke the claim. Claim and verify a fictional venue page protects profiles drive discovery while exposing ownership and privacy boundaries..
- **Roles / permission:** Vendor, Customer · claimant then verified owner
- **Environment:** mobile-web, backend-api · iPhone · es-EC · stable 4G
- **Accessibility profile:** None
- **Test:** api-integration · `API-PER-17-VENUE-CLAIM` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-17; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unverified venue claim
- Two availability blocks
- Scenario records carry fixture namespace st-049 and correlation id tdf-persona-st-049.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Ana Beltrán (PER-17) is in the isolated initial state with Vendor, Customer roles
- **When:** When they submit a venue claim with fake evidence, keep changes private during review, approve as moderator, update owned details, and revoke the claim.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-17 plus scenario namespace ST-049; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Submit a venue claim with fake evidence, keep changes private during review, approve as moderator, update owned details, and revoke the claim.
4. Exercise edge cases: duplicate claim; other owner; rejected evidence; direct edit before approval. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-049, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Claim and verify a fictional venue page” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-17 change under correlation id tdf-persona-st-049.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-17, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-049 namespaced data in the disposable database.
- Deactivate per-17.ana@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-050 — Publish discoverable, accurate venue availability

- **Epic / feature:** EP-03 — Search and discovery · `directory.venue.public`
- **Persona:** PER-17 — Ana Beltrán
- **Goal and business value:** Set public attributes and non-sensitive availability, discover by city/service, open the venue detail, and verify private contact data stays hidden. Publish discoverable, accurate venue availability protects discoverability is the entry to audience, lead, and revenue journeys..
- **Roles / permission:** Vendor, Customer · owner writes; public projection reads
- **Environment:** mobile-web, responsive-web, backend-api · iPhone · es-EC · stable 4G
- **Accessibility profile:** None
- **Test:** web-api-integration · `INT-PER-17-VENUE-DISCOVERY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-17; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unverified venue claim
- Two availability blocks
- Scenario records carry fixture namespace st-050 and correlation id tdf-persona-st-050.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Ana Beltrán (PER-17) is in the isolated initial state with Vendor, Customer roles
- **When:** When they set public attributes and non-sensitive availability, discover by city/service, open the venue detail, and verify private contact data stays hidden.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-17 plus scenario namespace ST-050; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Set public attributes and non-sensitive availability, discover by city/service, open the venue detail, and verify private contact data stays hidden.
4. Exercise edge cases: stale index; unpublished venue; empty availability; private field query. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-050, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Publish discoverable, accurate venue availability” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-17 change under correlation id tdf-persona-st-050.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-17, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-050 namespaced data in the disposable database.
- Deactivate per-17.ana@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-051 — Respond to a venue-service inquiry

- **Epic / feature:** EP-07 — Marketplace sales, rentals, availability, tracking, cancellations, and disputes · `marketplace.vendor`
- **Persona:** PER-17 — Ana Beltrán
- **Goal and business value:** Receive a fictional inquiry, quote availability, accept a booking request, track order, handle cancellation, and close follow-up. Respond to a venue-service inquiry protects money, inventory, custody, deposits, and multi-party ownership must remain consistent..
- **Roles / permission:** Vendor, Customer · owned vendor records
- **Environment:** mobile-web, backend-api · iPhone · es-EC · stable 4G
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-17-VENDOR` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-17; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unverified venue claim
- Two availability blocks
- Scenario records carry fixture namespace st-051 and correlation id tdf-persona-st-051.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Ana Beltrán (PER-17) is in the isolated initial state with Vendor, Customer roles
- **When:** When they receive a fictional inquiry, quote availability, accept a booking request, track order, handle cancellation, and close follow-up.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-17 plus scenario namespace ST-051; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Receive a fictional inquiry, quote availability, accept a booking request, track order, handle cancellation, and close follow-up.
4. Exercise edge cases: overlap; expired quote; unauthorized buyer edit; dispute. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-051, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Respond to a venue-service inquiry” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-17 change under correlation id tdf-persona-st-051.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-17, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-051 namespaced data in the disposable database.
- Deactivate per-17.ana@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-052 — Rent equipment through custody and return

- **Epic / feature:** EP-07 — Marketplace sales, rentals, availability, tracking, cancellations, and disputes · `marketplace.rentals`
- **Persona:** PER-18 — Marco Quispe
- **Goal and business value:** Search availability, request dates, accept quote, fund sandbox deposit, record handoff, return with condition evidence, and close the rental. Rent equipment through custody and return protects money, inventory, custody, deposits, and multi-party ownership must remain consistent..
- **Roles / permission:** Customer · own rental; seller owns counterpart
- **Environment:** mobile-web, backend-api · Android phone · es-PE · roaming 4G
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-18-RENTAL` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-18; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified customer
- Fictional saved rental dates
- Scenario records carry fixture namespace st-052 and correlation id tdf-persona-st-052.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Marco Quispe (PER-18) is in the isolated initial state with Customer roles
- **When:** When they search availability, request dates, accept quote, fund sandbox deposit, record handoff, return with condition evidence, and close the rental.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-PE.

**Steps**

1. Reset and load PER-18 plus scenario namespace ST-052; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Search availability, request dates, accept quote, fund sandbox deposit, record handoff, return with condition evidence, and close the rental.
4. Exercise edge cases: unavailable dates; duplicate request; late return; damage dispute. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-052, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Rent equipment through custody and return” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-PE, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-18 change under correlation id tdf-persona-st-052.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-18, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-052 namespaced data in the disposable database.
- Deactivate per-18.marco@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-053 — Separate deposit authorization from final charge

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `payments.deposit`
- **Persona:** PER-18 — Marco Quispe
- **Goal and business value:** Authorize a fake deposit, verify it is not reported as revenue, complete return, release or partially capture through verified evidence, and reconcile. Separate deposit authorization from final charge protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** Customer · own status; finance executes controls
- **Environment:** mobile-web · Android phone · es-PE · roaming 4G
- **Accessibility profile:** None
- **Test:** provider-contract · `CONTRACT-PER-18-DEPOSIT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-18; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified customer
- Fictional saved rental dates
- Scenario records carry fixture namespace st-053 and correlation id tdf-persona-st-053.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Marco Quispe (PER-18) is in the isolated initial state with Customer roles
- **When:** When they authorize a fake deposit, verify it is not reported as revenue, complete return, release or partially capture through verified evidence, and reconcile.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-PE.

**Steps**

1. Reset and load PER-18 plus scenario namespace ST-053; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Authorize a fake deposit, verify it is not reported as revenue, complete return, release or partially capture through verified evidence, and reconcile.
4. Exercise edge cases: provider timeout; duplicate capture; release failure; browser-only success. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-053, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Separate deposit authorization from final charge” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-PE, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-18 change under correlation id tdf-persona-st-053.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-18, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-053 namespaced data in the disposable database.
- Deactivate per-18.marco@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-054 — Track deposit release and refund

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `refunds.customer-status`
- **Persona:** PER-18 — Marco Quispe
- **Goal and business value:** Follow pending, submitted, provider-confirmed and reconciled states with timestamps and support guidance. Track deposit release and refund protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** Customer · own financial status
- **Environment:** mobile-web, backend-api · Android phone · es-PE · roaming 4G
- **Accessibility profile:** None
- **Test:** api-integration · `API-PER-18-REFUND-STATUS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-18; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified customer
- Fictional saved rental dates
- Scenario records carry fixture namespace st-054 and correlation id tdf-persona-st-054.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Marco Quispe (PER-18) is in the isolated initial state with Customer roles
- **When:** When they follow pending, submitted, provider-confirmed and reconciled states with timestamps and support guidance.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-PE.

**Steps**

1. Reset and load PER-18 plus scenario namespace ST-054; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Follow pending, submitted, provider-confirmed and reconciled states with timestamps and support guidance.
4. Exercise edge cases: partial capture; late webhook; mismatched amount; support escalation. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-054, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Track deposit release and refund” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-PE, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-18 change under correlation id tdf-persona-st-054.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-18, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-054 namespaced data in the disposable database.
- Deactivate per-18.marco@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-055 — Create and operate an equipment listing

- **Epic / feature:** EP-07 — Marketplace sales, rentals, availability, tracking, cancellations, and disputes · `marketplace.listings`
- **Persona:** PER-19 — Rosa Andrade
- **Goal and business value:** Draft listing, upload fake images, publish, receive request, approve, hand off, receive return, settle dispute if needed, and archive. Create and operate an equipment listing protects money, inventory, custody, deposits, and multi-party ownership must remain consistent..
- **Roles / permission:** Vendor, Customer · owned listing and requests
- **Environment:** tablet-web, backend-api · Android tablet · es-EC · home Wi-Fi
- **Accessibility profile:** 150% zoom
- **Test:** backend-integration · `BE-PER-19-LISTING` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-19; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional interface listing
- Pending rental request
- Scenario records carry fixture namespace st-055 and correlation id tdf-persona-st-055.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Rosa Andrade (PER-19) is in the isolated initial state with Vendor, Customer roles
- **When:** When they draft listing, upload fake images, publish, receive request, approve, hand off, receive return, settle dispute if needed, and archive.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-19 plus scenario namespace ST-055; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Draft listing, upload fake images, publish, receive request, approve, hand off, receive return, settle dispute if needed, and archive.
4. Exercise edge cases: missing condition; duplicate publish; other seller edit; active-order archive. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-055, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Create and operate an equipment listing” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-19 change under correlation id tdf-persona-st-055.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-19, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-055 namespaced data in the disposable database.
- Deactivate per-19.rosa@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-056 — Preserve immutable handoff evidence

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `marketplace.custody`
- **Persona:** PER-19 — Rosa Andrade
- **Goal and business value:** Capture fake condition evidence and mutual acknowledgement at handoff and return, then verify later edits append rather than replace history. Preserve immutable handoff evidence protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** Vendor, Customer · rental parties; audit append only
- **Environment:** tablet-web, backend-api · Android tablet · es-EC · home Wi-Fi
- **Accessibility profile:** 150% zoom
- **Test:** api-integration · `API-PER-19-CUSTODY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-19; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional interface listing
- Pending rental request
- Scenario records carry fixture namespace st-056 and correlation id tdf-persona-st-056.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Rosa Andrade (PER-19) is in the isolated initial state with Vendor, Customer roles
- **When:** When they capture fake condition evidence and mutual acknowledgement at handoff and return, then verify later edits append rather than replace history.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-19 plus scenario namespace ST-056; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Capture fake condition evidence and mutual acknowledgement at handoff and return, then verify later edits append rather than replace history.
4. Exercise edge cases: missing acknowledgement; offline duplicate; clock skew; attachment removal. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-056, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Preserve immutable handoff evidence” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-19 change under correlation id tdf-persona-st-056.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-19, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-056 namespaced data in the disposable database.
- Deactivate per-19.rosa@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-057 — Understand seller settlement and cancellation

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `marketplace.settlement`
- **Persona:** PER-19 — Rosa Andrade
- **Goal and business value:** Cancel within and outside policy, inspect fee/refund split, complete a clean rental, and reconcile seller settlement to provider evidence. Understand seller settlement and cancellation protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** Vendor, Customer · own settlement; finance reconciles
- **Environment:** tablet-web, backend-api · Android tablet · es-EC · home Wi-Fi
- **Accessibility profile:** 150% zoom
- **Test:** integration · `INT-PER-19-SETTLEMENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-19; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional interface listing
- Pending rental request
- Scenario records carry fixture namespace st-057 and correlation id tdf-persona-st-057.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Rosa Andrade (PER-19) is in the isolated initial state with Vendor, Customer roles
- **When:** When they cancel within and outside policy, inspect fee/refund split, complete a clean rental, and reconcile seller settlement to provider evidence.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-19 plus scenario namespace ST-057; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Cancel within and outside policy, inspect fee/refund split, complete a clean rental, and reconcile seller settlement to provider evidence.
4. Exercise edge cases: partial refund; chargeback simulation; duplicate settlement; policy version change. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-057, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Understand seller settlement and cancellation” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-19 change under correlation id tdf-persona-st-057.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-19, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-057 namespaced data in the disposable database.
- Deactivate per-19.rosa@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-058 — Publish and operate a fictional event

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `social.events`
- **Persona:** PER-20 — Javier Mena
- **Goal and business value:** Draft event, assign venue and collaborators, validate required details, publish, update, cancel one occurrence, and archive after completion. Publish and operate a fictional event protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** Promoter, Producer, Customer · owned event and assigned collaborators
- **Environment:** desktop-web, backend-api · Windows laptop · es-EC · stable broadband
- **Accessibility profile:** None
- **Test:** api-integration · `API-PER-20-EVENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-20; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft event
- Two ticket tiers
- Scenario records carry fixture namespace st-058 and correlation id tdf-persona-st-058.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Javier Mena (PER-20) is in the isolated initial state with Promoter, Producer, Customer roles
- **When:** When they draft event, assign venue and collaborators, validate required details, publish, update, cancel one occurrence, and archive after completion.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-20 plus scenario namespace ST-058; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Draft event, assign venue and collaborators, validate required details, publish, update, cancel one occurrence, and archive after completion.
4. Exercise edge cases: missing end time; venue conflict; stale edit; cancelled-event cache. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-058, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Publish and operate a fictional event” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-20 change under correlation id tdf-persona-st-058.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-20, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-058 namespaced data in the disposable database.
- Deactivate per-20.javier@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-059 — Issue and validate tickets exactly once

- **Epic / feature:** EP-08 — Public events, tickets, checkout, issuance, and validation · `tickets.validation`
- **Persona:** PER-20 — Javier Mena
- **Goal and business value:** Publish tiers, sell sandbox tickets, deliver through a fake adapter, validate at entry, reject replay, refund one ticket, and close sales. Issue and validate tickets exactly once protects public conversion and fraud-resistant entry depend on authoritative ticket state..
- **Roles / permission:** Promoter, Producer, Customer · owned event operations
- **Environment:** desktop-web, backend-api · Windows laptop · es-EC · stable broadband
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-20-TICKETS` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-20; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft event
- Two ticket tiers
- Scenario records carry fixture namespace st-059 and correlation id tdf-persona-st-059.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Javier Mena (PER-20) is in the isolated initial state with Promoter, Producer, Customer roles
- **When:** When they publish tiers, sell sandbox tickets, deliver through a fake adapter, validate at entry, reject replay, refund one ticket, and close sales.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-20 plus scenario namespace ST-059; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Publish tiers, sell sandbox tickets, deliver through a fake adapter, validate at entry, reject replay, refund one ticket, and close sales.
4. Exercise edge cases: oversell; QR replay; offline validator; refunded ticket. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-059, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Issue and validate tickets exactly once” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-20 change under correlation id tdf-persona-st-059.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-20, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-059 namespaced data in the disposable database.
- Deactivate per-20.javier@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-060 — Reconcile event orders, refunds, and payout

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `finance.event-reconciliation`
- **Persona:** PER-20 — Javier Mena
- **Goal and business value:** Compare issued and validated tickets to orders and verified provider events, resolve mismatch, account for refund, and produce a sanitized closeout. Reconcile event orders, refunds, and payout protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** Promoter, Producer, Customer · owned summary; finance reconciliation
- **Environment:** desktop-web, backend-api · Windows laptop · es-EC · stable broadband
- **Accessibility profile:** None
- **Test:** cross-epic-integration · `INT-PER-20-EVENT-RECON` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-20; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Draft event
- Two ticket tiers
- Scenario records carry fixture namespace st-060 and correlation id tdf-persona-st-060.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Javier Mena (PER-20) is in the isolated initial state with Promoter, Producer, Customer roles
- **When:** When they compare issued and validated tickets to orders and verified provider events, resolve mismatch, account for refund, and produce a sanitized closeout.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-20 plus scenario namespace ST-060; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Compare issued and validated tickets to orders and verified provider events, resolve mismatch, account for refund, and produce a sanitized closeout.
4. Exercise edge cases: late webhook; cash not modeled; partial refund; duplicate event. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-060, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Reconcile event orders, refunds, and payout” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-20 change under correlation id tdf-persona-st-060.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-20, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-060 namespaced data in the disposable database.
- Deactivate per-20.javier@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-061 — Deliver a synthetic release and correct a rejection

- **Epic / feature:** EP-12 — Label, releases, assets, tracks, DDEX, partners, delivery, and status · `label.delivery`
- **Persona:** PER-21 — Gabriela Flores
- **Goal and business value:** Complete release metadata, validate fake assets, approve, deliver through a fake partner, receive rejection, correct one field, redeliver idempotently, and reach terminal status. Deliver a synthetic release and correct a rejection protects rights, unreleased media, metadata, and external delivery failures carry contractual risk..
- **Roles / permission:** LabelRep, Customer · owned label catalog
- **Environment:** desktop-web, backend-api · MacBook · es-MX · stable broadband
- **Accessibility profile:** None
- **Test:** contract-integration · `API-PER-21-DELIVERY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-21; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional artist Luz Cobalto
- Draft release FI-2026-01
- Scenario records carry fixture namespace st-061 and correlation id tdf-persona-st-061.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Gabriela Flores (PER-21) is in the isolated initial state with LabelRep, Customer roles
- **When:** When they complete release metadata, validate fake assets, approve, deliver through a fake partner, receive rejection, correct one field, redeliver idempotently, and reach terminal status.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-MX.

**Steps**

1. Reset and load PER-21 plus scenario namespace ST-061; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Complete release metadata, validate fake assets, approve, deliver through a fake partner, receive rejection, correct one field, redeliver idempotently, and reach terminal status.
4. Exercise edge cases: duplicate track; partner timeout; out-of-order status; territory change. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-061, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Deliver a synthetic release and correct a rejection” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-MX, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-21 change under correlation id tdf-persona-st-061.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-21, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-061 namespaced data in the disposable database.
- Deactivate per-21.gabriela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-062 — Pay a distribution order without duplicate charge

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `distribution.checkout`
- **Persona:** PER-21 — Gabriela Flores
- **Goal and business value:** Create quote and order, cancel once, retry using an idempotency key, wait for verified sandbox provider event, and reconcile the service entitlement. Pay a distribution order without duplicate charge protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** LabelRep, Customer · own order
- **Environment:** desktop-web, backend-api · MacBook · es-MX · stable broadband
- **Accessibility profile:** None
- **Test:** backend-integration · `BE-PER-21-DIST-PAYMENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-21; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional artist Luz Cobalto
- Draft release FI-2026-01
- Scenario records carry fixture namespace st-062 and correlation id tdf-persona-st-062.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Gabriela Flores (PER-21) is in the isolated initial state with LabelRep, Customer roles
- **When:** When they create quote and order, cancel once, retry using an idempotency key, wait for verified sandbox provider event, and reconcile the service entitlement.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-MX.

**Steps**

1. Reset and load PER-21 plus scenario namespace ST-062; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create quote and order, cancel once, retry using an idempotency key, wait for verified sandbox provider event, and reconcile the service entitlement.
4. Exercise edge cases: lost response; duplicate webhook; amount mismatch; payment method decline. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-062, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Pay a distribution order without duplicate charge” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-MX, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-21 change under correlation id tdf-persona-st-062.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-21, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-062 namespaced data in the disposable database.
- Deactivate per-21.gabriela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-063 — Import partner metadata without cross-label leakage

- **Epic / feature:** EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations · `label.partner-import`
- **Persona:** PER-21 — Gabriela Flores
- **Goal and business value:** Import a synthetic partner file twice, merge deterministic identities, inspect only owned assets, and reject another label's identifiers. Import partner metadata without cross-label leakage protects administrative errors can affect every user and must be backend-enforced..
- **Roles / permission:** LabelRep, Customer · owned label scope
- **Environment:** desktop-web · MacBook · es-MX · stable broadband
- **Accessibility profile:** None
- **Test:** security-contract · `SEC-PER-21-PARTNER` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-21; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional artist Luz Cobalto
- Draft release FI-2026-01
- Scenario records carry fixture namespace st-063 and correlation id tdf-persona-st-063.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Gabriela Flores (PER-21) is in the isolated initial state with LabelRep, Customer roles
- **When:** When they import a synthetic partner file twice, merge deterministic identities, inspect only owned assets, and reject another label's identifiers.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-MX.

**Steps**

1. Reset and load PER-21 plus scenario namespace ST-063; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Import a synthetic partner file twice, merge deterministic identities, inspect only owned assets, and reject another label's identifiers.
4. Exercise edge cases: duplicate import; malformed file; guessed asset URL; role revocation. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-063, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Import partner metadata without cross-label leakage” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-MX, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-21 change under correlation id tdf-persona-st-063.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-21, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-063 namespaced data in the disposable database.
- Deactivate per-21.gabriela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-064 — Reconcile an unmatched verified provider event

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `payments.reconciliation`
- **Persona:** PER-22 — Luis Vallejo
- **Goal and business value:** Inspect sanitized sandbox evidence, match it to one order, replay safely, prove no duplicate financial effect, and close the exception. Reconcile an unmatched verified provider event protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** Accounting · finance reconciliation
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-only
- **Test:** backend-integration · `BE-PER-22-RECONCILE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-22; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unmatched sandbox payment
- Refund awaiting approval
- Scenario records carry fixture namespace st-064 and correlation id tdf-persona-st-064.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Luis Vallejo (PER-22) is in the isolated initial state with Accounting roles
- **When:** When they inspect sanitized sandbox evidence, match it to one order, replay safely, prove no duplicate financial effect, and close the exception.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-22 plus scenario namespace ST-064; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Inspect sanitized sandbox evidence, match it to one order, replay safely, prove no duplicate financial effect, and close the exception.
4. Exercise edge cases: invalid signature; amount mismatch; duplicate event; out-of-order event. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-064, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Reconcile an unmatched verified provider event” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-22 change under correlation id tdf-persona-st-064.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-22, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-064 namespaced data in the disposable database.
- Deactivate per-22.luis@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-065 — Prepare and approve a refund with dual control

- **Epic / feature:** EP-14 — Finance, reports, refunds, reconciliation, and audit trails · `refunds.dual-control`
- **Persona:** PER-22 — Luis Vallejo
- **Goal and business value:** Prepare a refund, prevent self-approval, approve as an independent authorized actor, call only a sandbox/fake adapter, and reconcile final evidence. Prepare and approve a refund with dual control protects financial state requires verified evidence, dual control, and durable auditability..
- **Roles / permission:** Accounting · finance preparation; separate approver
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-only
- **Test:** security-integration · `SEC-PER-22-REFUND` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-22; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unmatched sandbox payment
- Refund awaiting approval
- Scenario records carry fixture namespace st-065 and correlation id tdf-persona-st-065.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Luis Vallejo (PER-22) is in the isolated initial state with Accounting roles
- **When:** When they prepare a refund, prevent self-approval, approve as an independent authorized actor, call only a sandbox/fake adapter, and reconcile final evidence.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-22 plus scenario namespace ST-065; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Prepare a refund, prevent self-approval, approve as an independent authorized actor, call only a sandbox/fake adapter, and reconcile final evidence.
4. Exercise edge cases: self approval; duplicate approval; provider failure; amount exceeds paid. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-065, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Prepare and approve a refund with dual control” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-22 change under correlation id tdf-persona-st-065.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-22, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-065 namespaced data in the disposable database.
- Deactivate per-22.luis@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-066 — Export a minimal audited finance report

- **Epic / feature:** EP-15 — Administration, roles, settings, CMS, diagnostics, and integrations · `finance.exports`
- **Persona:** PER-22 — Luis Vallejo
- **Goal and business value:** Filter a test period, export sanitized financial records, verify access audit and retention guidance, and attempt customer administration. Export a minimal audited finance report protects administrative errors can affect every user and must be backend-enforced..
- **Roles / permission:** Accounting · finance export; deny admin users
- **Environment:** desktop-web, backend-api · Windows desktop · es-EC · stable LAN
- **Accessibility profile:** keyboard-only
- **Test:** security-integration · `SEC-PER-22-EXPORT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-22; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Unmatched sandbox payment
- Refund awaiting approval
- Scenario records carry fixture namespace st-066 and correlation id tdf-persona-st-066.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Luis Vallejo (PER-22) is in the isolated initial state with Accounting roles
- **When:** When they filter a test period, export sanitized financial records, verify access audit and retention guidance, and attempt customer administration.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-22 plus scenario namespace ST-066; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Filter a test period, export sanitized financial records, verify access audit and retention guidance, and attempt customer administration.
4. Exercise edge cases: formula injection; large export; expired session; unauthorized profile API. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-066, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Export a minimal audited finance report” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-22 change under correlation id tdf-persona-st-066.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-22, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-066 namespaced data in the disposable database.
- Deactivate per-22.luis@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-067 — Create a Live Session through publish readiness

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `live-sessions`
- **Persona:** PER-23 — Renata Paz
- **Goal and business value:** Create session, book room, assign guests and staff, attach fake consent, complete production, approve edit, publish, and close reporting. Create a Live Session through publish readiness protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** LiveSessionsProducer, Producer · assigned Live Session
- **Environment:** tablet-web, backend-api · iPad · es-EC · studio Wi-Fi
- **Accessibility profile:** reduced motion
- **Test:** backend-integration · `BE-PER-23-LIVE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-23; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional session Andean Frequencies
- Unassigned camera
- Scenario records carry fixture namespace st-067 and correlation id tdf-persona-st-067.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Renata Paz (PER-23) is in the isolated initial state with LiveSessionsProducer, Producer roles
- **When:** When they create session, book room, assign guests and staff, attach fake consent, complete production, approve edit, publish, and close reporting.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-23 plus scenario namespace ST-067; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Create session, book room, assign guests and staff, attach fake consent, complete production, approve edit, publish, and close reporting.
4. Exercise edge cases: missing consent; room conflict; unapproved edit; failed notification. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-067, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Create a Live Session through publish readiness” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-23 change under correlation id tdf-persona-st-067.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-23, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-067 namespaced data in the disposable database.
- Deactivate per-23.renata@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-068 — Coordinate session collaborators safely

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `social.collaboration`
- **Persona:** PER-23 — Renata Paz
- **Goal and business value:** Invite fictional collaborators, track accept/decline, send scoped updates, remove one participant, and verify old access is revoked. Coordinate session collaborators safely protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** LiveSessionsProducer, Producer · session participants
- **Environment:** tablet-web, backend-api · iPad · es-EC · studio Wi-Fi
- **Accessibility profile:** reduced motion
- **Test:** api-integration · `API-PER-23-COLLAB` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-23; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional session Andean Frequencies
- Unassigned camera
- Scenario records carry fixture namespace st-068 and correlation id tdf-persona-st-068.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Renata Paz (PER-23) is in the isolated initial state with LiveSessionsProducer, Producer roles
- **When:** When they invite fictional collaborators, track accept/decline, send scoped updates, remove one participant, and verify old access is revoked.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-23 plus scenario namespace ST-068; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Invite fictional collaborators, track accept/decline, send scoped updates, remove one participant, and verify old access is revoked.
4. Exercise edge cases: duplicate invite; expired invite; removed participant API; message failure. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-068, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Coordinate session collaborators safely” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-23 change under correlation id tdf-persona-st-068.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-23, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-068 namespaced data in the disposable database.
- Deactivate per-23.renata@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-069 — Allocate production equipment and staff

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `operations.assignments`
- **Persona:** PER-23 — Renata Paz
- **Goal and business value:** Check availability, reserve camera and room, assign crew, resolve conflict, record custody, return assets, and close assignments. Allocate production equipment and staff protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** LiveSessionsProducer, Producer · assigned production resources
- **Environment:** tablet-web, backend-api · iPad · es-EC · studio Wi-Fi
- **Accessibility profile:** reduced motion
- **Test:** backend-integration · `BE-PER-23-RESOURCES` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-23; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional session Andean Frequencies
- Unassigned camera
- Scenario records carry fixture namespace st-069 and correlation id tdf-persona-st-069.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Renata Paz (PER-23) is in the isolated initial state with LiveSessionsProducer, Producer roles
- **When:** When they check availability, reserve camera and room, assign crew, resolve conflict, record custody, return assets, and close assignments.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-23 plus scenario namespace ST-069; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Check availability, reserve camera and room, assign crew, resolve conflict, record custody, return assets, and close assignments.
4. Exercise edge cases: maintenance block; double booking; late return; stale assignment. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-069, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Allocate production equipment and staff” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-23 change under correlation id tdf-persona-st-069.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-23, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-069 namespaced data in the disposable database.
- Deactivate per-23.renata@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-070 — Complete public discovery and recovery with screen reader and keyboard

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `accessibility.public`
- **Persona:** PER-24 — Eva Williams
- **Goal and business value:** Navigate landmarks and headings, search, open details, trigger and correct errors, reach help, and return focus to the initiating control. Complete public discovery and recovery with screen reader and keyboard protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** Customer, Fan · public and own account
- **Environment:** desktop-web · Windows laptop with NVDA · en-US · stable broadband
- **Accessibility profile:** screen reader, keyboard-only
- **Test:** axe-manual · `A11Y-PER-24-PUBLIC` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-24; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- No stored payment instrument
- Scenario records carry fixture namespace st-070 and correlation id tdf-persona-st-070.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Eva Williams (PER-24) is in the isolated initial state with Customer, Fan roles
- **When:** When they navigate landmarks and headings, search, open details, trigger and correct errors, reach help, and return focus to the initiating control.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-24 plus scenario namespace ST-070; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Navigate landmarks and headings, search, open details, trigger and correct errors, reach help, and return focus to the initiating control.
4. Exercise edge cases: missing accessible name; focus trap; unannounced error; pointer-only control. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-070, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Complete public discovery and recovery with screen reader and keyboard” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-24 change under correlation id tdf-persona-st-070.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-24, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-070 namespaced data in the disposable database.
- Deactivate per-24.eva@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-071 — Book a service without pointer input

- **Epic / feature:** EP-05 — Studio services, reservations, orders, sessions, and Live Sessions · `services.checkout`
- **Persona:** PER-24 — Eva Williams
- **Goal and business value:** Select service and time, enter delivery details, review price, submit sandbox checkout, recover from an inline error, and retrieve confirmation. Book a service without pointer input protects scheduling conflicts and incomplete payment state directly affect operations and revenue..
- **Roles / permission:** Customer, Fan · own booking
- **Environment:** desktop-web, responsive-web · Windows laptop with NVDA · en-US · stable broadband
- **Accessibility profile:** screen reader, keyboard-only
- **Test:** web-e2e · `PW-PER-24-SERVICE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-24; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- No stored payment instrument
- Scenario records carry fixture namespace st-071 and correlation id tdf-persona-st-071.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Eva Williams (PER-24) is in the isolated initial state with Customer, Fan roles
- **When:** When they select service and time, enter delivery details, review price, submit sandbox checkout, recover from an inline error, and retrieve confirmation.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-24 plus scenario namespace ST-071; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Select service and time, enter delivery details, review price, submit sandbox checkout, recover from an inline error, and retrieve confirmation.
4. Exercise edge cases: keyboard date picker; live error; payment modal focus; slow API. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-071, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Book a service without pointer input” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-24 change under correlation id tdf-persona-st-071.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-24, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-071 namespaced data in the disposable database.
- Deactivate per-24.eva@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-072 — Register for a course with accessible status feedback

- **Epic / feature:** EP-06 — School, courses, trials, registrations, schedules, and attendance · `school.public-courses`
- **Persona:** PER-24 — Eva Williams
- **Goal and business value:** Browse courses, inspect schedule and seats, register, resolve validation error, confirm seat hold, and retrieve registration status. Register for a course with accessible status feedback protects seat inventory, schedules, student privacy, and minor consent intersect..
- **Roles / permission:** Customer, Fan · public view; own registration
- **Environment:** desktop-web, responsive-web · Windows laptop with NVDA · en-US · stable broadband
- **Accessibility profile:** screen reader, keyboard-only
- **Test:** web-e2e · `PW-PER-24-COURSE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-24; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Verified account
- No stored payment instrument
- Scenario records carry fixture namespace st-072 and correlation id tdf-persona-st-072.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Eva Williams (PER-24) is in the isolated initial state with Customer, Fan roles
- **When:** When they browse courses, inspect schedule and seats, register, resolve validation error, confirm seat hold, and retrieve registration status.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in en-US.

**Steps**

1. Reset and load PER-24 plus scenario namespace ST-072; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Browse courses, inspect schedule and seats, register, resolve validation error, confirm seat hold, and retrieve registration status.
4. Exercise edge cases: sold out; hold expiry; status color; modal focus. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-072, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Register for a course with accessible status feedback” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for en-US, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-24 change under correlation id tdf-persona-st-072.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-24, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-072 namespaced data in the disposable database.
- Deactivate per-24.eva@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-073 — Accept assignment and complete equipment custody

- **Epic / feature:** EP-13 — Inventory, equipment bookings, assignments, maintenance, and custody · `operations.custody`
- **Persona:** PER-25 — Tomás León
- **Goal and business value:** Accept assigned case, verify contents, record fake handoff, work offline, record late return, sync once, and close custody. Accept assignment and complete equipment custody protects availability and custody must prevent unsafe or conflicting operations..
- **Roles / permission:** RoadCrew, Maintenance · assigned operational record
- **Environment:** native-android, native-mobile, backend-api · Android phone · es-EC · intermittent event Wi-Fi
- **Accessibility profile:** glove-friendly large controls
- **Test:** detox-api · `DTX-PER-25-CUSTODY` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-25; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional road-case assignment
- Offline handoff draft
- Scenario records carry fixture namespace st-073 and correlation id tdf-persona-st-073.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Tomás León (PER-25) is in the isolated initial state with RoadCrew, Maintenance roles
- **When:** When they accept assigned case, verify contents, record fake handoff, work offline, record late return, sync once, and close custody.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-25 plus scenario namespace ST-073; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Accept assigned case, verify contents, record fake handoff, work offline, record late return, sync once, and close custody.
4. Exercise edge cases: duplicate sync; wrong assignee; clock skew; missing item. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-073, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Accept assignment and complete equipment custody” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-25 change under correlation id tdf-persona-st-073.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-25, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-073 namespaced data in the disposable database.
- Deactivate per-25.tomas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-074 — Receive only relevant event-day updates

- **Epic / feature:** EP-04 — Community, contacts, messaging, events, and collaboration · `events.assignments`
- **Persona:** PER-25 — Tomás León
- **Goal and business value:** Open assigned event, acknowledge call time, receive fake operational notification, reject access to attendee details, and complete assignment. Receive only relevant event-day updates protects multi-user interactions create moderation and isolation risks..
- **Roles / permission:** RoadCrew, Maintenance · assigned operational context only
- **Environment:** native-android, native-mobile, backend-api · Android phone · es-EC · intermittent event Wi-Fi
- **Accessibility profile:** glove-friendly large controls
- **Test:** mobile-integration · `MOB-PER-25-EVENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-25; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional road-case assignment
- Offline handoff draft
- Scenario records carry fixture namespace st-074 and correlation id tdf-persona-st-074.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Tomás León (PER-25) is in the isolated initial state with RoadCrew, Maintenance roles
- **When:** When they open assigned event, acknowledge call time, receive fake operational notification, reject access to attendee details, and complete assignment.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-25 plus scenario namespace ST-074; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Open assigned event, acknowledge call time, receive fake operational notification, reject access to attendee details, and complete assignment.
4. Exercise edge cases: unassigned event; duplicate notification; revoked assignment; offline acknowledgement. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-074, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Receive only relevant event-day updates” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-25 change under correlation id tdf-persona-st-074.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-25, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-074 namespaced data in the disposable database.
- Deactivate per-25.tomas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-075 — Use large controls under poor connectivity

- **Epic / feature:** EP-16 — Accessibility, localization, privacy, help, and recovery · `mobile.accessibility`
- **Persona:** PER-25 — Tomás León
- **Goal and business value:** Navigate assignment and custody screens with large touch targets, visible text status and intermittent connectivity without losing entered state. Use large controls under poor connectivity protects inclusive, comprehensible recovery determines whether journeys are independently completable..
- **Roles / permission:** RoadCrew, Maintenance · assigned records
- **Environment:** native-android, native-mobile · Android phone · es-EC · intermittent event Wi-Fi
- **Accessibility profile:** glove-friendly large controls
- **Test:** detox-manual · `DTX-PER-25-RESILIENCE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-25; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Fictional road-case assignment
- Offline handoff draft
- Scenario records carry fixture namespace st-075 and correlation id tdf-persona-st-075.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Tomás León (PER-25) is in the isolated initial state with RoadCrew, Maintenance roles
- **When:** When they navigate assignment and custody screens with large touch targets, visible text status and intermittent connectivity without losing entered state.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-25 plus scenario namespace ST-075; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Navigate assignment and custody screens with large touch targets, visible text status and intermittent connectivity without losing entered state.
4. Exercise edge cases: gloves; 2G; orientation change; process restart. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-075, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Use large controls under poor connectivity” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-25 change under correlation id tdf-persona-st-075.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-25, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-075 namespaced data in the disposable database.
- Deactivate per-25.tomas@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-076 — Take a Domo inquiry through quote and booking

- **Epic / feature:** EP-10 — Domo discovery, quotes, availability, booking, and follow-up · `domo.quote`
- **Persona:** PER-26 — Micaela Ortiz
- **Goal and business value:** Discover Domo publicly, submit consented inquiry, receive staff follow-up, accept authoritative quote, confirm availability, pay fake deposit, book, complete, and reconcile. Take a Domo inquiry through quote and booking protects a public lead must retain context through quote, deposit, booking, and operations..
- **Roles / permission:** DJ, Customer · own inquiry; staff stages scoped
- **Environment:** mobile-web, backend-api · budget Android phone · es-EC · data saver with 2G fallback
- **Accessibility profile:** reduced motion
- **Test:** cross-epic-integration · `INT-PER-26-DOMO` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-26; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Incomplete fictional Domo inquiry
- Scenario records carry fixture namespace st-076 and correlation id tdf-persona-st-076.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Micaela Ortiz (PER-26) is in the isolated initial state with DJ, Customer roles
- **When:** When they discover Domo publicly, submit consented inquiry, receive staff follow-up, accept authoritative quote, confirm availability, pay fake deposit, book, complete, and reconcile.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-26 plus scenario namespace ST-076; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Discover Domo publicly, submit consented inquiry, receive staff follow-up, accept authoritative quote, confirm availability, pay fake deposit, book, complete, and reconcile.
4. Exercise edge cases: duplicate inquiry; expired quote; unavailable date; WhatsApp declined. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-076, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Take a Domo inquiry through quote and booking” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-26 change under correlation id tdf-persona-st-076.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-26, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-076 namespaced data in the disposable database.
- Deactivate per-26.micaela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-077 — Compare marketplace options on a limited data plan

- **Epic / feature:** EP-07 — Marketplace sales, rentals, availability, tracking, cancellations, and disputes · `marketplace.discovery`
- **Persona:** PER-26 — Micaela Ortiz
- **Goal and business value:** Filter sale and rental listings, inspect total cost and availability, save one option, return from a web fallback, and retain state. Compare marketplace options on a limited data plan protects money, inventory, custody, deposits, and multi-party ownership must remain consistent..
- **Roles / permission:** DJ, Customer · public read; own saved item
- **Environment:** mobile-web, responsive-web · budget Android phone · es-EC · data saver with 2G fallback
- **Accessibility profile:** reduced motion
- **Test:** web-e2e · `PW-PER-26-MARKETPLACE` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-26; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Incomplete fictional Domo inquiry
- Scenario records carry fixture namespace st-077 and correlation id tdf-persona-st-077.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Micaela Ortiz (PER-26) is in the isolated initial state with DJ, Customer roles
- **When:** When they filter sale and rental listings, inspect total cost and availability, save one option, return from a web fallback, and retain state.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-26 plus scenario namespace ST-077; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Filter sale and rental listings, inspect total cost and availability, save one option, return from a web fallback, and retain state.
4. Exercise edge cases: 2G timeout; large images; stale availability; no results. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-077, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Compare marketplace options on a limited data plan” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-26 change under correlation id tdf-persona-st-077.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-26, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-077 namespaced data in the disposable database.
- Deactivate per-26.micaela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.

## ST-078 — Resume interrupted checkout without duplicate payment

- **Epic / feature:** EP-09 — Provider payments, callbacks, webhooks, retries, refunds, and reconciliation · `payments.idempotency`
- **Persona:** PER-26 — Micaela Ortiz
- **Goal and business value:** Start checkout, lose connectivity after submission, reopen from another device, resolve authoritative order state, retry only when safe, and confirm once. Resume interrupted checkout without duplicate payment protects incorrect authority or idempotency can cause duplicate charges or financial misstatement..
- **Roles / permission:** DJ, Customer · own checkout
- **Environment:** mobile-web, backend-api · budget Android phone · es-EC · data saver with 2G fallback
- **Accessibility profile:** reduced motion
- **Test:** backend-integration · `BE-PER-26-INTERRUPTED-PAYMENT` · **specified-not-executed**

**Preconditions**

1. Use a disposable local or explicitly authorized non-production database.
1. Load deterministic fixture PER-26; all records use the reserved persona.test domain and are excluded from public discovery.
1. Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.

**Initial database state**

- Incomplete fictional Domo inquiry
- Scenario records carry fixture namespace st-078 and correlation id tdf-persona-st-078.
- No real user, provider credential, payment instrument, media asset, or public publication exists.

**Acceptance criteria**

- **Given:** Given Micaela Ortiz (PER-26) is in the isolated initial state with DJ, Customer roles
- **When:** When they start checkout, lose connectivity after submission, reopen from another device, resolve authoritative order state, retry only when safe, and confirm once.
- **Then:** Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in es-EC.

**Steps**

1. Reset and load PER-26 plus scenario namespace ST-078; assert the initial database state.
2. Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.
3. Start checkout, lose connectivity after submission, reopen from another device, resolve authoritative order state, retry only when safe, and confirm once.
4. Exercise edge cases: lost response; duplicate tap; late webhook; stale client state. Include direct URL/API denial where authorization or ownership is relevant.
5. Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.
6. Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.
7. Clean scenario namespace ST-078, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.

**Expected visible behavior**

- The interface makes “Resume interrupted checkout without duplicate payment” discoverable and states current status, next action, total price or consequence where relevant.
- Validation and provider errors are specific, recoverable, localized for es-EC, and announced accessibly without color-only meaning.
- Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.

**Expected backend state / side effects**

- Only records owned by or explicitly assigned to PER-26 change under correlation id tdf-persona-st-078.
- State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.
- Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.

**Expected notifications / audit**

- A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.
- Audit events identify PER-26, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.
- Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.

**Cleanup**

- Remove or archive all ST-078 namespaced data in the disposable database.
- Deactivate per-26.micaela@persona.test credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.
- Verify the public index has no fixture records and no external adapter received a real request.

**Execution evidence:** None yet. Specified in this program; executable environment or implementation remains a coverage gap.
