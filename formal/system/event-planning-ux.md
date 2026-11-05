# Event preparation: clock, price estimate and poster recovery

Contract: EVT-PLANNING-UX-001. This change prepares an event; it grants no new
organizer, finance, publication or account permissions.

The creator interprets wall-clock input in its explicit event timezone and sends
both UTC instants and that zone. Invalid zones, nonexistent and ambiguous DST
times are rejected. Restored v1 drafts without a zone inherit the current user
preference. A new logistics plan inherits the stored event zone; an explicitly
saved logistics zone takes precedence, then the configured legacy default.

Ticket forms show a standard self-service estimate before submission. Integer
cents and BigInt arithmetic implement 400 basis points, floor the total fee and
assign an odd fee cent to the buyer. Checkout's live server policy is authoritative;
these estimates do not change billing, taxes, refunds or stock. The UI tests
exhaustively check conservation for 0–100,000 face-value cents and test invalid,
fractional and unsafe numeric boundaries. This is a finite check, not a proof
over all inputs or a claim about every payment provider.

Only the existing organizer sees the preparation panel. The existing image
endpoint performs attachment under its event lock. The UI confirms the stored
URL with a read, retains the selected image after failure, and reuses a confirmed
upload when that read fails. It never sends a full event PUT. A changed auth epoch,
owner, route or unmounted editor must stop late continuation and success feedback.

`scripts/event-planning-ux-model.mjs` explores the abstract upload/confirm/retry
callback model to depth 8. Its tests reject three negative controls: a removed
post-upload owner guard, a removed confirmation guard and a duplicate upload on
retry. React tests separately execute those boundaries against the actual panel.
The model is not a network, browser, storage or Haskell concurrency proof.

Validation commands:

```sh
node --test scripts/__tests__/event-planning-ux-model.test.mjs
cd tdf-hq-ui
npm test -- --runTestsByPath src/components/events/EventPreparationPanel.test.tsx src/pages/collaborativeEventCreation.test.ts src/utils/eventTicketPricing.test.ts src/pages/SocialEventDetailPage.test.tsx src/pages/PartyRelationshipMigration.test.ts
npm run typecheck
npm run build
```

Haskell tests in `tdf-hq/test/Spec.hs` cover timezone precedence. They require the
repository's pinned Stack toolchain. The current authoring environment has no
Stack executable, so local UI checks do not establish a backend test pass.

Remaining UX work: compact agenda/lazy operation reads, a shared paid-ticket pool
with private courtesy issuance, campaign readiness checks and a reviewed delegated
event-manager role. The current logistics editor role is not full administration
of tickets, event metadata, finance, refunds or team permissions.
