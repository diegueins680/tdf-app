# Startup capability-gate policy (DEPLOY-STARTUP-001)

Reconstructed on 2026-10-12 from `buildSchemaVerificationSql` in
`scripts/lib/production-release.mjs` (the `revenue_feature_flag` assertions)
and `tdf-hq/production-entrypoint.sh`, which runs that verification after the
migration batch and before the server accepts traffic. The requirement text
was read against the code, not the other way round.

## The policy as coded

Every rule reads `revenue_feature_flag` rows with `environment = 'production'`
and raises a fixed message; the verification never writes a flag.

| Flags | Accepted states | Fixed message |
| --- | --- | --- |
| `checkout.bank_transfer`, `checkout.cash`, `checkout.pos` | enabled | Manual settlement capability flags are missing or disabled |
| `commerce.marketplace_sales`, `commerce.marketplace_rentals` | enabled | Production marketplace sales and rentals capability gates must exist enabled |
| `commerce.service_bookings` | disabled | Production service booking capability gate must exist disabled |
| `commerce.courses`, `commerce.course_recurring_billing` | disabled | Production course checkout and recurring billing gates must exist disabled |
| `checkout.paypal.webhooks`, `checkout.paypal.refunds`, `checkout.datafast.webhooks`, `checkout.datafast.refunds` | enabled or disabled (AUTHORITY-050) | Production provider capability gates must exist; PlaceToPay and Payphone must remain staged |
| `checkout.placetopay.webhooks`, `checkout.payphone.notifications` | disabled | same |

A missing row fails in every group. No other production flag is asserted at
startup (the hetzner preparation observes more flags; it does not gate
startup).

## What this means for operations (the restart hazard)

Enabling `commerce.service_bookings`, `commerce.courses`,
`commerce.course_recurring_billing`, `checkout.placetopay.webhooks` or
`checkout.payphone.notifications` in production, or disabling any of the
manual-settlement or marketplace gates, does not affect the running API but
makes every later container start fail verification with the message above:
the class of the 2026-10-07 incident (AUTHORITY-050), when
`checkout.paypal.refunds` was enabled by product decision while the verifier
still required it disabled and production returned 502 at the first restart.
Such a decision must change the verifier (and this policy) in the same release
as the flag. The requirement's statement, which named only the provider gates,
is completed with the full asserted set (AUTHORITY-077); the code is unchanged.

## Evidence

- `scripts/__tests__/startup-gates-postgres.mjs` (CI step "Verify the startup
  capability-gate policy for every asserted flag state"): the real
  verification SQL run through psql on the migrated fixture, the approved
  baseline and the two joint extremes of the either-state gates accepted, then
  each of the fourteen flags alone in each of its three states (42 runs):
  every state accepted or rejected exactly as the table says, with the fixed
  message; the sweep restores the fixture's rows and checks the verification
  wrote none.
- `formal/event-operations/StartupGates.tla` (+4 controls): one state; the
  policy copied per flag, guard by guard; `ApprovedStarts` (every approved
  configuration verifies) and `StartImpliesApproved` (a verifying configuration
  is approved) over all 3^14 = 4,782,969 assignments. Controls
  `StagedAccepted`, `MissingAccepted`, `EitherRejected` (the pre-AUTHORITY-050
  verifier) and `RequiredUnchecked` each fail on the named invariant.
- `scripts/__tests__/production-release.test.mjs` and
  `scripts/test-automatic-migrations-production-schema.sh` (unchanged):
  structure of the verification and its acceptance of the fixture.

## Bounds and ambiguities

- The policy is a conjunction of per-flag rules, so varying one flag at a
  time over an approved baseline is exhaustive for the runtime evidence; the
  model quantifies over all joint assignments.
- The model is a hand copy of the rules; the PostgreSQL sweep ties it to the
  SQL. Neither covers the verifier's other assertions (tables, triggers,
  functions), which the automatic-migrations rehearsal exercises.
- Production's actual flag states are not observable from the repository;
  the hazard list is a precondition for flag changes, not a statement about
  production.
