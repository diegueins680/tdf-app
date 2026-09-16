# Subscription and operator readiness review

Public subscription routes now require verified recurring capability in addition to one-time checkout completion and caller restrictions. Datafast card and PayPal wallet regressions remove each required capability independently and verify the route disappears. This does not activate a provider or claim live subscription qualification.

Operator account cards display capability evidence only when its verification environment exactly matches the account environment. The UI regression mixes sandbox and production capabilities on both accounts and verifies that evidence does not cross accounts or environments.

Validation: Stack backend 2555 examples, zero failures; operator UI 6 tests, zero failures; UI typecheck and repository quality passed. Strict catalog snapshots are refreshed separately. No schema, API or generated-client changes. The canonical no-charge fallback concern remains open as explained in component-environment-review-2026-09-16.md.
