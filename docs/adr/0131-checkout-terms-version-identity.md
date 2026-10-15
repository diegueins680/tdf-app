# ADR 0131: a terms_version names exactly one terms text per course or event

Date: 2026-10-13 UTC. Status: implemented in
`tdf-hq/sql/2026-10-13_checkout_policy_terms_identity.sql`; resolves issue #564.
Depends on [ADR 0111](0111-course-seat-holds-and-verified-enrollment.md) and the
LEGAL-CONSENT-001 binding (`CourseCheckout.hs`, `SocialEventsHandlers.hs`).

## Context

Course and ticket checkouts show the approved policy's terms text and bind the
buyer's acceptance to that policy's `terms_version`: the client sends the version
it displayed and the server rejects a missing or stale version with 400/409.
Policies are immutable once approved ("create a new version"), but the schema
only made `(course_id, policy_version)` and `(event_id, policy_version)` unique.
A replacement policy row could therefore reuse the previous row's
`terms_version` while changing `terms_summary`, `cancellation_policy` or
`refund_policy`. A buyer still holding the old text would pass the version
comparison, and the order would record the new text as accepted.

No application path creates or activates these policies today; they are written
by migrations and operator SQL. The hazard is an operator mistake, so the
invariant belongs in the database, where every writer meets it.

## Decision

Within a course or event, a `terms_version` identifies exactly one terms text:

- `course_checkout_policy`: `(terms_summary, cancellation_policy)`;
- `event_ticket_checkout_policy`: `(terms_summary, refund_policy)`.

`BEFORE INSERT OR UPDATE` triggers reject a row whose `(scope, terms_version)`
already names different text. Scope-only changes to `course_id` or `event_id`
also run the corresponding trigger and validate the destination scope. Rows with
identical text may share a version, so a
price-only or fee-only replacement keeps the version the buyer already accepted.
Each trigger takes a transaction-scoped advisory lock on `(scope, terms_version)`
before the existence check, so two concurrent writers of differing rows cannot
both pass under `READ COMMITTED`.

The migration first checks existing rows and raises if any scope already reuses
a version for different text; the operator assigns new versions before the
triggers are installed. The rollback drops the triggers and leaves rows intact.

A plain uniqueness constraint on `(scope, terms_version)` was rejected because
it would forbid the legitimate same-text reuse above and force a new acceptance
round for buyers whose terms did not change. Sending a content digest from the
client was rejected as a larger contract change with no additional protection
once the version is guaranteed to name one text.

## Operator procedure

To change the terms of a course or event: insert a new policy row with a **new**
`terms_version` and the new text, approve and activate it, and retire the old
row. Reusing a version is accepted only when the text is byte-identical.

## Verification

`scripts/test-checkout-policy-terms-identity-migration.sh` (migration-tests job)
applies the checkout runtime migrations, proves the failing precondition
installs nothing, applies the migration twice, exercises allowed reuse, rejected
insert/update/rename reuse for both tables, rollback and reinstall. Scope-only
reassignment is rejected when the destination has conflicting text, and permitted
for matching text or an unused version in that scope. Removing either scope column
from its trigger independently makes the corresponding regression fail.
