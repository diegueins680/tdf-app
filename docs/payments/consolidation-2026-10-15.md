# Payment integration and verification boundary

PR #551 preserves the ancestry of ledger posting (#551), fixed checkout lines
(#554), capture uncertainty (#555), and policy terms identity (#569). The capture
branch also retains ticket review and browser repairs from #545/#579. Source PRs
and branches must remain until a successor has merged and its tree is verified.

The orphan `fix/release-gate-paypal-refunds` at `c81c4c98d7` is also retained by
ancestry. Its old Datafast-disabled rule was superseded by merged PR #506 and
AUTHORITY-050; the current verifier policy is preserved. Its unique regression
checks that both enabled PayPal webhook and refund flags survive an actual
backend restart, beyond merely accepting those states in a SQL verifier. The
combined automatic-migration/startup harness passes on a disposable database.

The migration manifest retains the complete 204-entry prefix from main
`2781ddc2a3c9fd661684742550918d653afdc0cf`, followed by ledger posting,
checkout-line snapshots, terms identity, and historical terms identity: 208
entries in total. Original introduction commits are retained. The unmerged
October 13 terms migration has a reviewed-candidate checksum change: its preflight and
trigger installation now share a transaction and writer-excluding table locks.
This checksum revision needs review before production use.

The old terms migration admitted a concurrent conflicting policy after its
preflight. The new canonical PostgreSQL race test witnesses both course and
event writers blocked until installation, then rejected by the installed guard.
Running that same test against the original #569 SQL fails its named assertion
for each scope. The bounded TermsIdentityUpgrade model and three counterexample
controls cover this installation protocol; they are not universal proofs or a
refinement proof of PostgreSQL.

## Current review repairs

The October 15 additive historical-identity migration checks append-only policy
snapshots as well as current rows. Deleting the last live draft cannot permit
the same terms version to name different text. Current and history tables are
locked together across preflight and installation; conflicting or incomplete
historical snapshots refuse installation. Retain this guard and its indexes
across application rollback. The earlier rollback file must not be used to
remove the historical protection, and historical snapshots must not be rewritten
to make the preflight pass.

Completed PayPal captures with invalid provider binding remain fenced against
another payment method. The uncertainty check now holds the checkout lock before
reading payment attempts, and failure writers take that same lock. Existing
event-before-checkout ordering is preserved. Ledger and ticket-payment fixtures
use the shared parsed URI authority guard, including rejection of userinfo-based
remote-host substitution and inherited libpq routing overrides.

On clean implementation `1c41ffbc40e6213a4dfdcc8aa4e7f9cd493e4bab`, all 11 compiled
payment PostgreSQL cases pass. Removing the binding repair or pre-read lock in
isolated control worktrees fails the corresponding assertion; the latter control
actually admits the competing payment attempt after the failure writer commits.
The repaired race uses observed PostgreSQL blocking, bounded waits and cleanup
that releases the lock holder before cancelling a blocked task. All 10 ledger
cases and 23 URI/runner tests also pass.

All 208 migrations applied and replayed on a fresh owned PostgreSQL 16.10
database. Canonical introduction ancestry and checksums pass. Course/event
history tests reject changed-text reuse and accept identical replay and new
versions; both original live-only guard controls reproduce forbidden reuse,
and both conflicting-history preflights refuse installation. Three affected
bounded model configurations and six expected counterexamples pass. Repository
quality, formal inventory, traceability, changed-surface mapping and
`npm run verify:formal` pass. Full current-head hosted integration and independent
review remain required; the earlier complete-suite results below retain their
original source scope. No new migration has been applied to production.

## Executed local checks

On the integrated implementation (source `354e4f0fff09f38dee75ac11a7d3f512f0ac3406`
before the subsequent provider-harness/documentation update):

- Backend quality: 3,948 examples, zero failures, 19 environment-dependent pending.
- HTTP/PostgreSQL conformance: 619 checks, clean source receipt; owned database
  removed. Cached build metadata is not a release identity attestation.
- Complete 207-entry manifest applied twice on disposable PostgreSQL 16.10.
- Ledger: nine database examples, five invalid legacy upgrade histories refused,
  clean reapplication, and witnessed reversal races at READ COMMITTED and
  REPEATABLE READ.
- Checkout amount/snapshot runner: passed, including concurrent append checks.
- Terms runner: passed on PostgreSQL 17.10, including the installation race,
  rollback/reapply, and independent transactions; no production data involved.
- Ticket payment gate: ten examples; manual-review concurrency: 15 examples.
- Provider retry/refund runner with both new ledger and line constraints: 304
  examples, zero failures, plus its rollback/history preservation checks.
- Four affected bounded TLA+ models and 13 expected counterexamples passed.
- Repository quality (including `npm run verify:formal`) and production web
  build passed. Initial JavaScript budget: five preloads, 332,262 gzip bytes.

These results have separate scopes. The 19 pending backend examples are not
reported as executed by the ordinary backend run. Hosted CI and independent
review must qualify the actual published head before merge.

Subsequent verification executed all 19 environment-dependent groups separately:
188 database examples passed. Hosted CI on `1ec64e5ab` passed 3,231 web tests
across 293 suites and 625 Mobile tests across 97 suites, but failed an older
ledger arithmetic fixture: it caught generic P0001 while the posting-integrity
guard returns SQLSTATE23514. The repaired fixture requires the typed rejection
and exact ledger diagnostic. All six arithmetic cases and the standalone
manual-review harness's nine examples pass; disabling the posting guard or
raising an unrelated constraint error still makes the fixture fail. This changes
test admission, not production SQL. Historical CI remains failed; the follow-up
head requires its own current checks and review.

## Full-schema provider diagnostic: unresolved

Directly running all 304 provider tests against a disposable database built
from the full 207-entry manifest produced **119 failures**. This is a diagnostic,
not a passing full-schema provider gate. The canonical provider runner uses
reduced domain fixtures; it now includes the new ledger and line constraints.
The full-schema HTTP and ticket suites cover different boundaries.

The diagnostic separates two concerns:

1. Fixture incompatibilities: synthetic events omit mandatory event types;
   ticket ledger/refund projections omit approved policy and complete runtime
   fields; some tests set a checkout to paid without payment evidence; generic
   capture fixtures use `payment` even for providers whose domain guard expects
   `capture` or `checkout`. These fixtures must be ported to real domain setup
   before their entire suite can establish full-schema compatibility.
2. A pre-existing implementation mismatch: the service-booking verified-payment
   guard accepts Datafast checkout, PayPal capture, Stripe payment, and approved
   manual evidence, but not PlaceToPay session or PayPhone sale. It also assumes
   the binding reference equals the domain order, whereas provider execution
   stores its immutable provider reference. The event/course/Domo guards have
   the same older provider list. These guard definitions are unchanged from
   main `2781ddc2`; the integration does not repair or bypass them.

Provider execution's authenticated-query checks must remain intact. Repairing
the mismatch requires a reviewed additive migration with exact binding tests,
all affected domain guards, and negative controls for wrong account, reference,
resource, currency and amount. A fixture workaround or disabling a trigger would
not establish safety. Do not use the reduced provider suite as evidence that
these rails can settle every production domain, and do not activate them on the
basis of this integration.

No production migration, provider request, real-money transaction, feature
activation, or deployment was performed for this consolidation. Production
preflight, data backup, the canonical guarded executor and independent review
remain release requirements.
