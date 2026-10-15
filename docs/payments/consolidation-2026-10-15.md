# Payment integration and verification boundary

PR #551 preserves the ancestry of ledger posting (#551), fixed checkout lines
(#554), capture uncertainty (#555), and policy terms identity (#569). The capture
branch also retains ticket review and browser repairs from #545/#579. Source PRs
and branches must remain until a successor has merged and its tree is verified.

The migration manifest retains the complete 204-entry prefix from main
`2781ddc2a3c9fd661684742550918d653afdc0cf`, followed by ledger posting,
checkout-line snapshots, and terms identity. Original introduction commits are
retained. The unmerged terms migration has a new checksum: its preflight and
trigger installation now share a transaction and writer-excluding table locks.
This checksum revision needs review before production use.

The old terms migration admitted a concurrent conflicting policy after its
preflight. The new canonical PostgreSQL race test witnesses both course and
event writers blocked until installation, then rejected by the installed guard.
Running that same test against the original #569 SQL fails its named assertion
for each scope. The bounded TermsIdentityUpgrade model and three counterexample
controls cover this installation protocol; they are not universal proofs or a
refinement proof of PostgreSQL.

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
