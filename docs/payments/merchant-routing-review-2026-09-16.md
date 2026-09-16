# Merchant-bound approval and CI fixture sequencing

## Availability follow-up

Public availability now retains account identity while checking runtime readiness,
before converting approved activations into public routes. PayPal's configured
merchant ID and Datafast's entity ID must exactly match the approved account's
nonblank merchant reference, as well as the requested environment. Contract,
credential, feature and method-specific capability gates remain unchanged.
The regression uses the actual availability function and account/capability SQL
queries against SQLite with synthetic local configuration: both providers match,
mismatched/missing merchants fail closed, and environment mismatch fails closed.
No provider request, activation, secret change or schema migration is involved.
This follow-up's full validation is recorded separately from the earlier counts below.

Availability follow-up validation: `stack test tdf-hq:test:tdf-hq-test --fast
--jobs 2 --test-arguments='--match=provider-neutral --fail-on=empty'` passed all
31 examples; `stack test --fast --jobs 2 --test-arguments=--format=progress`
built executable/tests and passed 2,559 examples. This does not resolve the
separate Datafast hosted-checkout finality/fallback review blocker and is not
evidence of a live provider transaction or a successful final-head CI run.

Canonical route validation now retains the approved account merchant reference and requires an exact nonblank match with the runtime attempt merchant before creating an intent or contacting a provider. Matching provider/environment alone does not transfer merchant A's approval to merchant B. Contract, capability, environment and feature controls remain. The PostgreSQL regression loads actual account/capability rows, allows A and rejects B, blank, padded and missing approved references.

The first Stack test invocation explicitly removes TDF_PAYMENT_AUDIT_DATABASE_URL from its child environment. The dedicated PostgreSQL invocation retains the configured URL and initializes the isolated fixture before enabling its integration tests. No tests are skipped from the combined quality job; the integration suite runs once after fixture setup.

Results and exact commands are recorded in the audit validations.md. No deployment or provider activation was performed.

Local validation completed: Stack2555 examples/0 failures, actual PostgreSQL4 examples/0 failures, repository quality, strict JSON/CSV1117 and whitespace checks. The separate default-optimization full quality job remains recorded independently until completion.
