# Merchant-bound approval and CI fixture sequencing

Canonical route validation now retains the approved account merchant reference and requires an exact nonblank match with the runtime attempt merchant before creating an intent or contacting a provider. Matching provider/environment alone does not transfer merchant A's approval to merchant B. Contract, capability, environment and feature controls remain. The PostgreSQL regression loads actual account/capability rows, allows A and rejects B, blank, padded and missing approved references.

The first Stack test invocation explicitly removes TDF_PAYMENT_AUDIT_DATABASE_URL from its child environment. The dedicated PostgreSQL invocation retains the configured URL and initializes the isolated fixture before enabling its integration tests. No tests are skipped from the combined quality job; the integration suite runs once after fixture setup.

Results and exact commands are recorded in the audit validations.md. No deployment or provider activation was performed.

Local validation completed: Stack2555 examples/0 failures, actual PostgreSQL4 examples/0 failures, repository quality, strict JSON/CSV1117 and whitespace checks. The separate default-optimization full quality job remains recorded independently until completion.
