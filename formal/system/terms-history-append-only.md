# Checkout terms history remains append-only

`LEGAL-CONSENT-001` requires each course/event terms version to retain one text.
The historical-identity guard consults retained snapshots after live policy rows
are deleted. Without archive mutation guards, deleting those snapshots permits a
different text to reuse the version; changing snapshots also rewrites the evidence.

The additive `2026-10-22_checkout_terms_history_append_only.sql` migration installs
row guards for UPDATE/DELETE and statement guards for TRUNCATE on both history
tables in one transaction. Existing migration SQL and all 216 manifest entries
remain intact; entry 217 refers to the new SQL's committed ancestor revision.
Snapshot INSERTs and ordinary policy lifecycle operations remain available. No
historical row is repaired, rewritten or deleted. Keep these guards through an
application rollback.

The full-schema SQL fixture checks all six mutation boundaries independently,
then retains conflicting reuse/self-update refusals, identical replay and new
version acceptance. Each expected-failure control selectively disables one owned
fixture guard in a rolled-back transaction and must restore the trigger definition.
It never targets a shared or production database. The finite two-scope, one-version,
two-text model retains independent disclosed history and requires the original
live-only counterexample plus three archive-mutation counterexamples.

These ordinary DML/TRUNCATE guards do not prevent a privileged table owner from
changing DDL, disabling triggers or changing replication semantics. No production
role audit, missing-history reconstruction, retention policy or complete
checkout-handler refinement is established. Production rollout and retention gates
remain separate requirements; the financial candidate remains draft.
