# Stale catalog decision reconciliation contract

Depends on draft #416 at `d5505e2731d395cc2a5859d215e36ffda9e88138`.
Continues the approved CI catalog review, not feature implementation.

Before changing active decisions:

1. Reconstruct each of the nine obsolete fingerprints from historical source
   using the unchanged scanner. Resolve mobile history through the root gitlink.
2. Review its current replacement, consumer and authority individually. A changed
   fingerprint does not automatically inherit approval or classification.
3. Retain the complete original decision in a separate retirement ledger with
   its source revision, exact successor, change description and original-object
   SHA-256. The ledger is evidence, never an input to the approval scanner.
4. Remove only those nine superseded active entries and add exactly nine reviewed
   current entries. Preserve every other decision and all application sources,
   manifests, generated clients, mobile gitlinks, CI controls and scanner code.
5. API function registries remain technical dispatch. RSVP statuses remain
   governed reference data. Role, capability, request-field and navigation
   registries retain security review; a DTO field or route does not grant access.
   Onboarding intent remains a governed business consumer, not a new authority.
6. Test ledger integrity and exact current fingerprints using isolated copies of
   the inspected files. A negative control using retired decisions must fail.
   Then run the complete audit with the initialized, unchanged mobile gitlink;
   all unrelated unreviewed findings must continue to fail the gate.

No runtime, authorization or workflow transition changes are permitted in this
increment. No new formal-model check is necessary or claimed for these metadata
changes. Existing finite model-checking results do not prove catalog completeness.
Rollback restores the nine original decisions and removes their replacements and
this ledger; it does not roll back migrations or alter production data.
