# Exact ee745ca41 checkpoint

Exact clean revision ee745ca41f249952c1ddb3eabdcb11ec736883cc passed the complete
bounded formal runner and repository quality gate. The formal receipt records
unchanged source fingerprints, tool hashes and log hash. Its scope is57 positive
TLC configurations,125 expected mutation counterexamples, one PlusCal regeneration
with13 translator-integrity tests, and Alloy2SAT/13UNSAT within declared bounds.
The125 count includes69 `expect_counterexample` calls and56 `run_negative_tlc`
calls. Earlier prose counted only the first helper and underreported the total;
the earlier raw logs remain unchanged. This does not establish whole-program
refinement or universal correctness.

Independent read-only exact-revision review found no actionable issue, running
87Node and7Python checks and verifying the scoped chat runtime evidence hashes.
It is not GitHub approval. The actual chat before/after evidence remains in
../2026-10-05-chat/checkpoint.json; the full fixed suite had1148examples/0failures.

The allowlisted read-only runtime receipt and preparation were collected at this
clean revision. They observe production backend645f56fcc44f81609fbfd0e03d683b40376ce77a,
159applied and10pending migrations, and live permissive CORS. Preparation is
explicitly non-executable. No production rollout, provider activation, charge or
migration was performed. These receipts expire for future release preparation;
do not present them as a later live state or deployment clearance.
