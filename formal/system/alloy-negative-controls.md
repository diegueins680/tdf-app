# Alloy mutation sensitivity

SYS-VERIFY-002 requires the event-operations Alloy checks to reject deliberately
broken guards, in addition to finding a consistent scenario and no counterexample
to the intended assertions. A successful assertion check alone is insufficient:
Alloy facts restrict its search, and several structural assertions restate facts.

The canonical executable control register is
`scripts/lib/alloy-controls.mjs`. It maps each assertion in `EventStructure.als`
and `TaskReadStructure.als` to at least one mutation of a named fact or predicate.
The runner copies each model to a fresh evidence directory and changes exactly
one occurrence within that declaration. Assertions, commands and scopes retain
their original bytes. Missing, duplicate or unchanged mutation targets reject;
new assertions without a control reject. No second handwritten model is maintained.

The event controls remove accountable uniqueness, responsible presence, acyclicity,
current-version consent, exclusive booking conflict protection, invitation scope
attenuation, private-read membership and RACI membership. The task controls remove
event locality, exact-task scope, current grant effectiveness, actor binding,
permitted action scope, manager authority and candidate read authority.

Each original scenario must be SAT; each original assertion must be UNSAT; each
mutation must yield SAT for its named assertion, demonstrating a counterexample.
The checksum-pinned Alloy 6.2.0 SAT4J process must exit zero without timeout/error
and report exactly the expected command, result and matching solver receipt.
SAT requires a retained solution. Failed executions remain failed evidence.
Source and mutated-source hashes, command scopes, logs, receipt hashes and
counterexample instances remain in the evidence directory. The overall formal
runner archives it alongside TLC evidence and rejects changed source fingerprints.

The bounds remain those written in the original models: event scenarios use six
default atoms and five-bit integers with the specified exact per-signature bounds;
event assertions use four default atoms and four-bit integers with their exact
overrides. Task scenarios use the exact two-Party/two-Event/two-Task/one-Grant
configuration; task assertions use scope four. These are static relational checks;
fairness and temporal liveness are not modeled. Effective grants abstract expiry
and revocation; scopes are not a production role matrix. Database transactions,
HTTP authorization, provider behavior and source-to-model refinement remain separate
obligations. Mutation sensitivity does not establish whole-system correctness or
prove that every relevant defect is detected.

Run with the same pinned Java/Alloy toolchain as the existing formal suite:

```sh
node --test scripts/__tests__/alloy-controls.test.mjs
node scripts/verify-alloy-controls.mjs --output /tmp/NEW-UNUSED-ALLOY-EVIDENCE
node scripts/verify-system-evidence.mjs --output /tmp/NEW-UNUSED-FORMAL-EVIDENCE
```

The ordinary repository quality gate runs admission/coverage regression tests;
the required formal CI job runs the real solver and every mutation.
