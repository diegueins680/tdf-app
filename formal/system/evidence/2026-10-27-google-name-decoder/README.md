# Google token decoder: account-name privacy qualification

Requirement: **AUTH-ENTRY-002**, automatic account-name default only.
Qualified clean source: `eb0d3a2ad1cf78d217373915a427ef74e90fa65e`.
Parent PR #627 head preserved: `67cd04d502c880348ebfca8c1be37aa96f598e91`.
Protected main observed: `4b81ab4ab92416aa31dc816057fb8e6a736596d1`.
Publication adds evidence and generated catalog metadata; fingerprint equality
must hold before any qualified result is reused.

## Counterexample and correction

A valid Google token-info JSON object with verified synthetic contact email and
no usable name is accepted. The old validator substitutes the normalized email
into `gpName`, so the subsequent neutral-name helper treats it as a supplied
public name. Eight missing/null/invalid-name variants reproduce that disclosure.
The restored-fallback mutation reproduces the entire old ServerAuth file from
`f41c993a56e55242b5b0c09c4d5da96970ca21ae`, not a hand-written approximation.

Preserve `Nothing` after name sanitization. The existing account-creation helper
then selects `Cuenta TDF`. Preserve normalized contact email, valid explicit
provider name, subject/issuer and the existing authentication rejection rules.
No SQL or financial runtime source changes are introduced by this repair.

## Reproduce

```sh
STACK_ROOT=/path/to/stack python3 scripts/verify-google-account-name.py
node --test scripts/__tests__/ci-change-scope.test.mjs
python3 scripts/test-auth-predicates.py
python3 scripts/test-recovery-expiry-verifier.py
python3 scripts/verification-report.py --output /tmp/google-name-assurance-unique
npm run quality:repo
REQUIRE_STACK=1 STACK_ROOT=/path/to/stack npm run quality:backend
```

Use the repository-pinned Stack project and a Python environment containing the
existing pinned Z3/PyYAML dependencies. The last command is required hosted
qualification; it was not locally executed in this constrained checkout.
The backend suite imports the full ServerAuth module. The quality lane also
selects the new group with `--fail-on=empty`. Script-only changes select backend.

Local native results: 19 examples, zero failures, zero pending; restored fallback
completes all 19 with eight failures. Six negative cases retain rejection of
unverified email, wrong audience/issuer, blank subject, malformed email and an
expired token. The canonical backend registration adds 15 cases alongside four
existing helper cases. Existing validator cases retain their original unsafe
inputs and now require absence rather than the defective email substitution.

Repository quality, 18 scope tests, eight authorization-verifier controls and
eight recovery-verifier controls pass. A fresh assurance report binds 1,363
inputs and replays the existing eleven conditional SMT claims. These are not
privacy proofs. The catalog retry uses compressed stdout after an ENOSPC write;
the failed empty output is not evidence of success. No large native build was
attempted locally and no other actor's process or checkout was changed.

## Assurance snapshot

| Measurement | Repository-derived result |
| --- | --- |
| Requirements / critical requirements | 150 / 119 |
| Complete requirement proofs | 0; all 150 remain UNPROVEN |
| Implementation / refinement obligations | 150 / 150 UNPROVEN |
| Existing conditional SMT claims | 11 discharged under their recorded assumptions |
| Critical financial / authorization requirements | 20 / 12; zero complete proofs claimed |
| Privacy property repaired in this cycle | 1 narrow property TESTED; 19 examples |
| Counterexample inputs / underlying defects | 8 / 1 |
| Restored defect control | Detected, eight failures out of 19 examples |
| Critical defects fixed / integrated in main this cycle | 1 / 0 at qualification |
| Current evidence source coverage | 1,363 exact input hashes; no full-system coverage percentage |
| Full backend / provider / HTTP / SQL execution | Requires hosted backend CI / not established / not established / not established |

Review and merge state must be read from the child PR. No approval, integration,
CI enforcement at a historical merge, or production release is inferred from
this source qualification. Parent PR #627 should remain held until this decoder
path is independently reviewed and its full backend CI passes.

## Limits and next obligations

TESTED source fragments with a trusted extractor, caller check, compiler and
libraries. The actual JSON FromJSON instance, validation dependencies and name
selector are compiled; this is not execution of the network handler or account
persistence. The native regression module is shared with the full backend suite,
where it imports ServerAuth directly. Whole-program refinement, live Google
provider behavior, historical public-name remediation and complete profile
publication policy remain unresolved. No new liveness/concurrency claim is made.

The next required gate is full backend qualification and independent current-head
review, followed by a protected normal integration into #627 and fresh parent
checks. No deployment, provider activation or historical-data rewrite is authorized
by this evidence.
