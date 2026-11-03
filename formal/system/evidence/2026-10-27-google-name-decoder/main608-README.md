# Google privacy repair on main after event-memory integration

Qualified clean source: `85693b37d7cfaf43b409d9f4543fe162512b575e`.
Normal integration commit: `3171df86f5d3bda6feb5c01bbee3103625df2aac`, retaining
reviewed privacy candidate `985be1ca49908260d57ca608e1faf278115ed10c` and protected
main `301810ae3861ea36b1fa790beee940bfe2461a02` as parents.

The one-line Google token decoder correction remains unchanged. All 154 main
requirement IDs and objects are preserved except AUTH-ENTRY-002 test/evidence
mappings. Main's 227-entry migration manifest, SQL, UI/E2E, compiled API surface,
CI workflows and Mobile pin `7000df1da934aa467e443ad267c0a8caa1e9cb72` are retained.
The only Cabal change is one test-module registration; Spec.hs only adds its
import/call. All SMT obligation claims and assumptions and all 1,350 catalog
decisions retain their current-main contents.

## Fresh verification

- Exact-source native probe: 19 examples, zero failures/pending; restoring the
  private-email fallback produces eight expected failures out of all 19 cases.
- Scope tests: 18 passed. Authorization and recovery verification controls:
  eight passed in each suite, including their negative source/solver controls.
- Repository quality: passed with the original tests/configuration. The first
  run failed because an overnight-cleaned temporary jq executable was missing;
  that raw failure is retained. Restoring the previously recorded jq 1.8.1 asset
  after exact SHA256 verification changed only a private tool directory.
- Fresh source-bound report: 1,408 inputs match the qualified source. All 154
  complete requirements and 308 implementation/refinement obligations remain
  UNPROVEN. The existing 11 conditional SMT claims pass fresh execution/replay
  under their explicit assumptions. Release remains BLOCKED.
- Strict catalog: 1,350 candidates, zero unreviewed, unchanged candidate IDs and
  decisions. The audit ran at 3171df86; only its derived JSON/CSV and generated
  specification indices changed before the clean qualification commit.

There are 123 critical requirements (20 financial, 12 in domain AUTH). No new
complete requirement proof or whole-system coverage percentage is claimed.
The narrow Google decoder-to-name property is TESTED over the recorded cases.

## Prior evidence and remaining gates

The earlier complete hosted run remains at its recorded f272/985 source and
separate `evidence/google-profile-hosted-20261027` branch. Twelve backend inputs
changed with incoming event-memory work; do not promote that old full-backend
execution to this combined candidate. Its eight focused native-probe inputs
remain identical, but the current source probe was nevertheless executed anew.
The combined real-module/full backend run requires fresh hosted CI, followed by
fresh current-head independent review and canonical FIFO admission.

Compiler, extractor/caller correspondence, libraries and execution environment
remain trusted. This does not establish Google HTTP/account-persistence,
historical-name remediation, arbitrary schedules or complete refinement. No
provider activation, production migration, deployment or risk waiver occurred.

## Reproduce

```sh
STACK_ROOT=/path/to/stack python3 scripts/verify-google-account-name.py
node --test scripts/__tests__/ci-change-scope.test.mjs
python3 scripts/test-auth-predicates.py
python3 scripts/test-recovery-expiry-verifier.py
python3 scripts/verification-report.py --output /tmp/google-main608-unique
npm run quality:repo
```

Use the existing pinned Python verification environment and recorded jq on PATH.
Initialize the exact committed Mobile pin before inventory/report checks. Native
local tools: Python 3.13.5, Stack 3.7.1, GHC 9.10.3, Hspec 2.11.17, Aeson 2.2.5.0,
Text 2.1.3 and Time 1.12.2. Node/npm/jq versions, input and artifact hashes,
archive member hashes and prior-hosted incompatibilities are machine-readable
in `main608-integration.json`; raw logs/formulas/results are in
`main608-artifacts.tar.gz`. The eight privacy counterexamples and original source
remain in the prior qualification without relabeling their execution identities.
