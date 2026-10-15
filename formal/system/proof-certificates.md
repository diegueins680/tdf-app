# Independent SMT and CPC evidence

Authority: the owner's formal-verification mandate §§8, 12, 18–19 and
`SYS-VERIFY-002`. This supplements the conditional contracts in
[auth-predicates.md](auth-predicates.md),
[recovery-expiry-arithmetic.md](recovery-expiry-arithmetic.md) and
[payment-arithmetic.md](payment-arithmetic.md). Business requirements,
preconditions and source bindings are unchanged.

## Claims and trust boundary

CVC5 1.3.1 independently rechecks **all 41 exact SMT inputs** retained by a fresh
assurance report: 14 UNSAT contracts/lemmas and 27 SAT witnesses/controls. Solver
agreement is separate from certificate checking. Neither proves that a trusted
source translator correctly implements Haskell, SQL or HTTP semantics.

Eight UNSAT formulas additionally have CPC refutations independently checked by
Ethos 0.2.1 with the matching, pinned CVC5 CPC signatures:

| Suite | Formula IDs | Certificate claim |
|---|---|---|
| authorization | strict-admin, module-access, ingestion-admission | Refutation of each negated source-derived predicate contract |
| authorization | cardinality-nonnegative, cardinality-zero, cardinality-sum | Refutation of each negated supporting algebra lemma |
| recovery | schema-correspondence, issuance-exact | Refutation of each negated source-derived arithmetic contract |

These certificate claims concern the exact SMT domains. Three authorization and
two recovery contracts retain their **conditional source-derived** scope.
Supporting lemmas remain separate. No full implementation or refinement obligation
changes status. The 122 complete requirements remain UNPROVEN; release remains
BLOCKED with no accepted risks.

The remaining six UNSAT inputs have **UNPROVEN certificate validation**: four
payment contracts, recovery window-equivalence and issuance-admission. They retain
their separately established conditional SMT status. Initial capture-safety
generation timed out at 30 seconds; recovery window-equivalence and
issuance-admission timed out at 15 seconds. The other three payment certificate
inputs were not attempted. Timeouts are not refutations. Initial LFSC
authorization output contained unjustified ACI_NORM/EVALUATE trust steps despite
the no-proof-allow-trust flag. Those outputs are not accepted certificates. CPC
was selected after independent checking succeeded for the eight listed inputs.
No source invariant, numeric domain or query was weakened to obtain a certificate.

## Admission and correspondence

`verify-proof-certificates.py` requires a successful, fresh assurance report with
the same source fingerprint, revision, Mobile identity and dirty state as the
current checkout. Consumer-owned expectations come from the reporter. Exact SMT
bytes and hashes are retained in a new evidence directory. Each CVC5 invocation
has a 30-second limit and a 45-second process deadline. CVC5 ignores Z3's known
`:timeout` option; that diagnostic is admitted only for the exact known preamble,
with these independent time limits.

A restricted s-expression parser admits only definitions, matching declarations,
assumptions, scopes and proof steps. Checked duplicate declarations are removed:
redeclaring would create fresh variables and lose reference binding. Assertions
are not rewritten. Unknown commands, signature/rule declarations, shadowing
definitions and unbalanced scopes refuse admission. The final root step must
explicitly conclude `false`.

Each checker input includes the pinned `Cpc.eo` and references the **original SMT
file**. Ethos checks root assumptions against that reference. Only exit zero
with exact `correct` output is admitted; `incomplete`, timeout or errors do not
establish a certificate. Ethos returns `correct` for an empty file, so the final
contradiction check is essential. Real checker controls reproduce valid checking,
forged-assumption rejection, trust-step incompleteness and the empty-file hazard.

Receipt statuses concern exact formulas and the recorded toolchain. Python
admission itself is regression-tested, not universally proved. Independent
checking reduces reliance on the solving algorithm; it does not certify source
translation or eliminate the TCB.

## Reproduction and CI

Use Python 3.13, existing pinned Python requirements, CMake, a C++17 compiler and
GMP development headers/library. Tool archives/source revisions are SHA-256 pinned
in [certificate-tools.json](certificate-tools.json). Installer platforms are Linux
x86_64 and macOS x86_64/arm64; others fail explicitly. Use new external directories.

```sh
python scripts/install-proof-checkers.py --output /absolute/new-tools
python scripts/verification-report.py --output /absolute/new-report
PROOF_CHECKER_TOOLS=/absolute/new-tools python scripts/test-proof-certificates.py
python scripts/verify-proof-certificates.py --report /absolute/new-report --tools /absolute/new-tools --output /absolute/new-certificates
```

The existing specification job runs these checks on PRs, main and weekly. Its
ten-minute budget includes the checker build; no existing check is removed. It
retains SMT/CPC files, solver/checker streams, hashes, installation/build receipts
and failures. All 41 cross-checks and eight supported certificates must succeed.
Six unsupported certificate obligations remain explicitly listed; passing this
job grants no release permission.

Retained CPC inputs use relative paths and include the exact pinned signatures.
After downloading or moving an artifact, rerun `ethos PATH/authorization/strict-admin.cpc`
(and each other retained `.cpc`) directly. Require `correct` and exit zero; input
and proof hashes remain in `result.json`. No solver is needed for that replay.

## Trusted computing base and primary sources

Trusted: Ethos parser/type checker/evaluator, CPC signatures and rule side
conditions, admission/reference glue, Python, SHA-256/Git, compiler, system GMP,
OS/hardware and archive distribution. CVC5 is a proof producer; producer correctness
alone does not justify an admitted certificate. Reviewed source translators,
bindings, Haskell/SQL semantics and runtime/compiler assumptions remain trusted
for source correspondence. Ethos is independent C++ code, not a kernel-verified
proof assistant. Source pins and recorded binary hashes do not establish
reproducible compiler output, CPC rule soundness or authenticated build provenance.

- [CVC5 CPC semantics and incomplete proofs](https://cvc5.github.io/docs/cvc5-1.3.1/proofs/output_cpc.html).
- [Pinned Ethos manual: reference binding and conclusion responsibility](https://github.com/cvc5/ethos/blob/7059fc50a12d1efa6c075163d445f34edceab1ae/user_manual.md).
- [CVC5 1.3.1 checker compatibility and source pins](https://github.com/cvc5/cvc5/blob/ea1b484fa54bfe56c0f8b3ac90a6e3e2f46441e7/contrib/get-ethos-checker).
- [LFSC trust-step limitations](https://cvc5.github.io/docs/cvc5-1.3.1/proofs/output_lfsc.html).
