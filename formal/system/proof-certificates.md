# Independent SMT and CPC evidence

Authority: the owner's formal-verification mandate §§8, 12, 18–19 and
`SYS-VERIFY-002`. This supplements the conditional contracts in
[auth-predicates.md](auth-predicates.md),
[recovery-expiry-arithmetic.md](recovery-expiry-arithmetic.md) and
[payment-arithmetic.md](payment-arithmetic.md). Business requirements,
preconditions and source bindings are unchanged.

## Claims and trust boundary

CVC5 1.4.2 independently rechecks **all 47 exact SMT inputs** retained by a fresh
assurance report: 16 UNSAT contracts/lemmas and 31 SAT witnesses/controls. Solver
agreement is separate from certificate checking. Neither proves that a trusted
source translator correctly implements Haskell, SQL or HTTP semantics.

Sixteen UNSAT obligations (fourteen distinct inputs) have CPC refutations checked by
Ethos 0.2.5 with the matching, pinned CVC5 CPC signatures:

| Suite | Formula IDs | Certificate claim |
|---|---|---|
| authorization | strict-admin, module-access, ingestion-admission | Refutation of each negated source-derived predicate contract |
| authorization | cardinality-nonnegative, cardinality-zero, cardinality-sum | Refutation of each negated supporting algebra lemma |
| arithmetic | capture-safety, capture-admission, capture-caller-precondition, refund-safety, refund-admission, refund-caller-precondition | Refutation of each negated source-derived capture/refund contract |
| recovery | window-equivalence, schema-correspondence, issuance-admission, issuance-exact | Refutation of each negated source-derived arithmetic contract |

These certificate claims concern the exact SMT domains. Six payment arithmetic/caller, three
authorization and four recovery contracts retain their **conditional source-derived** scope.
Supporting lemmas remain separate. No full implementation or refinement obligation
changes status. The 123 complete requirements remain UNPROVEN; release remains
BLOCKED with no accepted risks.

## Tool qualification and historical failures

The CVC5 1.4.2 / Ethos 0.2.5 update regenerates the original eight certificates
and adds all six remaining obligations using unchanged signed-Int64 formulas.
Longer proof generation/checking limits accommodate the larger payment
certificates; no invariant, numeric domain or query is weakened. Resource limits
constrain computation, not the quantified domain. A timeout still yields UNPROVEN.

The upstream release notes also report solver soundness fixes. This motivates
fresh qualification with the current matching checker/signature pair; it does
not establish that any earlier TDF formula or certificate was refuted. A direct
review found the prior certificates' signed-comparison elimination rewrite
declarations unchanged apart from formatting. That footprint check is not a
soundness proof of the complete solver, checker or signature.

Historical CVC5 1.3.1 capture-safety generation timed out at 30 seconds, and the
missing recovery certificates at 15 seconds. Internal eager bit-blasting timed
out at 60 seconds; the CaDiCaL mode refused proof support. Integer conversion
required an INT_BLASTER trust step and was rejected. CVC5 1.4.2 payment attempts
also exceeded a 30-second budget; a macro proof needed an unjustified
MACRO_SR_PRED_INTRO step and was refused. Initial LFSC output contained
unjustified ACI_NORM/EVALUATE steps. None of those failed, incomplete or
trust-dependent attempts is accepted as a certificate or called a refutation.

## Admission and correspondence

`verify-proof-certificates.py` requires a successful, fresh assurance report with
the same source fingerprint, revision, Mobile identity and dirty state as the
current checkout. Consumer-owned expectations come from the reporter. Exact SMT
bytes and hashes are retained in a new evidence directory. Ordinary CVC5 replay
has a 30-second limit and 45-second process deadline. Certificate generation has
a 120-second solver limit and 135-second process deadline; independent checking
has a 600-second process deadline. CVC5 ignores Z3's known
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


Capture/refund safety share exactly the same SMT bytes; their admission problems
also share exactly the same bytes. A fresh run independently checks fourteen
unique refutations and reuses those two validated formulas for the other
obligation IDs. Reuse is keyed by complete input bytes, not names, expected
results or historical receipts. Each ID retains its own input hash, requirement
mapping and an explicit `reusedFrom` link to the checked proof. The cache starts
empty in each run. Altering even one assertion forces a new certificate; real
checker controls demonstrate that a satisfiable alteration cannot inherit a
PROVEN label. Input, proof, signature and tool hashes are rechecked before success.
Metrics distinguish sixteen obligation results from fourteen unique checked inputs.

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
certificate step allows 35 minutes, with a separate eight-minute checker build.
All step budgets total 97 minutes inside a 110-minute job; assurance uploads
precede the checker work and retain an incomplete fallback on generation timeout.
No existing check is removed. It retains SMT/CPC files, solver/checker streams,
hashes, installation/build receipts and failures. All 47 cross-checks and
sixteen supported certificate obligations must succeed.
Passing this job grants no release permission.

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

- [CVC5 CPC semantics and incomplete proofs](https://cvc5.github.io/docs/cvc5-1.4.2/proofs/output_cpc.html).
- [Pinned Ethos manual: reference binding and conclusion responsibility](https://github.com/cvc5/ethos/blob/08e4aa40c4f8a6e00833f10e8d8985777e424027/user_manual.md).
- [CVC5 1.4.2 checker compatibility and source pins](https://github.com/cvc5/cvc5/blob/282d5e0c84baf49c780704c532f87ba66d8b513d/contrib/get-ethos-checker).
- [CVC5 release changes and soundness fixes](https://github.com/cvc5/cvc5/blob/282d5e0c84baf49c780704c532f87ba66d8b513d/NEWS.md).
- [LFSC trust-step limitations](https://cvc5.github.io/docs/cvc5-1.3.1/proofs/output_lfsc.html).
