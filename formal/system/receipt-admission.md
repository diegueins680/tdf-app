# SMT receipt admission — SYS-VERIFY-002

Authority: the registered requirement that failed, incomplete or stale formal
execution must never be admitted as passing evidence, and the owner's mandatory
mathematical-honesty rules. This specializes the existing requirement; it changes
no application business rule or previously specified mathematical property.

## Boundary and contracts

The relevant implementation is `scripts/verification-report.py`, specifically
`replay_smt`, `admit_receipt`, `replay_arithmetic` and `run`. Producers remain
`verify-payment-arithmetic.py`, `verify-auth-predicates.py` and
`verify-recovery-expiry.py`. Their translation assumptions and proof scopes are
documented separately. An artifact hash establishes byte identity, not truth.

For a suite `g`, let `E_g` map each required SMT input ID to its independently
specified SAT/UNSAT result. The consumer defines these inventories without
importing producer code or reading its expected results as authority:

| Suite | Inputs | Expected UNSAT | Expected SAT |
|---|---:|---:|---:|
| Payment arithmetic | 10 | 4 contracts | 4 witnesses + 2 legacy counterexamples |
| Authorization | 17 | 3 contracts + 3 algebra lemmas | 6 witnesses + 5 mutations |
| Recovery arithmetic | 14 | 4 contracts | 5 witnesses + 5 mutations |

Let `A_g` be the retained artifact inventory, `S` the report's source snapshot,
and `R_g(i)` the result of reparsing input `i` into a fresh Z3 solver. Define:

```
Complete(g) = IDs(A_g) = domain(E_g) AND no duplicate IDs
FilesMatch(g) = every filename is ID + ".smt2", directly retained, and hash matches
SourceMatch(g,S) = revision and dirty flag match S, the source-manifest digest
                  is valid, reviewed source pins match S, all required sources
                  are present, and every producer source hash agrees with S
ReplayPass(g) = for every i in domain(E_g), R_g(i) = E_g(i)
```

For authorization/recovery the report may admit a source-derived PROVEN label
only if the producer exits zero, its success flag is the Boolean `true`, its
claim/lemma inventory is complete and claims success, and all four predicates
above hold. A string such as `"true"` does not meet that contract. The report
still checks its entire registered source fingerprint after all executions;
source drift makes every affected claim stale and UNPROVEN.

For payment arithmetic, the producer's `source` must name exactly
`tdf-hq/src/TDF/Commerce/StateMachine.hs`, and its `sha256` must match that
entry in the initial report fingerprint. Both successful-suite admission and
partial counterexample admission enforce this binding before replay. A stale
receipt cannot prove or refute the current source even if its formulas replay
successfully or a concurrent source edit was restored before the final check.

A fresh SAT result for a retained negated contract establishes a counterexample
within that formula's semantics and trusted translation boundary. It becomes
REFUTED there, even if the producer called it PROVEN. Other claims and supporting
lemmas cannot remain PROVEN when a required witness/control or producer fails.
Unknown, missing or malformed evidence is UNPROVEN. Raw producer REFUTED text
also cannot substitute for a fresh SAT result. Partial failed arithmetic suites
may retain independently replayed counterexamples, but cannot establish PROVEN
claims without their complete witnesses and controls.

The full requirement and its implementation/refinement obligations remain
**UNPROVEN**. These Python admission contracts have controlled regression
evidence, not a universal machine-checked implementation proof. The eleven
existing conditional source-derived contracts remain a different assurance
category; this work adds no new application proof count.

## Preserved counterexample and correction

[receipt-admission-counterexample.json](receipt-admission-counterexample.json)
records a synthetic producer fault against reporter revision
`2d8a99bf51bacd08a69f25673beeef95259aa70c`. The real producer first generates its
normal inputs. The fault control replaces only `strict-admin.smt2` with
`(assert true)` and recomputes its receipt hash, while leaving the producer's
successful exit, expected UNSAT and PROVEN labels unchanged. The formula is SAT,
but the historical reporter returns success and admits three authorization
PROVEN labels. Release remains BLOCKED even in that historical control.

This is conclusive evidence of an evidence-admission failure under the stated
producer-fault model. It is **not** a counterexample to actual application
authorization: the synthetic formula deliberately lacks the source translation.
The corrected reporter replays the file, retains the SAT result, refuses suite
success, marks that synthetic encoded contract REFUTED and the other authorization
claims/lemmas UNPROVEN. Unaffected suites retain independently checked evidence.

Additional controls cover relabelled proof expectations, forged refutations,
failed witnesses, unknown results, failed producer exits, string success flags,
source/revision mismatches, missing/duplicate IDs, hashes, unsafe filenames and
symlinked artifacts. The existing actual arithmetic source mutation still yields
a retained SAT counterexample after independent partial-suite replay.

## Reproduction and CI

Install `formal/system/requirements-verification.txt` in Python 3.13, initialize
the committed Mobile gitlink, then run:

```sh
python scripts/test-verification-report.py
python scripts/verification-report.py --output /absolute/new-directory --require-release
```

The first command includes the controlled producer fault; its temporary report
must fail proof execution. The second uses actual producers and requires fresh
replay of all 41 inputs. With all narrow contracts established, it still returns
exit 2 because full release evidence is missing. A proof/tool failure returns 1.

`authorization-replay.json`, `recovery-replay.json`, the payment replay and any
`arithmetic-counterexample-replay.json` retain consumer results. Raw producer
receipts, all inputs, provenance and failure logs remain archived. The existing
PR/main/weekly formal workflow runs these scripts and archives failed attempts.
No production deployment or gate bypass is introduced.

## Trusted computing base and limitations

Trusted: source recognizers/translators, reviewed source bindings, independent
expectation inventories and admission code, Python, Z3 4.13.3, Git/SHA-256,
OS/hardware and the stated Haskell/PostgreSQL semantics. This is same-solver
consumer replay, not an independent proof checker or checked UNSAT certificate.
An incorrect source-to-formula translation can still generate an irrelevant
UNSAT formula. No compiler refinement, hostile-runner authentication, general
artifact-store integrity or whole-application correctness is established here.


Authorization and recovery producers retain one byte snapshot of each input,
including the reviewed contract. Fingerprints and translated UTF-8 text derive
from those same buffers. A final reread rejects persistent drift; a transient
edit restored before that check cannot change the formulas' source snapshot.
Regression controls inject differing second-read text and require translation
of the original fingerprinted bytes. This binding is local evidence integrity,
not a trusted execution or hostile-host attestation.
