# Formal assurance reporting and release decision

Authority: the owner's 2026-10-13 formal verification mandate, particularly §§2,
10, 16–19 and 23. Existing requirements and domain contracts remain canonical;
this report does not redefine them. `SYS-VERIFY-002` owns this reporting boundary.

## Reproduce

Use Python 3.13 and `formal/system/requirements-verification.txt` (Z3 4.13.3.0,
PyYAML 6.0.3) in an isolated environment. Initialize the exact Mobile gitlink.

```sh
python scripts/verification-report.py --output /absolute/new-directory
python scripts/test-verification-report.py
python scripts/verification-report.py --output /absolute/another-new-directory --require-release
```

The last command exits 2 for the currently BLOCKED release. Exit 1 means failed
execution, stale evidence, missing tooling or unsupported source. Exit 0 without
`--require-release` means report production and its narrow proof execution worked;
it is expressly not release approval. Output directories must not already exist
and must resolve outside the source checkout, including through symlinks. This
keeps generated evidence from changing the checkout's own dirty-state fingerprint.

If the registered source set cannot be fingerprinted, the reporter preserves an
UNPROVEN report with a null revision/digest and the failure reason; it does not
invent source identity or start proof producers. Source loss during execution
invalidates all claims. CI archives the report immediately after generation,
before checker installation or subsequent tests. Explicit step budgets fit
inside the larger job budget; if generation itself is terminated, a separately
initialized fallback records BLOCKED/incomplete execution. It is not a proof
report. Platform cancellation or runner loss can still prevent artifact upload.

`report.json` is the generated obligation/traceability matrix; `report.md` renders
it. `sources.json` hashes every registered implementation, model and test and the
report's direct dependencies, binding a dirty working tree explicitly to both its
base commit and content digest. This is not a complete compiler dependency graph.
The requirement register and authorization/recovery contract metadata are retained
as byte snapshots before fingerprinting. Enumeration, receipt admission and report
claims use those same bytes; the final reread only detects persistent source drift.
Certificate requirement mappings also use fingerprinted metadata, and report/SMT
inputs are parsed or copied from the exact byte buffers checked by their hashes.
This prevents transient rereads from attaching a valid formula to a different
claim; it does not defend against a compromised Python process or hostile host.
Exact commit alone is insufficient for dirty-source evidence. `arithmetic/*.smt2`
are the sixteen exact formulas; `result.json`, `replay.json`, logs and output hashes
preserve execution evidence. Replay with pinned `z3 -smt2 FILE.smt2` or Z3Py
`Solver.from_string`. Six negated properties must be UNSAT; six non-vacuity
witnesses, two caller mutation controls and two legacy bug counterexamples must be SAT. The reporter pins that
expectation for each obligation ID independently of the producer's receipt; a
producer that relabels a satisfiable safety formula cannot receive a proof label.
No new release charge,
provider call, database write or deployment is involved.

## Scope and architecture

Existing `inventory.json` and `traceability.json` provide the source-level graph,
reverse mappings, unmapped surfaces, state machines, interfaces and exact Mobile
revision. Inspection confirms the primary implementation boundaries:

| Subsystem | Source authority / implementation | Verification boundary |
|---|---|---|
| Backend | `tdf-hq/stack.yaml`, `tdf-hq/tdf-hq.cabal`, `src/TDF/API.hs`, `src/TDF/Server.hs` | Haskell/Servant, database adapters, workers; no whole-program verifier |
| Web | `tdf-hq-ui/package.json`, `src/`, `functions/` | React/TypeScript and edge routing; tests do not prove runtime behavior |
| Mobile | root `tdf-mobile` gitlink, nested package/API sources | Separate repository, exact pin only; no native runtime refinement |
| Data | `tdf-hq/sql/`, reviewed production migration manifest | PostgreSQL transaction/trigger semantics trusted; finite runtime probes complement proofs |
| Payments | `TDF/Commerce`, provider handlers and workers | Library arithmetic proof excludes provider workflows and remote behavior |
| Media/messaging | Drive/S3 adapters, music workers, Meta/WhatsApp handlers | Providers, credentials, delivery and revocation remain environmental boundaries |
| Deployment | `ops/hetzner/`, `.github/workflows/`, Cloudflare web configuration | Runtime/image/source correspondence and coordinated recovery remain separate obligations |

Requirements → declared state/guards/effects → implementation paths → model/test
paths → source fingerprints are machine-readable already. This addition supplies
explicit proof obligations and the required four-state mathematical status system.
It does not pretend that that graph itself proves a simulation relation.

## Specification and proof coverage

Each registered requirement gets separate implementation and refinement obligations,
initially UNPROVEN. The register's existing PARTIAL/PASS/test labels cannot discharge
either. All `critical: true` requirements are conservatively prioritized P0; other
registered requirements are P1. Fine-grained risk scoring and exhaustive derivation
of each transition's initialization, preservation, termination and liveness
obligations remain UNPROVEN work. Ambiguities remain visible from the authority
register. No blanket NOT_APPLICABLE designation is inferred from missing evidence.

Three authorization predicate obligations are separately reported as
`source-derived-authorization-SMT`; see [auth-predicates.md](auth-predicates.md)
for the exact domain, three supporting cardinality lemmas, five mutation controls
and all translation/endpoint limitations. Their seventeen SMT inputs are retained
in `authorization/`. These do not discharge full requirements or refinement.

Six additional arithmetic obligations are conditional source-derived SMT proofs: capture
and refund safety, admission equivalence and outer-validator sign preconditions, as defined in
[payment-arithmetic.md](payment-arithmetic.md). No finite input bound is substituted:
the formula covers every signed Int64 value satisfying its preconditions. The
translation is trusted, not verified; neither GHC nor complete callers are proved.
Full requirement and refinement statuses remain UNPROVEN even when all six
arithmetic claims are PROVEN. These results concern a library whose adapter has no
discovered active handler caller, not proof of the production payment service.

Model results are deliberately not imported from historical receipts. Existing
TLC/Alloy CI evidence retains its finite configuration, bounds and independent
status. A current model pass would still not discharge implementation refinement.
Metrics stay separate by category; there is no combined “percent correct”.

## Counterexamples and defects

The retained legacy arithmetic controls exhibit `current=1`,
`amount=limit=9223372036854775807`: post-addition admission accepts a wrapped
negative counter. Main already contains the pre-addition correction. This work
makes the sixteen exact solver inputs independently replayable; it introduces no new
application behavior or business rule. The old counterexample is not a claim that
main still has the bug, and SMT serialization is not a proof certificate.

Pending ledger, checkout, migration and worker fixes must be evaluated on their
own reviewed commits. Unmerged PRs are not credited to the analyzed main revision.
The broad mission remains incomplete: no full application proof, exhaustive
specification reconciliation, all-path security proof or deployment qualification
has been established.

## Trusted computing base and methodology

Trusted: restricted Haskell source recognizer and expression translation; Python,
Z3 4.13.3, report generator, source hashes/Git, OS and hardware. Native runtime
claims would additionally trust GHC, libraries, PostgreSQL, generated clients,
JavaScript engines, infrastructure and external providers. No independently checked
UNSAT certificate or compiler refinement is claimed by the ordinary report. Fresh SMT replay checks the
serialized formula, using the same solver; it is not independent solver validation.
No new axioms or admitted theorems are introduced.

The separate [CPC certificate runner](proof-certificates.md) adds CVC5 cross-checks
of all 47 inputs and independent Ethos checks of sixteen formula obligations
(fourteen distinct inputs). Its receipt links certificate obligations to the
source-derived report. Complete requirements and refinement remain UNPROVEN;
this report's release decision is unchanged.

Primary methodological sources: [Z3 bitvector semantics](https://microsoft.github.io/z3guide/docs/theories/Bitvectors/),
[Z3 satisfiability results](https://microsoft.github.io/z3guide/docs/logic/basiccommands/),
and [Lamport's data refinement explanation](https://lamport.azurewebsites.net/tla/tutorial/session11-1.html).
These support the method choice, not evidence that this repository is correct.
Haskell's unsupported full runtime semantics are why the claim is explicitly
conditional on the restricted translation.

## Continuous verification and release governance

The existing event-operations workflow runs on every PR, main push and weekly
Monday 05:23 UTC schedule. Its specification job now produces this report and
archives the complete evidence even when a later check fails. Each SMT query has
a 30-second timeout in the ordinary Z3 report; its proof/control subprocesses
have 180-second deadlines. The separate CPC step has the longer generation and
checker budgets documented in `proof-certificates.md`. CI's
job deadline is the outer limit. Unknown, unsupported source, timeouts and changed
fingerprints never become PROVEN. Every run conservatively regenerates all four
arithmetic, three authorization predicate and four recovery-time obligations; no unsafe incremental dependency assumption is made.

Release decision: **BLOCKED**. When the requirement register is readable, every unresolved critical requirement is enumerated
in `release.criticalUnprovenRequirements`; accepted risks is empty. This initial
reporter intentionally cannot issue READY or consume unauthenticated approval JSON.
Implementing owner-authenticated, exact-artifact risk acceptance is future work,
not an invitation to change statuses to PROVEN. Ordinary report CI can pass while
release remains blocked, allowing remediation PRs to proceed.

The Hetzner planner already sets `executionAllowed: false`; it now explicitly
lists formal assurance/risk acceptance among its remaining gates. The repository
has no guarded production executor enforcing every release prerequisite. This
command and the non-executing planner do not establish a server-side protection
against manual SSH deployment, nor a GitHub required-check configuration. Those
remain governance gaps; no deployment is performed under this report.

Continuation: review each critical requirement's precise transition obligations,
prove supported implementation fragments, validate model refinements, retain
counterexamples, repair real defects, and attach fresh exact-artifact evidence.
Re-run after relevant changes. Human risk acceptance, where allowed by the owner,
must enumerate all accepted UNPROVEN properties and assumptions without changing
their mathematical status.

## Recovery-time evidence

[Recovery expiry contracts](recovery-expiry-arithmetic.md) add four source-derived
conditional proofs for `ID-SESSION-003`. Their separate category cannot discharge
the full recovery transaction or implementation refinement. The PostgreSQL CI
job also exercises the actual time CHECK over finite boundary values; those
probes validate assumptions and remain test evidence, never universal proofs.

## Receipt admission and replay

[Receipt admission](receipt-admission.md) specifies the consumer boundary for all
47 SMT inputs. Authorization and recovery require independent consumer expectation
inventories, source/manifest correspondence, fresh solver replay, complete witnesses
and controls, and successful producer execution before PROVEN labels are admitted.
Supporting lemmas are downgraded with failed suites. Arithmetic REFUTED labels also
require a fresh SAT replay of a retained negated contract, including partial failed
suites. Raw producer text alone cannot establish those report statuses. This still
trusts translation and Z3; same-solver replay is not an independent certificate.
