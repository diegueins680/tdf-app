# Payment arithmetic correspondence

Authority: accepted embedded ADR-0200 in
`docs/payments/ecuador-payment-platform-audit-2026-09-09.md`, invariants 1 and 6 and its
State model paragraph (integer minor units and cumulative capture/refund bounds); ADR 0102
(integer minor-unit snapshots), ADR 0104 (balanced immutable financial entries), and the
current audit's explicit integrity mandate. ADR 0101 governs provider evidence/refund workflow;
it is not the source of the numeric cumulative-bound requirement.
Implementation: `tdf-hq/src/TDF/Commerce/StateMachine.hs`; caller:
`tdf-hq/src/TDF/Commerce/PaymentIntentStore.hs:transitionPaymentIntent`.

Call-site search found no current HTTP handler/worker importing that persistence adapter.
The claim is about the implemented library/adapter, not active provider-handler arithmetic.
Production payment path correspondence remains open; this repair does not activate a future path.

## Counterexample and correction

With `M = 9223372036854775807`, current count 1 and increment M, old capture/refund helpers
compute `-9223372036854775808`; their post-addition upper-bound guard accepts it. The actual
Stack/GHC9.10.3 module returned `Right` with that negative field. Existing PostgreSQL CHECKs
reject negative persistence, so this is an invalid successful transition and downstream failure,
not evidence that negative money reached production storage.

The repaired admission checks `increment > ceiling - current` before the addition. Lifecycle
validation supplies nonnegative fields, making that subtraction representable for all Int64
nonnegative current/ceiling values, including current greater than ceiling. Valid exact-maximum
captures/refunds remain accepted. No monetary limit or provider assumption was narrowed.

Ledger accumulation now converts each Int64 entry to `Integer` before grouping/summing.
The old `[M,M,2]` false balance and large valid compensating sums are regressions. This helper
has no discovered runtime caller; SQL ledger correctness is a separate open obligation.

## Machine-checked claims

`scripts/verify-payment-arithmetic.py` recognizes each complete helper's supported source shape,
extracts its two guards and numeric update, parses a restricted expression grammar, and lowers
the actual extracted expressions to signed 64-bit Z3 bitvectors. Unsupported syntax or changed
record-update shape fails; it does not substitute a handwritten known-good arithmetic function.

For all `current, ceiling >= 0` in signed Int64 and every signed Int64 increment:

1. If the extracted guards admit the update, the stored result equals the exact sum,
   is greater than current, and is at most ceiling.
2. Admission is equivalent to `increment > 0 && exactSum <= ceiling`.

Exact sum uses sign-extended 65-bit operands: addition of two signed 64-bit numbers always fits
in signed 65 bits. Each negated claim must be UNSAT. Two positive SAT witnesses per helper
establish non-vacuity, including exact maximum. Restoring the old post-addition guards must
produce the specified SAT counterexample. Mutating either arithmetic update or source shape
must fail. The solver uses a 30-second limit; unknown, timeout or tool failure is inconclusive
and exits unsuccessfully.

These are source-derived solver proofs of the recognized arithmetic fragments over the full
64-bit domain, conditional on the trusted translation. They are not bounded random tests, but
also not a mechanically verified GHC translation or whole-program refinement proof.
No proof certificate is independently checked; Z3 is trusted.

The outer lifecycle validator's nonnegative guards and the selector of authorized/total/captured
ceilings discharge the arithmetic precondition by manual source inspection. Its full dispatch,
state-consistency policy, caller authentication, locking/transactions, provider signatures,
concurrent history, retries and liveness remain outside this proof. No environment fairness
assumption is needed for this terminating pure arithmetic fragment.

The actual-module `PaymentArithmeticProbe.hs` checks 400 transition cases against an independent
Integer oracle and 512 ledger sequences. Hspec regressions cover exact bug vectors and maximum
successful amounts. These are execution/correspondence evidence, not a proof of all compiler or
database behavior. SQL CHECKs at the canonical migration additionally reject negative/excess
counters; no new migration is required.

## Trusted computing base

The fail-closed source recognizer/restricted AST translator, Python3.13, Z3-solver4.13.3.0,
signed Int64 semantics, Stack/GHC9.10.3 for execution, machine/OS, and source hashing are trusted.
Haskell specifies modular fixed-width arithmetic in
[Data.Int](https://www.haskell.org/definition/haskell2010.pdf); Z3's signed bitvector interpretation
is described in its [official guide](https://microsoft.github.io/z3guide/docs/theories/Bitvectors/).
`Integer` allocation/resource exhaustion is outside the mathematical ledger-sum claim.

Record source SHA256 and solver version from the command's JSON together with CI revision,
logs and tests. Re-run after source, translator, toolchain, caller precondition or configuration
changes; historical green results do not certify a later implementation.
