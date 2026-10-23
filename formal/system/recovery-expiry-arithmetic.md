# Recovery time arithmetic — ID-SESSION-003

This checks four **conditional source-derived contracts** connecting the actual
Haskell time predicate and issuance addition to the migration's PostgreSQL time
constraint. Full ID-SESSION-003 implementation/refinement remains **UNPROVEN**.
No transaction, current clock, credential binding or HTTP property is promoted by
these arithmetic results. No application behavior or expiry policy is changed.

## Authority, source and formal contracts

[identity-lifecycle.md](identity-lifecycle.md#recovery-expiry--id-session-003)
requires a 900-second lifetime and half-open interval `[issued, expires)`. The
current requirement register identifies this as TDF policy, with bound metadata
and time sampled after lock waits. Supporting tests alone do not supply authority.
The source files are:

- `tdf-hq/src/TDF/ServerAuth.hs`: `recoveryWindowValid`, and the expiry expression
  passed to `PersistInt64` in `createPasswordResetToken`.
- `tdf-hq/sql/2026-10-04_auth_recovery_expiry.sql`: both `BIGINT NOT NULL` columns
  and `auth_recovery_time_window`.

For **every** signed-64-bit issued time `i`, stored expiry `e` and sampled current
second `n`, define exact mathematical policy:

```
P(i,e,n) = i >= 0 AND e = i + 900 AND i <= n < e
```

Let `H` be the translated Haskell predicate, `D` successful admission by only the
SQL time constraint on non-null values, and `E(i)` the actual wrapping Int64
issuance addition. Required obligations:

| ID | Formula |
|---|---|
| window-equivalence | H(i,e,n) iff P(i,e,n) |
| schema-correspondence | D(i,e) iff H(i,e,i) |
| issuance-admission | D(i,E(i)) iff 0 <= i <= maxInt64 - 900 |
| issuance-exact | D(i,E(i)) implies signed(E(i)) = exact(i + 900) |

The second contract proves agreement of the two **translated expressions** at the
issuance boundary; it is not a refinement theorem for the Haskell/database system.
Foreign keys, primary keys, table installation, other constraints and transaction
success can still refuse insertion. `D` never denotes successful whole insertion.
Issuance after `maxInt64-900` wraps in Haskell; its time constraint refuses it.
No exploitable current expiry defect is inferred from the intermediate wrap.

## Translation and arithmetic semantics

Inputs and ordinary comparisons use signed 64-bit bitvectors. Haskell's
`toInteger expires - toInteger issued` is represented by subtracting sign-extended
65-bit inputs; every such difference fits in signed 65 bits, so this is exact.
The policy's addition of 900 also fits in 65 bits. Issuance uses 64-bit addition,
matching the source rather than replacing it with mathematical addition.

SQL bigint subtraction may throw on overflow. Its encoding requires both the
translated CHECK and representability of the exact subtraction in signed64. An
error and a false CHECK both refuse admission; no SQL evaluation order or exact
error code is assumed. Only the recognized conjunction is supported; SQL OR,
functions, casts, nullable time columns and unsupported syntax fail closed.
`CHECK` permitting NULL cannot be ignored: the translator explicitly requires
both `NOT NULL` declarations. Runtime code/schema correspondence is still a
trusted assumption, not a proof that production installed this constraint.

Whole ServerAuth.hs, migration, policy document and Stack/Cabal manifests are
pinned in [recovery-expiry-arithmetic.json](recovery-expiry-arithmetic.json).
Source/hash changes require semantic review before updating those pins. The
recognizer consumes the complete pure predicate and identifies the issuance
expression; it does not prove the surrounding effectful function. The main
report also fingerprints every registered requirement source before and after
execution. No reduced input range or favorable epoch precondition is substituted
for the full three-input Int64 domain.

## Evidence, controls and reproduction

With `formal/system/requirements-verification.txt` installed:

```sh
python scripts/verify-recovery-expiry.py --output /absolute/new-recovery-evidence
python scripts/test-recovery-expiry-verifier.py
python scripts/verification-report.py --output /absolute/new-report --require-release
```

The standalone runner requires four UNSAT negated contracts, five SAT witnesses,
and five SAT source-mutation counterexamples. Witnesses include epoch zero, the
largest representable valid window, its last permitted second, and refusal of
the next (overflowing) issuance. Mutations admit deadline equality, ignore future
issuance, change validator/SQL lifetimes, or change the issuance increment. They
are synthetic regressions, not claims that main contains those defects.

All fourteen exact SMT-LIB inputs are replayed and retained with source hashes,
commit/dirty status, tool versions, results and SAT valuations. The output
directory must be new. Unknown, timeout, source drift, unsupported syntax or a
failed control never becomes PROVEN. A SAT negated contract is REFUTED within the
stated translation boundary. `z3 -smt2 FILE.smt2` reproduces each expected result;
these inputs and solver outputs are not independently checked proof certificates.
Same-solver replay is not an independent proof checker.

The assurance report keeps these four obligations in
`source-derived-recovery-SMT`, separate from arithmetic/authorization evidence
and complete requirement/refinement statuses. PR, main and weekly CI regenerate
all inputs and archive failures. `--require-release` remains blocked (exit2).

## Trusted computing base and remaining obligations

Trusted: restricted translation and reviewed bindings/imports, Python, Z3 4.13.3,
Git/SHA-256, OS/hardware; Haskell Int64/toInteger and PostgreSQL checked-bigint and
NOT NULL/CHECK semantics. No verified compiler, SQL evaluator or translation
certificate is claimed. Defined finite scalar inputs are assumed; process failure
and partial/bottom evaluation are outside these contracts.

Primary references: [PostgreSQL integer ranges and errors](https://www.postgresql.org/docs/17/datatype-numeric.html),
[CHECK and NOT NULL semantics](https://www.postgresql.org/docs/17/ddl-constraints.html),
and [Z3 bitvectors](https://microsoft.github.io/z3guide/docs/theories/Bitvectors/).
These justify the chosen semantics; they do not prove the application correct.

Still UNPROVEN here: after-lock sampling, clock synchronization/rollback, token
entropy/secrecy, single use, credential binding, atomic password/session effects,
rollback after failed issuance, migration drift, deployment fleet compatibility,
HTTP dispatch and external delivery. Existing bounded RecoveryExpiry models and
runtime tests remain separate evidence. Release is **BLOCKED**, with no accepted
unproven critical risk. The broader mission remains incomplete.

## Complementary database checks

`sh scripts/test-recovery-time-postgres.sh` creates and removes its own PostgreSQL
16.10 Docker container, pinned by manifest digest
`sha256:029660641a0cfc575b14f336ba448fb8a75fd595d42e1fa316b9fb4378742297`.
It executes the actual extracted time CHECK in a temporary table, with non-null bigint columns, over signed boundary pairs, generated wrapped
expiries and NULLs. Expected admission is computed independently with Python
integers. A separate overflowing subtraction must raise the PostgreSQL range
error. These are finite tests of trusted semantics, not full migration testing or
a proof of runtime correspondence. Existing recovery migration tests cover a
different boundary and remain required.

For an already provisioned disposable database, generate the same probes with
`python scripts/recovery-time-boundaries.py > /tmp/recovery-time-probes.sql`, then
run `psql -X -v ON_ERROR_STOP=1 -d DISPOSABLE_DB -f /tmp/recovery-time-probes.sql`.
The script uses only a temporary table inside a rolled-back transaction. The
existing required `postgres-foundation` CI job runs the Docker probes on every
configured PR, main and scheduled execution.

## Source review after main integration

Main `2781ddc2a3c9fd661684742550918d653afdc0cf` adds
`TDF.IndexDeclarationSpec` to the Cabal test-module list. The complete diff
changes no recovery function, time constraint, authority document or compiler
configuration. The Cabal pin was refreshed after reviewing that registration;
the integrated candidate requires fresh solver execution and evidence hashes.
This review does not establish the correctness of the newly merged index
migration or promote its separate full requirement to PROVEN.

## Verification-runner defect and correction

Hosted run `38673959518` on candidate `4ca89a07e828cc459cf40d16e58c9cc73877f99c`
passed the arithmetic job but failed the database probes before executing SQL.
Unix-socket readiness can observe the pinned image's temporary initialization
server; that server stops before the final PostgreSQL process starts. The runner
now waits for an authenticated `SELECT 1` on container-loopback TCP and executes
probes through that same endpoint. The pinned entrypoint starts its temporary
server with `listen_addresses=''`, so it cannot satisfy this readiness check.
A controlled bootstrap/final-server regression fixture rejects the prior socket
behavior. This repairs evidence collection, not an application correctness bug.
The failed hosted run remains part of the audit history; tests and proofs must
be regenerated for the corrected runner revision.

## Prepared integration source review, 2026-10-22

The local merge of `580a74560405112ae817748f4d9e796a8ba2a002` with prepared
retrieval parent `c063bb5d806f73c9b4e305d1ccef53a353db6f33` changes only the
Cabal manifest among the pinned recovery inputs. Its complete diff registers
`TDF.Courses.PublicMetadata` and `TDF.PublicUrl` in the executable and test suite,
and adds `RagIndexPrivacySpec`, `CoursePublicMetadataSpec` and
`MusicPublicationSpec` to test modules. Dependencies and compiler configuration
are unchanged. `ServerAuth.hs`, the recovery migration and the identity contract
remain byte-identical. The Cabal hash is refreshed after that review; recovery
claims, translation and negative controls remain unchanged and require fresh
execution. This preparation is not independent approval or protected admission.
