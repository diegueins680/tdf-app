# Restore crash admission checkpoint

The clean implementation revision is `bea8cd79147254fd41f139a03732155c5a487040`.
It retains shipped Mobile `c1d6031b36e52dd365e699163900e65adb0621e0`.

The helper now rejects visible orphan containers and unresolved durable creation
markers under the host lock before inspecting production. Marker creation is
exclusive and fsynced before the external Docker request. Unknown completion
retains the marker; only fully admitted target removal permits clearing it.
Independent review found the delayed-create crash window and the implementation
was repaired before this live rehearsal.

The exact-source repository and full formal gates passed:58 positive TLC
configurations,130 expected negative controls, one PlusCal regeneration with13
integrity tests and Alloy2SAT/13UNSAT within declared bounds. The formal receipt
binds identical starting/final source fingerprints. Independent source review passed15
Python and13 Node cases; this is distinct from GitHub approval. The parent43ff
hosted CI and exact-head approval are retained as historical receipts. They do
not approve or certify later commits.

The actual online restoration at03:28UTC passed652 relation counts and preserved
159 historical migration entries. Ten pending migrations applied twice to the
isolated copy, with169 final entries and canonical schema verification. Existing
reported controls were unchanged; new flags and the bank-transfer provider remain
disabled. The isolate and pending marker were removed before PASS. Production
remained backend645f56fcc44f81609fbfd0e03d683b40376ce77a with159 migrations. Private
archives and raw diagnostics remain on the host; these receipts contain metadata,
counts and hashes only.

The earlier123 SourceTarget model control produced an expression error, not its
required safety violation, and therefore failed. Parentheses repaired the Boolean
assignment atbea. Preserve this failed-control distinction when reading the later
passing model receipt; a tool error is never a successful negative control.

[The model boundary](../../restore-isolation.md) describes two runs, delayed create
completion, operator recovery assumptions and five controlled mutations. No
fairness, universal correctness or executable refinement proof is claimed.

This is not deployment clearance. Coordinated assets/secret/off-host recovery,
application canary, canonical routine release execution and broad semantic
conformance remain open. No production data mutation, charge, feature activation,
root/Mobile merge or application deployment occurred in this checkpoint.
