--------------------------- MODULE MigrationLedger ---------------------------
(* Production migration correspondence (DEPLOY-MIGRATION-001):
   scripts/lib/migration-contract.mjs (loadMigrationContract,
   reconcileMigrationLedger: release preparation) and buildMigrationBatchSql in
   scripts/lib/production-release.mjs (the psql batch the release and the
   entrypoint run, recording each applied migration in tdf_schema_migration).

   A manifest of N migrations in order; each has a current source version and
   a set of explicitly compatible applied versions. The ledger holds the
   version recorded as applied, or 0. Effects are the migration versions whose
   SQL ran. A batch run walks the known manifest in order: it halts on an
   applied entry whose recorded version the candidate does not allow, skips
   other applied entries and applies pending ones. The source may change an
   entry's version (with or without declaring the old one compatible), and an
   older image may be rolled back (its manifest does not know the newest
   entry). Preparation reconciles the ledger with the candidate manifest. The
   batch's advisory lock (one run at a time) is below the model: one runner.

   Guards, each removed by one negative configuration:
     AtomicRecord         the ledger row commits together with the migration's effects
     BatchChecksumGuard   the batch halts on an applied entry whose version is not allowed
     PrepareChecksumGuard preparation rejects such an entry
     RejectUnknown        preparation rejects a ledger row absent from the manifest *)
EXTENDS Naturals, FiniteSets

CONSTANTS N, Versions, MaxRuns, AtomicRecord, BatchChecksumGuard, PrepareChecksumGuard, RejectUnknown

Ids == 1..N

VARIABLES
  manifest,   \* [Ids -> Versions]: the candidate's version of each migration
  compat,     \* [Ids -> SUBSET Versions]: explicitly compatible applied versions
  known,      \* the ids the running image's manifest contains (rollback drops N)
  ledger,     \* [Ids -> Versions \cup {0}]
  effects,    \* set of <<id, version>> whose SQL ran
  applied,    \* [Ids -> Nat]: how many times an entry's SQL ran
  pc,         \* 0 idle, else the entry the run is processing (N+1: done)
  unrecorded, \* effects applied, ledger row not yet written (non-atomic layout), or 0
  runsLeft,   \* bound on batch starts
  prepared,   \* "none" | "accepted" | "rejected"
  halted      \* a run halted on a checksum mismatch

vars == <<manifest, compat, known, ledger, effects, applied, pc, unrecorded, runsLeft, prepared, halted>>

Allowed(i) == {manifest[i]} \cup compat[i]
Idle == pc = 0

TypeOK ==
  /\ manifest \in [Ids -> Versions] /\ compat \in [Ids -> SUBSET Versions]
  /\ known \subseteq Ids /\ ledger \in [Ids -> Versions \cup {0}]
  /\ effects \subseteq Ids \X Versions /\ applied \in [Ids -> 0..MaxRuns]
  /\ pc \in 0..(N+1) /\ unrecorded \in Ids \cup {0}
  /\ runsLeft \in 0..MaxRuns /\ prepared \in {"none", "accepted", "rejected"} /\ halted \in BOOLEAN

Init ==
  /\ manifest \in [Ids -> Versions] /\ compat = [i \in Ids |-> {}]
  /\ known = Ids /\ ledger = [i \in Ids |-> 0] /\ effects = {} /\ applied = [i \in Ids |-> 0]
  /\ pc = 0 /\ unrecorded = 0 /\ runsLeft = MaxRuns /\ prepared = "none" /\ halted = FALSE

Start == Idle /\ runsLeft > 0 /\ pc' = 1 /\ runsLeft' = runsLeft - 1
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, unrecorded, prepared, halted>>

(* The entry at pc is not in this image's manifest, or is applied with an allowed version. *)
Skip ==
  /\ pc \in Ids /\ unrecorded = 0
  /\ \/ pc \notin known
     \/ ledger[pc] # 0 /\ (ledger[pc] \in Allowed(pc) \/ ~BatchChecksumGuard)
  /\ pc' = pc + 1
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, unrecorded, runsLeft, prepared, halted>>

(* The checksum check halts the run before anything else happens. *)
Halt ==
  /\ pc \in Ids /\ unrecorded = 0 /\ BatchChecksumGuard
  /\ pc \in known /\ ledger[pc] # 0 /\ ledger[pc] \notin Allowed(pc)
  /\ pc' = 0 /\ halted' = TRUE
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, unrecorded, runsLeft, prepared>>

(* A pending entry is applied; with the atomic row the ledger is written in the same step. *)
Apply ==
  /\ pc \in Ids /\ unrecorded = 0 /\ pc \in known /\ ledger[pc] = 0
  /\ effects' = effects \cup {<<pc, manifest[pc]>>} /\ applied' = [applied EXCEPT ![pc] = applied[pc] + 1]
  /\ (IF AtomicRecord
        THEN ledger' = [ledger EXCEPT ![pc] = manifest[pc]] /\ unrecorded' = 0 /\ pc' = pc + 1
        ELSE ledger' = ledger /\ unrecorded' = pc /\ pc' = pc)
  /\ UNCHANGED <<manifest, compat, known, runsLeft, prepared, halted>>

(* Without the atomic row the ledger insert is a later statement... *)
Record ==
  /\ unrecorded # 0
  /\ ledger' = [ledger EXCEPT ![unrecorded] = manifest[unrecorded]]
  /\ unrecorded' = 0 /\ pc' = pc + 1
  /\ UNCHANGED <<manifest, compat, known, effects, applied, runsLeft, prepared, halted>>

(* ...and the process may die between the two. *)
Crash == unrecorded # 0 /\ unrecorded' = 0 /\ pc' = 0
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, runsLeft, prepared, halted>>

Finish == pc = N + 1 /\ pc' = 0
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, unrecorded, runsLeft, prepared, halted>>

(* The candidate's source of an entry changes; the old version may be declared compatible. *)
ChangeSource(i, v, declare) ==
  /\ Idle /\ v # manifest[i]
  /\ manifest' = [manifest EXCEPT ![i] = v]
  /\ compat' = (IF declare THEN [compat EXCEPT ![i] = compat[i] \cup {manifest[i]}] ELSE compat)
  /\ prepared' = "none"
  /\ UNCHANGED <<known, ledger, effects, applied, pc, unrecorded, runsLeft, halted>>

(* An older image is rolled back (its manifest lacks N) or the current one returns. *)
Rollback == Idle /\ N \in known /\ known' = Ids \ {N} /\ prepared' = "none"
  /\ UNCHANGED <<manifest, compat, ledger, effects, applied, pc, unrecorded, runsLeft, halted>>
Restore == Idle /\ N \notin known /\ known' = Ids /\ prepared' = "none"
  /\ UNCHANGED <<manifest, compat, ledger, effects, applied, pc, unrecorded, runsLeft, halted>>

(* reconcileMigrationLedger on the candidate (known) manifest. *)
Consistent ==
  \A i \in Ids : ledger[i] # 0 =>
    /\ (i \in known \/ ~RejectUnknown)
    /\ (i \in known => (ledger[i] \in Allowed(i) \/ ~PrepareChecksumGuard))
Prepare == Idle /\ prepared = "none"
  /\ prepared' = (IF Consistent THEN "accepted" ELSE "rejected")
  /\ UNCHANGED <<manifest, compat, known, ledger, effects, applied, pc, unrecorded, runsLeft, halted>>

Done == runsLeft = 0 /\ Idle /\ UNCHANGED vars

Next ==
  \/ Start \/ Skip \/ Halt \/ Apply \/ Record \/ Crash \/ Finish
  \/ \E i \in Ids, v \in Versions, d \in BOOLEAN : ChangeSource(i, v, d)
  \/ Rollback \/ Restore \/ Prepare \/ Done

Spec == Init /\ [][Next]_vars

(* Sanity: a ledger row never claims effects that did not run (by construction). *)
LedgerImpliesEffects == \A i \in Ids : ledger[i] # 0 => <<i, ledger[i]>> \in effects

(* Effects are recorded, except while a non-atomic run is still writing the row. *)
EffectsRecorded == \A e \in effects : ledger[e[1]] # 0 \/ unrecorded = e[1]

(* A migration's SQL runs at most once (a crash under the non-atomic layout
   makes the next run re-apply it). *)
SingleApplication == \A i \in Ids : applied[i] <= 1

(* A run never walks past an applied entry whose recorded version the candidate does not allow. *)
NoRunPastDrift ==
  \A i \in Ids : (pc > i /\ i \in known /\ ledger[i] # 0) => ledger[i] \in Allowed(i)

(* Accepted preparation means every ledger row is known and allowed. *)
PreparationSound ==
  prepared = "accepted" => \A i \in Ids : ledger[i] # 0 => (i \in known /\ ledger[i] \in Allowed(i))

=============================================================================
