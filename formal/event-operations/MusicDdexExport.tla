---------------------------- MODULE MusicDdexExport ----------------------------
(* DDEX export generation by leased worker runs (MEDIA-MUSIC-DDEX-001;
   tdf-hq/src/TDF/Server/MusicReleaseDDEX.hs and
   scripts/run-music-release-worker-once.sh).

   One export row (music_ddex_export.status) and its one generate_ddex job
   (music_processing_job.status), two worker processes that may share a
   worker name, and an environment flag "rules": the operation checker,
   the snapshot hash and both registry rows still admit the export. The API
   commits the export with its job; the legacy case (export queued, no job)
   is an initial state the authorized replay repairs. A process claims the
   job, re-checks the rules and marks the export generating (gate 1), renders
   and uploads outside the database, re-checks the rules and marks the export
   valid (gate 2), then closes the job. Any step can fail; a failed attempt
   is retried until MaxAttempts and then dead-lettered, which fails the
   export. Leases expire; an expired lease lets another process reclaim the
   job. Every database write of a working process goes through the lease
   guard, whose fencing token is the claimed attempt.

   Guards, each removed by one negative configuration:
     Fence          the guard compares the claimed attempt with the job's
                    current attempt, so a reclaimed process cannot write
     ValidIsFinal   a valid export is never regenerated or failed again
                    (one rule applied at two sites: gate 1 excludes valid and
                    mark_failed excludes valid)
     CommitGate     gate 2 re-checks the rules before marking valid
     RepairQueued   the replay recreates a missing job only for a queued
                    export and never for a failed one
     AtomicDeadLetter the dead letter and the export's failed status are one
                    write *)
EXTENDS Naturals, FiniteSets

CONSTANTS Fence, ValidIsFinal, CommitGate, RepairQueued, AtomicDeadLetter,
          MaxAttempts, MaxClaims

Processes == {"p1", "p2"}
None == "none"

ExportStates == {"queued", "generating", "validation_failed", "valid", "failed"}
JobStates == {None, "queued", "running", "retry", "succeeded", "dead_letter"}
Phases == {"idle", "claimed", "generating", "uploaded", "validated"}

VARIABLES
  export,     \* music_ddex_export.status
  job,        \* music_processing_job.status, or none (legacy stranded export)
  attempt,    \* job attempt counter
  owner,      \* process holding the job, or none
  leaseLive,  \* the job row's lease (locked_at within the ttl)
  phase,      \* per process
  token,      \* per process: the attempt it claimed
  rules,      \* the checker, snapshot and registry rows admit the export
  everValid,  \* history: the export was marked valid once
  validUnderRules, \* history: the last valid mark happened while rules held
  staleWrite  \* history: a database write by a process whose attempt is stale

vars == <<export, job, attempt, owner, leaseLive, phase, token, rules,
          everValid, validUnderRules, staleWrite>>

TypeOK ==
  /\ export \in ExportStates
  /\ job \in JobStates
  /\ attempt \in 0..MaxClaims
  /\ owner \in Processes \cup {None}
  /\ leaseLive \in BOOLEAN
  /\ phase \in [Processes -> Phases]
  /\ token \in [Processes -> 0..MaxClaims]
  /\ rules \in BOOLEAN
  /\ everValid \in BOOLEAN
  /\ validUnderRules \in BOOLEAN
  /\ staleWrite \in BOOLEAN

Init ==
  /\ export = "queued"
  /\ job \in {"queued", None}
  /\ attempt = 0
  /\ owner = None
  /\ leaseLive = FALSE
  /\ phase = [p \in Processes |-> "idle"]
  /\ token = [p \in Processes |-> 0]
  /\ rules \in BOOLEAN
  /\ everValid = FALSE
  /\ validUnderRules = TRUE
  /\ staleWrite = FALSE

(* The lease guard: status running, this owner, the claimed attempt and a
   live lease. Without Fence the guard knows only the (shared) worker name
   and the lease, so a reclaimed process still passes. *)
Guard(p) ==
  /\ job = "running"
  /\ leaseLive
  /\ Fence => (owner = p /\ token[p] = attempt)

Written(p) == staleWrite' = (staleWrite \/ token[p] # attempt)

(* Environment: the rules an export depends on change (registry revoked or
   reactivated, version state changed). *)
Flip ==
  /\ rules' = ~rules
  /\ UNCHANGED <<export, job, attempt, owner, leaseLive, phase, token,
                 everValid, validUnderRules, staleWrite>>

(* The authorized replay of the original request repairs a legacy export
   that has no job. Without RepairQueued it also re-enqueues a failed one. *)
Replay ==
  /\ \/ export = "queued" /\ job = None
     \/ ~RepairQueued /\ export = "failed" /\ job = "dead_letter"
  /\ job' = "queued"
  /\ UNCHANGED <<export, attempt, owner, leaseLive, phase, token, rules,
                 everValid, validUnderRules, staleWrite>>

(* The claim query: a due queued/retry job, or a running job whose lease
   expired. *)
Claim(p) ==
  /\ phase[p] = "idle"
  /\ attempt < MaxClaims
  /\ \/ job \in {"queued", "retry"}
     \/ job = "running" /\ ~leaseLive
  /\ job' = "running"
  /\ attempt' = attempt + 1
  /\ owner' = p
  /\ leaseLive' = TRUE
  /\ phase' = [phase EXCEPT ![p] = "claimed"]
  /\ token' = [token EXCEPT ![p] = attempt + 1]
  /\ UNCHANGED <<export, rules, everValid, validUnderRules, staleWrite>>

Expire ==
  /\ leaseLive
  /\ leaseLive' = FALSE
  /\ UNCHANGED <<export, job, attempt, owner, phase, token, rules,
                 everValid, validUnderRules, staleWrite>>

(* A process dies without writing; its job stays running until the lease
   expires. *)
Crash(p) ==
  /\ phase[p] # "idle"
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ UNCHANGED <<export, job, attempt, owner, leaseLive, token, rules,
                 everValid, validUnderRules, staleWrite>>

(* The guard refuses a write: the process stops without any effect. *)
Fenced(p) ==
  /\ phase[p] # "idle"
  /\ ~Guard(p)
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ UNCHANGED <<export, job, attempt, owner, leaseLive, token, rules,
                 everValid, validUnderRules, staleWrite>>

(* A valid export is recovered as is: the job closes, nothing is rendered. *)
Recover(p) ==
  /\ phase[p] = "claimed"
  /\ Guard(p)
  /\ export = "valid"
  /\ job' = "succeeded"
  /\ owner' = None
  /\ leaseLive' = FALSE
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ Written(p)
  /\ UNCHANGED <<export, attempt, token, rules, everValid, validUnderRules>>

(* Gate 1: the rules hold and the export is not valid (or ValidIsFinal is
   off): the export becomes generating. *)
Gate1(p) ==
  /\ phase[p] = "claimed"
  /\ Guard(p)
  /\ rules
  /\ export \in {"queued", "validation_failed", "generating"}
     \/ (~ValidIsFinal /\ export = "valid")
  /\ export' = "generating"
  /\ phase' = [phase EXCEPT ![p] = "generating"]
  /\ Written(p)
  /\ UNCHANGED <<job, attempt, owner, leaseLive, token, rules, everValid,
                 validUnderRules>>

(* Rendering and uploads happen outside the database. *)
Upload(p) ==
  /\ phase[p] = "generating"
  /\ phase' = [phase EXCEPT ![p] = "uploaded"]
  /\ UNCHANGED <<export, job, attempt, owner, leaseLive, token, rules,
                 everValid, validUnderRules, staleWrite>>

(* Gate 2: the rules are checked again (unless CommitGate is off) and the
   export becomes valid with the package of this attempt. *)
Gate2(p) ==
  /\ phase[p] = "uploaded"
  /\ Guard(p)
  /\ export = "generating"
  /\ CommitGate => rules
  /\ export' = "valid"
  /\ everValid' = TRUE
  /\ validUnderRules' = rules
  /\ phase' = [phase EXCEPT ![p] = "validated"]
  /\ Written(p)
  /\ UNCHANGED <<job, attempt, owner, leaseLive, token, rules>>

Succeed(p) ==
  /\ phase[p] = "validated"
  /\ Guard(p)
  /\ job' = "succeeded"
  /\ owner' = None
  /\ leaseLive' = FALSE
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ Written(p)
  /\ UNCHANGED <<export, attempt, token, rules, everValid, validUnderRules>>

(* mark_failed, for any reason (a gate that refused, a renderer or transfer
   error, a crash handler): the job is retried or dead-lettered and the
   export records the failure unless it is valid or already failed. Flip is
   always enabled, so the model has no terminal state and needs no stutter. *)
NextJob(p) == IF token[p] >= MaxAttempts THEN "dead_letter" ELSE "retry"

Fail(p) ==
  /\ phase[p] \in {"claimed", "generating", "uploaded", "validated"}
  /\ Guard(p)
  /\ job' = NextJob(p)
  /\ owner' = None
  /\ leaseLive' = FALSE
  /\ export' = (IF export = "valid" /\ ValidIsFinal THEN "valid"
                ELSE IF export = "failed" THEN "failed"
                ELSE IF NextJob(p) = "dead_letter" /\ AtomicDeadLetter THEN "failed"
                ELSE "validation_failed")
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ Written(p)
  /\ UNCHANGED <<attempt, token, rules, everValid, validUnderRules>>

Next ==
  \/ Flip
  \/ Replay
  \/ Expire
  \/ \E p \in Processes :
       Claim(p) \/ Crash(p) \/ Fenced(p) \/ Recover(p) \/ Gate1(p)
       \/ Upload(p) \/ Gate2(p) \/ Succeed(p) \/ Fail(p)

Spec == Init /\ [][Next]_vars

(* A valid export stays valid: its package is never regenerated or failed. *)
ValidForever == everValid => export = "valid"

(* Every database write is made by the process holding the current attempt. *)
CurrentWriter == ~staleWrite

(* An export is marked valid only while its rules hold. *)
ValidOnlyUnderRules == export = "valid" => validUnderRules

(* A failed export has exactly its dead-lettered job and gets no new one. *)
FailedIsFinal == export = "failed" => job = "dead_letter"

(* A pending export always has a job the worker can still run; only the
   legacy queued export may lack one until the replay repairs it. *)
PendingHasLiveJob ==
  /\ export = "queued" => job \in {None, "queued", "retry", "running"}
  /\ export \in {"generating", "validation_failed"} => job \in {"queued", "retry", "running"}

=============================================================================
