------------------------- MODULE MusicAssetProcessing -------------------------
(* Processing of one uploaded music asset by leased worker runs
   (MEDIA-MUSIC-ASSET-001; scripts/run-music-release-worker-once.sh).

   One processing job, two worker processes that may carry the same worker
   name (names are reused across restarts), one source object whose bytes
   either match the upload evidence or not. A process claims the job, works,
   and finishes by promoting the asset (bytes verified) or failing the
   attempt. Leases expire; an expired lease lets another process reclaim the
   job. Every write of a working process goes through the lease guard.

   Guards, each removed by one negative configuration:
     Fence           the guard compares the attempt the process claimed with
                     the job's current attempt (a fencing token), so a
                     process whose job was reclaimed cannot write
     ClaimOnlyExpired a running job is reclaimed only after its lease expired
     VerifyChecksum  promotion requires the downloaded bytes to match the
                     upload's SHA-256 evidence *)
EXTENDS Naturals, FiniteSets

CONSTANTS Fence, ClaimOnlyExpired, VerifyChecksum, MaxAttempts

Processes == {"p1", "p2"}

VARIABLES
  status,     \* job: queued | running | retry | succeeded | dead_letter
  attempt,    \* job attempt counter
  owner,      \* process holding the job, or "none"
  phase,      \* per process: idle | working
  token,      \* per process: the attempt it claimed
  lease,      \* per process: its own lease has not expired
  leaseLive,  \* the job row's lease (locked_at within the ttl)
  asset,      \* uploaded | valid
  bytesMatch, \* the stored bytes match the upload evidence
  doneToken   \* attempt that wrote the last completion, or 0

vars == <<status, attempt, owner, phase, token, lease, leaseLive, asset, bytesMatch, doneToken>>

TypeOK ==
  /\ status \in {"queued", "running", "retry", "succeeded", "dead_letter"}
  /\ attempt \in 0..MaxAttempts
  /\ owner \in Processes \cup {"none"}
  /\ phase \in [Processes -> {"idle", "working"}]
  /\ token \in [Processes -> 0..MaxAttempts]
  /\ lease \in [Processes -> BOOLEAN]
  /\ leaseLive \in BOOLEAN
  /\ asset \in {"uploaded", "valid"}
  /\ bytesMatch \in BOOLEAN
  /\ doneToken \in 0..MaxAttempts

Init ==
  /\ status = "queued"
  /\ attempt = 0
  /\ owner = "none"
  /\ phase = [p \in Processes |-> "idle"]
  /\ token = [p \in Processes |-> 0]
  /\ lease = [p \in Processes |-> FALSE]
  /\ leaseLive = FALSE
  /\ asset = "uploaded"
  /\ bytesMatch \in BOOLEAN
  /\ doneToken = 0

Claimable ==
  \/ status \in {"queued", "retry"}
  \/ status = "running" /\ (~ClaimOnlyExpired \/ ~leaseLive)

Claim(p) ==
  /\ phase[p] = "idle" /\ Claimable /\ attempt < MaxAttempts
  /\ status' = "running"
  /\ attempt' = attempt + 1
  /\ owner' = p
  /\ phase' = [phase EXCEPT ![p] = "working"]
  /\ token' = [token EXCEPT ![p] = attempt + 1]
  /\ lease' = [lease EXCEPT ![p] = TRUE]
  /\ leaseLive' = TRUE
  /\ UNCHANGED <<asset, bytesMatch, doneToken>>

Expire(p) ==
  /\ phase[p] = "working" /\ lease[p]
  /\ lease' = [lease EXCEPT ![p] = FALSE]
  /\ leaseLive' = IF owner = p THEN FALSE ELSE leaseLive
  /\ UNCHANGED <<status, attempt, owner, phase, token, asset, bytesMatch, doneToken>>

(* The lease guard reads the job row: still running under this process's
   name, with this attempt when fenced, and with a live lease. Both processes
   carry the same worker name, so without the fence a reclaimed job's fresh
   lease also satisfies the stale process. *)
Guard(p) ==
  /\ status = "running"
  /\ (~Fence \/ token[p] = attempt)
  /\ leaseLive

(* A process whose guard fails stops without writing. *)
Fenced(p) ==
  /\ phase[p] = "working" /\ ~Guard(p)
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ lease' = [lease EXCEPT ![p] = FALSE]
  /\ UNCHANGED <<status, attempt, owner, token, leaseLive, asset, bytesMatch, doneToken>>

Promote(p) ==
  /\ phase[p] = "working" /\ Guard(p)
  /\ bytesMatch \/ ~VerifyChecksum
  /\ asset' = "valid"
  /\ status' = "succeeded"
  /\ doneToken' = token[p]
  /\ owner' = "none"
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ lease' = [lease EXCEPT ![p] = FALSE]
  /\ leaseLive' = FALSE
  /\ UNCHANGED <<attempt, token, bytesMatch>>

Fail(p) ==
  /\ phase[p] = "working" /\ Guard(p)
  /\ ~bytesMatch
  /\ status' = IF attempt < MaxAttempts THEN "retry" ELSE "dead_letter"
  /\ doneToken' = token[p]
  /\ owner' = "none"
  /\ phase' = [phase EXCEPT ![p] = "idle"]
  /\ lease' = [lease EXCEPT ![p] = FALSE]
  /\ leaseLive' = FALSE
  /\ UNCHANGED <<attempt, token, asset, bytesMatch>>

Next == \E p \in Processes : Claim(p) \/ Expire(p) \/ Fenced(p) \/ Promote(p) \/ Fail(p)

Spec == Init /\ [][Next]_vars

(* An asset is promoted only when its bytes match the upload evidence. *)
PromotedOnlyVerified == asset = "valid" => bytesMatch

(* A completion is written by the attempt that currently owns the job. *)
CompletionIsCurrent == status \in {"succeeded", "retry", "dead_letter"} => doneToken = attempt

(* A working process whose own lease has not expired is the current attempt:
   nobody reclaims a live job. *)
LiveLeaseIsCurrent == \A p \in Processes : phase[p] = "working" /\ lease[p] => token[p] = attempt

(* Two processes never both hold a live lease. *)
OneLiveLease == Cardinality({p \in Processes : phase[p] = "working" /\ lease[p]}) <= 1

=============================================================================
