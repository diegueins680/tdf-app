---- MODULE ProviderInbox ----
(***************************************************************************)
(* PAY-PROVIDER-INBOX-001. One provider event in commerce_provider_event_   *)
(* inbox, processed by workers that claim it with a lease. A lease older    *)
(* than 15 minutes is stale and may be reclaimed by another worker; every   *)
(* claim increments attempt_count. A worker finishes with processed, retry  *)
(* or dead_letter and writes that outcome back.                             *)
(*                                                                         *)
(* Fence = FALSE : before 2026-11-01, the write only required               *)
(*                 processing_status = 'processing'.                        *)
(* Fence = TRUE  : the write also requires attempt_count = the worker's     *)
(*                 claimed attempt.                                          *)
(*                                                                         *)
(* The side effect of processing (payment admission) is outside this        *)
(* model; its idempotency is PAY-RETRY-001 / PAY-CAPTURE-RECON-001.          *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  Fence,
  \* @type: Set(Int);
  Workers,
  \* @type: Int;
  MaxAttempts,
  \* @type: Int;
  MaxClaims

VARIABLES
  \* @type: Str;
  status,         \* pending | processing | retry | processed | dead_letter
  \* @type: Int;
  attempt,        \* attempt_count
  \* @type: Bool;
  stale,          \* the current lease is older than the stale threshold
  \* @type: Int;
  recordedBy,     \* attempt whose worker wrote the current terminal status (0: none)
  \* @type: Int -> Str;
  wpc,            \* idle | working | done
  \* @type: Int -> Int;
  wattempt,       \* attempt the worker claimed
  \* @type: Int -> Str;
  woutcome        \* none | processed | retry | dead_letter

vars == <<status, attempt, stale, recordedBy, wpc, wattempt, woutcome>>

Terminal == {"processed", "dead_letter"}
Outcomes == {"processed", "retry", "dead_letter"}

Init ==
  /\ status = "pending"
  /\ attempt = 0
  /\ stale = FALSE
  /\ recordedBy = 0
  /\ wpc = [w \in Workers |-> "idle"]
  /\ wattempt = [w \in Workers |-> 0]
  /\ woutcome = [w \in Workers |-> "none"]

Claimable ==
  \/ status = "pending"
  \/ status = "retry"
  \/ (status = "processing" /\ stale)

\* claimProviderEvent: conditional UPDATE ... RETURNING attempt_count.
Claim(w) ==
  /\ wpc[w] = "idle"
  /\ Claimable
  /\ attempt < MaxClaims
  /\ status' = "processing"
  /\ attempt' = attempt + 1
  /\ stale' = FALSE
  /\ wpc' = [wpc EXCEPT ![w] = "working"]
  /\ wattempt' = [wattempt EXCEPT ![w] = attempt + 1]
  /\ UNCHANGED <<recordedBy, woutcome>>

\* Fifteen minutes pass while the lease holder is still working.
Expire ==
  /\ status = "processing"
  /\ ~stale
  /\ stale' = TRUE
  /\ UNCHANGED <<status, attempt, recordedBy, wpc, wattempt, woutcome>>

WriteAllowed(w) ==
  /\ status = "processing"
  /\ (~Fence \/ attempt = wattempt[w])

FinalStatus(o) ==
  IF o = "retry" /\ attempt >= MaxAttempts THEN "dead_letter" ELSE o

\* markProviderEventProcessed / Retry / DeadLetter.
Finish(w, o) ==
  /\ wpc[w] = "working"
  /\ wpc' = [wpc EXCEPT ![w] = "done"]
  /\ woutcome' = [woutcome EXCEPT ![w] = o]
  /\ IF WriteAllowed(w)
       THEN /\ status' = FinalStatus(o)
            /\ stale' = FALSE
            /\ recordedBy' = IF FinalStatus(o) \in Terminal THEN attempt ELSE 0
       ELSE UNCHANGED <<status, stale, recordedBy>>
  /\ UNCHANGED <<attempt, wattempt>>

Next ==
  \/ Expire
  \/ \E w \in Workers : Claim(w) \/ \E o \in Outcomes : Finish(w, o)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ status \in {"pending", "processing", "retry", "processed", "dead_letter"}
  /\ attempt \in Nat
  /\ stale \in BOOLEAN
  /\ recordedBy \in Nat
  /\ wpc \in [Workers -> {"idle", "working", "done"}]
  /\ wattempt \in [Workers -> Nat]
  /\ woutcome \in [Workers -> {"none"} \cup Outcomes]

(* A terminal status was written by the holder of the latest lease.        *)
TerminalByLatest == status \in Terminal => recordedBy = attempt

(* A successful outcome of the latest lease holder is never lost.           *)
NoLostSuccess ==
  \A w \in Workers :
    (wpc[w] = "done" /\ woutcome[w] = "processed" /\ wattempt[w] = attempt)
      => status = "processed"

Safe == TerminalByLatest /\ NoLostSuccess

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Fence
  /\ stale => status = "processing"
  /\ \A w \in Workers : wattempt[w] <= attempt
  /\ \A w \in Workers : wpc[w] = "idle" <=> wattempt[w] = 0
  /\ \A w \in Workers : wpc[w] = "done" <=> woutcome[w] /= "none"
  /\ \A w1, w2 \in Workers :
       (w1 /= w2 /\ wpc[w1] /= "idle" /\ wpc[w2] /= "idle") => wattempt[w1] /= wattempt[w2]
  /\ status = "processing" => \E w \in Workers : wpc[w] = "working" /\ wattempt[w] = attempt
  \* The holder of the latest lease is working exactly while the event is processing.
  /\ \A w \in Workers :
       (attempt > 0 /\ wattempt[w] = attempt) => (wpc[w] = "working" <=> status = "processing")
  /\ status = "pending" => attempt = 0
  /\ attempt = 0 => status = "pending"

CInitFence ==
  /\ Fence = TRUE
  /\ Workers \in SUBSET (1..3) /\ Workers /= {}
  /\ MaxAttempts \in 1..8
  /\ MaxClaims \in 1..8

CInitLegacy ==
  /\ Fence = FALSE
  /\ Workers = {1, 2}
  /\ MaxAttempts = 8
  /\ MaxClaims = 4
====
