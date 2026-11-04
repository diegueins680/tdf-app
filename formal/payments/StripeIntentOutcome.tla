---- MODULE StripeIntentOutcome ----
(***************************************************************************)
(* PAY-TIP-001 / F-07. Local record of one Stripe PaymentIntent outcome    *)
(* (artist tip, course registration or marketplace order) under webhook    *)
(* deliveries that are duplicated, reordered and processed concurrently.   *)
(*                                                                         *)
(* Each delivery first passes the event-id dedupe read and then, in a      *)
(* separate transaction, applies its outcome. The dedupe row is inserted   *)
(* after dispatch, so concurrent duplicates both dispatch; the model       *)
(* therefore lets every delivery dispatch.                                 *)
(*                                                                         *)
(* ConditionalUpdate = TRUE  : UPDATE ... SET status = target              *)
(*                             WHERE id = ? AND status = 'pending'          *)
(* ConditionalUpdate = FALSE : read the row, then write unconditionally    *)
(*                             when the read observed 'pending' (the code  *)
(*                             before this change).                         *)
(* SingleTerminal = TRUE     : environment assumption that Stripe never     *)
(*                             emits both succeeded and canceled for one    *)
(*                             intent.                                      *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  ConditionalUpdate,
  \* @type: Bool;
  SingleTerminal,
  \* @type: Set(Int);
  Deliveries

VARIABLES
  \* @type: Str;
  status,
  \* @type: Int -> Str;
  kind,
  \* @type: Int -> Str;
  pc,
  \* @type: Int -> Str;
  seen,
  \* @type: Int;
  overwrites

vars == <<status, kind, pc, seen, overwrites>>

Statuses == {"pending", "paid", "failed"}
Kinds == {"succeeded", "canceled"}

Target(d) == IF kind[d] = "succeeded" THEN "paid" ELSE "failed"

Init ==
  /\ status = "pending"
  /\ kind \in [Deliveries -> Kinds]
  /\ SingleTerminal => \A d1, d2 \in Deliveries : kind[d1] = kind[d2]
  /\ pc = [d \in Deliveries |-> "idle"]
  /\ seen = [d \in Deliveries |-> "pending"]
  /\ overwrites = 0

\* Dispatch reads the row (the guard of markArtistTipStatus and siblings).
Read(d) ==
  /\ pc[d] = "idle"
  /\ pc' = [pc EXCEPT ![d] = "read"]
  /\ seen' = [seen EXCEPT ![d] = status]
  /\ UNCHANGED <<status, kind, overwrites>>

\* A later transaction applies the outcome.
Write(d) ==
  /\ pc[d] = "read"
  /\ LET applies == IF ConditionalUpdate THEN status = "pending" ELSE seen[d] = "pending"
         next == IF applies THEN Target(d) ELSE status
     IN /\ status' = next
        /\ overwrites' = IF status /= "pending" /\ next /= status
                           THEN overwrites + 1 ELSE overwrites
  /\ pc' = [pc EXCEPT ![d] = "done"]
  /\ UNCHANGED <<kind, seen>>

Next == \E d \in Deliveries : Read(d) \/ Write(d)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ status \in Statuses
  /\ kind \in [Deliveries -> Kinds]
  /\ pc \in [Deliveries -> {"idle", "read", "done"}]
  /\ seen \in [Deliveries -> Statuses]
  /\ overwrites \in Nat

(* Safety: a recorded terminal outcome is never replaced, and it is always *)
(* the outcome of some delivered event.                                     *)
NoOverwrite == overwrites = 0
Justified ==
  /\ status = "paid" => \E d \in Deliveries : kind[d] = "succeeded"
  /\ status = "failed" => \E d \in Deliveries : kind[d] = "canceled"
Safe == NoOverwrite /\ Justified

(* Inductive strengthening. With conditional updates the invariant needs   *)
(* no environment assumption. With read-then-write it holds only because   *)
(* every delivery targets the same outcome.                                 *)
IndInv ==
  /\ TypeOK
  /\ Safe
  /\ ~ConditionalUpdate =>
       /\ SingleTerminal
       /\ \A d1, d2 \in Deliveries : kind[d1] = kind[d2]

(* Constant initialisers for Apalache (--cinit). Any set of up to six       *)
(* concurrent deliveries.                                                   *)
CInitConditional ==
  /\ ConditionalUpdate = TRUE
  /\ SingleTerminal \in BOOLEAN
  /\ Deliveries \in SUBSET (1..6)

CInitLegacyAssumed ==
  /\ ConditionalUpdate = FALSE
  /\ SingleTerminal = TRUE
  /\ Deliveries \in SUBSET (1..6)

CInitLegacyUnassumed ==
  /\ ConditionalUpdate = FALSE
  /\ SingleTerminal = FALSE
  /\ Deliveries \in SUBSET (1..6)
====
