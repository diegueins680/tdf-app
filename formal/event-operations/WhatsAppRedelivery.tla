---- MODULE WhatsAppRedelivery ----
(* MSG-WHATSAPP-REDELIVERY-001: inbound WhatsApp messages of one number, each *)
(* delivered by the provider any number of times, possibly at once. A STOP    *)
(* message withdraws consent; an enrolment message is answered with a reply;  *)
(* the number may subscribe again between deliveries. A delivery looks for    *)
(* the stored message, stores it if absent, applies what it asks for and      *)
(* then, for an enrolment message, registers the sender (idempotent, so not  *)
(* modelled) and replies after claiming the reply. The process may stop after *)
(* any committed step.                                                        *)
EXTENDS Naturals
CONSTANTS Deliveries, Stops, Enrols, MaxSubscriptions,
          FirstDeliveryOnly, UniqueStore, StoreWithEffect, ClaimReply
VARIABLES consent, rows, applied, replies, subscriptions, pc, msg, claimed, renewed
vars == <<consent, rows, applied, replies, subscriptions, pc, msg, claimed, renewed>>

Messages == Stops \cup Enrols
Stored(m) == rows[m] > 0

Init == /\ consent = TRUE /\ subscriptions = 0
        /\ rows = [m \in Messages |-> 0]
        /\ applied = [m \in Messages |-> 0]
        /\ replies = [m \in Messages |-> 0]
        /\ pc = [d \in Deliveries |-> "idle"]
        /\ msg \in [Deliveries -> Messages]
        /\ claimed = [m \in Messages |-> FALSE]
        /\ renewed = FALSE

Subscribe == /\ subscriptions < MaxSubscriptions
             /\ consent' = TRUE /\ subscriptions' = subscriptions + 1 /\ renewed' = TRUE
             /\ UNCHANGED <<rows, applied, replies, pc, msg, claimed>>

(* renewed records a subscription not yet answered by a newly stored STOP.   *)
Apply(m, storing) ==
  /\ applied' = [applied EXCEPT ![m] = @ + 1]
  /\ consent' = IF m \in Stops THEN FALSE ELSE consent
  /\ renewed' = IF m \in Stops /\ storing THEN FALSE ELSE renewed

(* The delivery reads whether the message is already stored.                 *)
Look(d) ==
  /\ pc[d] = "idle"
  /\ pc' = [pc EXCEPT ![d] = IF Stored(msg[d]) THEN "found" ELSE "absent"]
  /\ UNCHANGED <<consent, rows, applied, replies, subscriptions, msg, claimed, renewed>>

(* It found the message stored: a redelivery.                                *)
Redelivered(d) ==
  /\ pc[d] = "found"
  /\ IF FirstDeliveryOnly
       THEN UNCHANGED <<applied, consent, renewed>>
       ELSE Apply(msg[d], FALSE)
  /\ pc' = [pc EXCEPT ![d] = "reply"]
  /\ UNCHANGED <<rows, replies, subscriptions, msg, claimed>>

(* It stores the message. A second copy is refused when storage is unique,   *)
(* and that delivery ends having changed nothing.                            *)
Store(d) ==
  /\ pc[d] = "absent"
  /\ IF Stored(msg[d]) /\ UniqueStore
       THEN /\ pc' = [pc EXCEPT ![d] = "ended"]
            /\ UNCHANGED <<rows, applied, consent, renewed>>
       ELSE /\ rows' = [rows EXCEPT ![msg[d]] = @ + 1]
            /\ IF StoreWithEffect
                 THEN Apply(msg[d], TRUE) /\ pc' = [pc EXCEPT ![d] = "reply"]
                 ELSE UNCHANGED <<applied, consent, renewed>> /\ pc' = [pc EXCEPT ![d] = "stored"]
  /\ UNCHANGED <<replies, subscriptions, msg, claimed>>

(* Only when the effect is not part of the storing transaction.              *)
EffectLater(d) ==
  /\ pc[d] = "stored"
  /\ Apply(msg[d], TRUE)
  /\ pc' = [pc EXCEPT ![d] = "reply"]
  /\ UNCHANGED <<rows, replies, subscriptions, msg, claimed>>

(* One committed step takes the claim; the reply is sent by its holder. Any  *)
(* delivery of the message may take it, so a delivery that stopped before    *)
(* claiming does not prevent a later one from replying.                      *)
Claim(d) ==
  /\ pc[d] = "reply"
  /\ IF msg[d] \in Enrols /\ (ClaimReply => ~claimed[msg[d]])
       THEN /\ claimed' = [claimed EXCEPT ![msg[d]] = TRUE]
            /\ pc' = [pc EXCEPT ![d] = "send"]
       ELSE /\ pc' = [pc EXCEPT ![d] = "ended"]
            /\ UNCHANGED claimed
  /\ UNCHANGED <<consent, rows, applied, replies, subscriptions, msg, renewed>>

Send(d) ==
  /\ pc[d] = "send"
  /\ replies' = [replies EXCEPT ![msg[d]] = @ + 1]
  /\ pc' = [pc EXCEPT ![d] = "ended"]
  /\ UNCHANGED <<consent, rows, applied, subscriptions, msg, claimed, renewed>>

(* The process stops between two committed steps.                            *)
Stop(d) ==
  /\ pc[d] \in {"found", "absent", "stored", "reply", "send"}
  /\ pc' = [pc EXCEPT ![d] = "ended"]
  /\ UNCHANGED <<consent, rows, applied, replies, subscriptions, msg, claimed, renewed>>

Next == Subscribe \/ \E d \in Deliveries :
          Look(d) \/ Redelivered(d) \/ Store(d) \/ EffectLater(d) \/ Claim(d) \/ Send(d) \/ Stop(d)
Spec == Init /\ [][Next]_vars

TypeOK == /\ consent \in BOOLEAN /\ subscriptions \in 0..MaxSubscriptions
          /\ msg \in [Deliveries -> Messages] /\ claimed \in [Messages -> BOOLEAN]
          /\ renewed \in BOOLEAN
          /\ pc \in [Deliveries -> {"idle", "found", "absent", "stored", "reply", "send", "ended"}]

(* A message is stored once.                                                 *)
StoredOnce == \A m \in Messages : rows[m] <= 1
(* What a message asks for is applied at most once, however often it is      *)
(* delivered.                                                               *)
AppliedOnce == \A m \in Messages : applied[m] <= 1
(* After a new subscription, consent stays until a STOP is newly stored: an  *)
(* earlier STOP delivered again cannot undo it.                              *)
LaterSubscriptionKept == renewed => consent
(* An enrolment message is answered at most once.                            *)
RepliedOnce == \A m \in Messages : replies[m] <= 1
(* A stored STOP has been applied; storing it and stopping cannot lose it.   *)
StoredStopApplied ==
  \A m \in Stops : (Stored(m) /\ \A d \in Deliveries : msg[d] = m => pc[d] # "stored")
                     => applied[m] >= 1
====
