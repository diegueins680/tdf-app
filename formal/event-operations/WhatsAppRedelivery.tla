---- MODULE WhatsAppRedelivery ----
(* MSG-WHATSAPP-REDELIVERY-001: inbound WhatsApp messages of one number, each *)
(* delivered by the provider any number of times, possibly at once. A STOP    *)
(* message withdraws consent; an enrolment message is answered with a reply;  *)
(* the number may subscribe again between deliveries. A delivery looks for    *)
(* the stored message, stores it if absent, applies what it asks for and      *)
(* then replies. The process may stop after any committed step.               *)
EXTENDS Naturals
CONSTANTS Deliveries, Stops, Enrols, MaxSubscriptions,
          FirstDeliveryOnly, UniqueStore, StoreWithEffect, ReplyFirstOnly
VARIABLES consent, rows, applied, replies, subscriptions, pc, msg, first
vars == <<consent, rows, applied, replies, subscriptions, pc, msg, first>>

Messages == Stops \cup Enrols
Stored(m) == rows[m] > 0

Init == /\ consent = TRUE /\ subscriptions = 0
        /\ rows = [m \in Messages |-> 0]
        /\ applied = [m \in Messages |-> 0]
        /\ replies = [m \in Messages |-> 0]
        /\ pc = [d \in Deliveries |-> "idle"]
        /\ msg \in [Deliveries -> Messages]
        /\ first = [d \in Deliveries |-> FALSE]

Subscribe == /\ subscriptions < MaxSubscriptions
             /\ consent' = TRUE /\ subscriptions' = subscriptions + 1
             /\ UNCHANGED <<rows, applied, replies, pc, msg, first>>

Apply(m) == /\ applied' = [applied EXCEPT ![m] = @ + 1]
            /\ consent' = IF m \in Stops THEN FALSE ELSE consent

(* The delivery reads whether the message is already stored.                 *)
Look(d) ==
  /\ pc[d] = "idle"
  /\ pc' = [pc EXCEPT ![d] = IF Stored(msg[d]) THEN "found" ELSE "absent"]
  /\ UNCHANGED <<consent, rows, applied, replies, subscriptions, msg, first>>

(* It found the message stored: a redelivery.                                *)
Redelivered(d) ==
  /\ pc[d] = "found"
  /\ IF FirstDeliveryOnly
       THEN UNCHANGED <<applied, consent>>
       ELSE Apply(msg[d])
  /\ pc' = [pc EXCEPT ![d] = "reply"]
  /\ UNCHANGED <<rows, replies, subscriptions, msg, first>>

(* It stores the message. A second copy is refused when storage is unique,   *)
(* and that delivery ends having changed nothing.                            *)
Store(d) ==
  /\ pc[d] = "absent"
  /\ IF Stored(msg[d]) /\ UniqueStore
       THEN /\ pc' = [pc EXCEPT ![d] = "ended"]
            /\ UNCHANGED <<rows, applied, consent, first>>
       ELSE /\ rows' = [rows EXCEPT ![msg[d]] = @ + 1]
            /\ first' = [first EXCEPT ![d] = TRUE]
            /\ IF StoreWithEffect
                 THEN Apply(msg[d]) /\ pc' = [pc EXCEPT ![d] = "reply"]
                 ELSE UNCHANGED <<applied, consent>> /\ pc' = [pc EXCEPT ![d] = "stored"]
  /\ UNCHANGED <<replies, subscriptions, msg>>

(* Only when the effect is not part of the storing transaction.              *)
EffectLater(d) ==
  /\ pc[d] = "stored"
  /\ Apply(msg[d])
  /\ pc' = [pc EXCEPT ![d] = "reply"]
  /\ UNCHANGED <<rows, replies, subscriptions, msg, first>>

Reply(d) ==
  /\ pc[d] = "reply"
  /\ replies' = [replies EXCEPT ![msg[d]] =
       IF msg[d] \in Enrols /\ (ReplyFirstOnly => first[d]) THEN @ + 1 ELSE @]
  /\ pc' = [pc EXCEPT ![d] = "ended"]
  /\ UNCHANGED <<consent, rows, applied, subscriptions, msg, first>>

(* The process stops between two committed steps.                            *)
Stop(d) ==
  /\ pc[d] \in {"found", "absent", "stored", "reply"}
  /\ pc' = [pc EXCEPT ![d] = "ended"]
  /\ UNCHANGED <<consent, rows, applied, replies, subscriptions, msg, first>>

Next == Subscribe \/ \E d \in Deliveries :
          Look(d) \/ Redelivered(d) \/ Store(d) \/ EffectLater(d) \/ Reply(d) \/ Stop(d)
Spec == Init /\ [][Next]_vars

TypeOK == /\ consent \in BOOLEAN /\ subscriptions \in 0..MaxSubscriptions
          /\ msg \in [Deliveries -> Messages] /\ first \in [Deliveries -> BOOLEAN]
          /\ pc \in [Deliveries -> {"idle", "found", "absent", "stored", "reply", "ended"}]

(* A message is stored once.                                                 *)
StoredOnce == \A m \in Messages : rows[m] <= 1
(* What a message asks for is applied at most once, however often it is      *)
(* delivered, so an earlier STOP cannot undo a later subscription.           *)
AppliedOnce == \A m \in Messages : applied[m] <= 1
(* An enrolment message is answered at most once.                            *)
RepliedOnce == \A m \in Messages : replies[m] <= 1
(* A stored STOP has been applied; storing it and stopping cannot lose it.   *)
StoredStopApplied ==
  \A m \in Stops : (Stored(m) /\ \A d \in Deliveries : msg[d] = m => pc[d] # "stored")
                     => applied[m] >= 1
====
