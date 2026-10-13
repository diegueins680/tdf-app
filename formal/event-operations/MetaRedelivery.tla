---- MODULE MetaRedelivery ----
(* MSG-META-REDELIVERY-001: one inbound Instagram or Facebook message. The    *)
(* provider delivers it any number of times, possibly with other text, and    *)
(* may deliver its deletion any number of times, in any order. A reply worker *)
(* considers a stored message that is pending, not answered and not deleted,  *)
(* and answers it, puts it on hold for a person, or records a failure.        *)
EXTENDS Naturals
CONSTANTS Texts, None, MaxDeliveries, MaxDeletions, MaxClock,
          KeepReplyState, KeepText, KeepDeletionTime
VARIABLES stored, text, firstText, status, replied, deletedAt, firstDeletedAt,
          deletedFirst, considered, deliveries, deletions, clock
vars == <<stored, text, firstText, status, replied, deletedAt, firstDeletedAt,
          deletedFirst, considered, deliveries, deletions, clock>>

Init == /\ stored = FALSE /\ text = None /\ firstText = None
        /\ status = "pending" /\ replied = FALSE
        /\ deletedAt = None /\ firstDeletedAt = None /\ deletedFirst = FALSE
        /\ considered = 0 /\ deliveries = 0 /\ deletions = 0 /\ clock = 0

Tick == /\ clock < MaxClock /\ clock' = clock + 1
        /\ UNCHANGED <<stored, text, firstText, status, replied, deletedAt,
                       firstDeletedAt, deletedFirst, considered, deliveries, deletions>>

(* A delivery of the message with some text.                                 *)
Deliver(t) ==
  /\ deliveries < MaxDeliveries /\ deliveries' = deliveries + 1
  /\ IF ~stored
       THEN /\ stored' = TRUE /\ text' = t /\ firstText' = t
            /\ UNCHANGED status
       ELSE /\ UNCHANGED <<stored, firstText>>
            /\ text' = IF KeepText THEN text ELSE t
            /\ status' = IF KeepReplyState THEN status ELSE "pending"
  /\ UNCHANGED <<replied, deletedAt, firstDeletedAt, deletedFirst, considered, deletions, clock>>

(* A delivery of the deletion. Before the message, it stores a row without   *)
(* text.                                                                     *)
Delete ==
  /\ deletions < MaxDeletions /\ deletions' = deletions + 1
  /\ deletedFirst' = (deletedFirst \/ ~stored)
  /\ stored' = TRUE
  /\ deletedAt' = IF deletedAt = None \/ ~KeepDeletionTime THEN clock ELSE deletedAt
  /\ firstDeletedAt' = IF firstDeletedAt = None THEN clock ELSE firstDeletedAt
  /\ UNCHANGED <<text, firstText, status, replied, considered, deliveries, clock>>

Consider ==
  /\ stored /\ status = "pending" /\ ~replied /\ deletedAt = None
  /\ considered' = considered + 1
  /\ \E outcome \in {"sent", "hold", "error"} :
       /\ status' = outcome
       /\ replied' = (outcome = "sent")
  /\ UNCHANGED <<stored, text, firstText, deletedAt, firstDeletedAt, deletedFirst,
                 deliveries, deletions, clock>>

Next == Tick \/ Delete \/ Consider \/ \E t \in Texts : Deliver(t)
Spec == Init /\ [][Next]_vars

TypeOK == /\ stored \in BOOLEAN /\ replied \in BOOLEAN /\ deletedFirst \in BOOLEAN
          /\ text \in Texts \cup {None} /\ firstText \in Texts \cup {None}
          /\ status \in {"pending", "sent", "hold", "error"}
          /\ deletedAt \in (0..MaxClock) \cup {None}
          /\ firstDeletedAt \in (0..MaxClock) \cup {None}

(* The worker considers a message once: a message it answered, held for a    *)
(* person or failed on is not taken up again because it was delivered again. *)
(* The administrator's release of a held message is outside this model.      *)
ConsideredOnce == considered <= 1
(* A stored message keeps the text it was stored with.                       *)
TextKept == text = firstText
(* A message whose deletion arrived first never gains text.                  *)
NoTextAfterDeletion == deletedFirst => text = None
(* The deletion keeps its first time.                                        *)
DeletionTimeKept == deletedAt = firstDeletedAt
====
