---- MODULE WhatsAppConsent ----
(* PRIV-WHATSAPP-001: double opt-in for one phone number.                    *)
(* Row fields: consent, req (confirmation_requested_at, 0 = NULL), note,     *)
(* revoked (revoked_at, 0 = NULL). Each action is one committed statement.   *)
(* Staff-captured consent and staff revocation are outside this model.       *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS MaxTime, ResendInterval, Window, UnsentWindow,
          ReplyOrderGuard, WithdrawalGuard, WithdrawalKeepsInterval,
          BindFailureToClaim, KeepUndeliveredPending, ShortUnsentWindow
VARIABLES now, consent, req, note, revoked,
          inflight, delivered, replies,
          actReq, actReply, actRevoked, actAge, actKind, rejectedPending
vars == <<now, consent, req, note, revoked, inflight, delivered, replies,
          actReq, actReply, actRevoked, actAge, actKind, rejectedPending>>

Times == 1..MaxTime
Notes == {"none", "sent", "unsent"}
Withdrawn == revoked # 0 /\ revoked >= req

Init == /\ now = 1 /\ consent = FALSE /\ req = 0 /\ note = "none" /\ revoked = 0
        /\ inflight = {} /\ delivered = {} /\ replies = {}
        /\ actReq = 0 /\ actReply = 0 /\ actRevoked = 0 /\ actAge = 0
        /\ actKind = "none" /\ rejectedPending = FALSE

Tick == /\ now < MaxTime /\ now' = now + 1
        /\ UNCHANGED <<consent, req, note, revoked, inflight, delivered, replies,
                       actReq, actReply, actRevoked, actAge, actKind, rejectedPending>>

(* Public request: one conditional update claims the confirmation message.   *)
Claim == /\ ~consent /\ now \notin inflight
         /\ \/ req = 0
            \/ now > req + ResendInterval
            \/ (note = "unsent" /\ ~Withdrawn)
         /\ req' = now /\ note' = "sent" /\ inflight' = inflight \cup {now}
         /\ rejectedPending' = FALSE
         /\ UNCHANGED <<now, consent, revoked, delivered, replies,
                        actReq, actReply, actRevoked, actAge, actKind>>

(* The provider outcome for the attempt claimed at instant t arrives later.  *)
Accept(t) == /\ t \in inflight
             /\ inflight' = inflight \ {t} /\ delivered' = delivered \cup {t}
             /\ UNCHANGED <<now, consent, req, note, revoked, replies,
                            actReq, actReply, actRevoked, actAge, actKind, rejectedPending>>

Reject(t) ==
  /\ t \in inflight /\ inflight' = inflight \ {t}
  /\ LET matches == ~consent /\ req # 0 /\ note = "sent"
                    /\ (BindFailureToClaim => req = t)
         current == ~consent /\ req = t /\ note = "sent" /\ ~Withdrawn
     IN /\ req' = IF matches /\ ~KeepUndeliveredPending THEN 0 ELSE req
        /\ note' = IF matches THEN "unsent" ELSE note
        /\ rejectedPending' = IF current THEN TRUE ELSE rejectedPending
  /\ UNCHANGED <<now, consent, revoked, delivered, replies,
                 actReq, actReply, actRevoked, actAge, actKind>>

(* The number's holder sends an affirmative keyword stamped by the provider. *)
Reply == /\ replies' = replies \cup {now}
         /\ UNCHANGED <<now, consent, req, note, revoked, inflight, delivered,
                        actReq, actReply, actRevoked, actAge, actKind, rejectedPending>>

(* Webhook delivery of any earlier reply: late, duplicated or replayed.      *)
Confirm(r) ==
  /\ r \in replies /\ ~consent /\ req # 0 /\ note \in {"sent", "unsent"}
  /\ ReplyOrderGuard => req <= r
  /\ WithdrawalGuard => ~Withdrawn
  /\ now <= req + (IF note = "unsent" /\ ShortUnsentWindow THEN UnsentWindow ELSE Window)
  /\ consent' = TRUE /\ note' = "none" /\ revoked' = 0
  /\ actReq' = req /\ actReply' = r /\ actRevoked' = revoked
  /\ actAge' = now - req /\ actKind' = note /\ rejectedPending' = FALSE
  /\ UNCHANGED <<now, req, inflight, delivered, replies>>

(* Public opt-out or inbound STOP. The stored note is caller-supplied text,  *)
(* so it may coincide with either pending marker.                            *)
Withdraw == /\ consent' = FALSE /\ revoked' = now
            /\ req' = IF WithdrawalKeepsInterval THEN req ELSE 0
            /\ note' \in Notes /\ rejectedPending' = FALSE
            /\ UNCHANGED <<now, inflight, delivered, replies,
                           actReq, actReply, actRevoked, actAge, actKind>>

Next == \/ Tick \/ Claim \/ Reply \/ Withdraw
        \/ \E t \in inflight : Accept(t) \/ Reject(t)
        \/ \E r \in replies : Confirm(r)

TypeOK == /\ now \in Times /\ consent \in BOOLEAN /\ req \in 0..MaxTime
          /\ note \in Notes /\ revoked \in 0..MaxTime
          /\ inflight \subseteq Times /\ delivered \subseteq Times /\ replies \subseteq Times

(* Consent exists only through the number's own reply, made no earlier than  *)
(* the request it confirms.                                                  *)
ConfirmedByOwnLaterReply ==
  consent => actReq # 0 /\ actReply \in replies /\ actReply >= actReq

(* A withdrawal made at or after a request defeats that request.             *)
WithdrawalWins == consent => (actRevoked = 0 \/ actRevoked < actReq)

(* Two delivered confirmation requests are more than one interval apart.     *)
OneDeliveredRequestPerInterval ==
  \A a \in delivered : \A b \in delivered :
    a < b => b > a + ResendInterval

(* A request that was never delivered confirms only inside the short window. *)
ConfirmedInsideWindow ==
  consent => actAge <= (IF actKind = "unsent" THEN UnsentWindow ELSE Window)

(* An undelivered current request stays pending for the number to confirm.   *)
UndeliveredStaysPending == rejectedPending => req # 0 /\ note = "unsent"

Spec == Init /\ [][Next]_vars
====
