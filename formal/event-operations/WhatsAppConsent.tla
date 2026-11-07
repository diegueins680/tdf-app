---- MODULE WhatsAppConsent ----
(* PRIV-WHATSAPP-001: double opt-in for one phone number.                    *)
(* Row fields: consent, req (confirmation_requested_at, 0 = NULL), note,     *)
(* revoked (revoked_at, 0 = NULL). Each action is one committed transaction. *)
(* Staff-captured consent is outside this model.                             *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS MaxTime, ResendInterval, Window, UnsentWindow,
          ReplyOrderGuard, WithdrawalGuard, WithdrawalKeepsInterval,
          SettleWithdrawalInstant, BindFailureToClaim, KeepUndeliveredPending,
          ShortUnsentWindow, RetryKeepsInstant
VARIABLES now, consent, req, note, revoked,
          inflight, delivered, replies, anchor, withdrawnSinceRequest,
          actReq, actReply, actWithdrawn, actAge, actAnchorAge, actKind,
          rejectedPending
row == <<consent, req, note, revoked>>
ghost == <<actReq, actReply, actWithdrawn, actAge, actAnchorAge, actKind>>
vars == <<now, row, inflight, delivered, replies, anchor, withdrawnSinceRequest,
          ghost, rejectedPending>>

Times == 1..MaxTime
Notes == {"none", "sent", "unsent"}
Withdrawn == revoked # 0 /\ revoked >= req

Init == /\ now = 1 /\ consent = FALSE /\ req = 0 /\ note = "none" /\ revoked = 0
        /\ inflight = {} /\ delivered = {} /\ replies = {}
        /\ anchor = 0 /\ withdrawnSinceRequest = FALSE
        /\ actReq = 0 /\ actReply = 0 /\ actWithdrawn = FALSE /\ actAge = 0
        /\ actAnchorAge = 0 /\ actKind = "none" /\ rejectedPending = FALSE

Tick == /\ now < MaxTime /\ now' = now + 1
        /\ UNCHANGED <<row, inflight, delivered, replies, anchor,
                       withdrawnSinceRequest, ghost, rejectedPending>>

(* Public request: one conditional update claims the confirmation message.   *)
(* A new request takes the current instant. An undelivered, unwithdrawn      *)
(* request may be sent again inside its short window and keeps its instant.  *)
NewRequest == req = 0 \/ now > req + ResendInterval
RetryUndelivered == /\ req # 0 /\ note = "unsent" /\ ~Withdrawn
                    /\ RetryKeepsInstant => now <= req + UnsentWindow
Claim ==
  /\ ~consent /\ (NewRequest \/ RetryUndelivered)
  /\ LET fresh == NewRequest \/ ~RetryKeepsInstant
         bound == IF fresh THEN now ELSE req
     IN /\ <<bound, now>> \notin inflight
        /\ req' = bound
        /\ inflight' = inflight \cup {<<bound, now>>}
        /\ anchor' = IF NewRequest THEN now ELSE anchor
        /\ withdrawnSinceRequest' = IF fresh THEN FALSE ELSE withdrawnSinceRequest
  /\ note' = "sent" /\ rejectedPending' = FALSE
  /\ UNCHANGED <<now, consent, revoked, delivered, replies, ghost>>

(* The provider outcome for an attempt arrives later; it is bound to the     *)
(* request instant it claimed. Delivery is recorded at the attempt's time.   *)
Accept(a) == /\ a \in inflight
             /\ inflight' = inflight \ {a} /\ delivered' = delivered \cup {a[2]}
             /\ UNCHANGED <<now, row, replies, anchor, withdrawnSinceRequest,
                            ghost, rejectedPending>>

Reject(a) ==
  /\ a \in inflight /\ inflight' = inflight \ {a}
  /\ LET matches == ~consent /\ req # 0 /\ note = "sent"
                    /\ (BindFailureToClaim => req = a[1])
         current == ~consent /\ req = a[1] /\ note = "sent" /\ ~Withdrawn
     IN /\ req' = IF matches /\ ~KeepUndeliveredPending THEN 0 ELSE req
        /\ note' = IF matches THEN "unsent" ELSE note
        /\ rejectedPending' = IF current THEN TRUE ELSE rejectedPending
  /\ UNCHANGED <<now, consent, revoked, delivered, replies, anchor,
                 withdrawnSinceRequest, ghost>>

(* The number's holder sends an affirmative keyword stamped by the provider. *)
Reply == /\ replies' = replies \cup {now}
         /\ UNCHANGED <<now, row, inflight, delivered, anchor,
                        withdrawnSinceRequest, ghost, rejectedPending>>

(* Webhook delivery of any earlier reply: late, duplicated or replayed.      *)
Confirm(r) ==
  /\ r \in replies /\ ~consent /\ req # 0 /\ note \in {"sent", "unsent"}
  /\ ReplyOrderGuard => req <= r
  /\ WithdrawalGuard => ~Withdrawn
  /\ now <= req + (IF note = "unsent" /\ ShortUnsentWindow THEN UnsentWindow ELSE Window)
  /\ consent' = TRUE /\ note' = "none" /\ revoked' = 0
  /\ actReq' = req /\ actReply' = r /\ actWithdrawn' = withdrawnSinceRequest
  /\ actAge' = now - req /\ actAnchorAge' = now - anchor /\ actKind' = note
  /\ rejectedPending' = FALSE
  /\ UNCHANGED <<now, req, inflight, delivered, replies, anchor, withdrawnSinceRequest>>

(* Public opt-out, inbound STOP or staff revocation. The handler read its    *)
(* clock c before it locked the row, so c may precede a committed request;   *)
(* the stored note is caller-supplied and may equal either pending marker.   *)
Withdraw ==
  \E c \in 1..now :
    /\ consent' = FALSE
    /\ req' = IF WithdrawalKeepsInterval THEN req ELSE 0
    /\ revoked' = IF SettleWithdrawalInstant /\ c < req THEN req ELSE c
    /\ note' \in Notes /\ rejectedPending' = FALSE
    /\ withdrawnSinceRequest' = TRUE
    /\ UNCHANGED <<now, inflight, delivered, replies, anchor, ghost>>

Next == \/ Tick \/ Claim \/ Reply \/ Withdraw
        \/ \E a \in inflight : Accept(a) \/ Reject(a)
        \/ \E r \in replies : Confirm(r)

TypeOK == /\ now \in Times /\ consent \in BOOLEAN /\ req \in 0..MaxTime
          /\ note \in Notes /\ revoked \in 0..MaxTime
          /\ inflight \subseteq (Times \X Times)
          /\ delivered \subseteq Times /\ replies \subseteq Times

(* Consent exists only through the number's own reply, made no earlier than  *)
(* the request it confirms.                                                  *)
ConfirmedByOwnLaterReply ==
  consent => actReq # 0 /\ actReply \in replies /\ actReply >= actReq

(* A withdrawal committed after a request defeats that request.              *)
WithdrawalWins == consent => ~actWithdrawn

(* Delivered confirmation requests are at least the interval less the short  *)
(* retry window apart.                                                       *)
DeliveredRequestsSpaced ==
  \A a \in delivered : \A b \in delivered :
    a < b => b + UnsentWindow > a + ResendInterval

(* A request confirms only inside its window, and one the number never saw   *)
(* only inside the short window measured from the first undelivered attempt. *)
ConfirmedInsideWindow ==
  consent => /\ actAge <= (IF actKind = "unsent" THEN UnsentWindow ELSE Window)
             /\ actKind = "unsent" => actAnchorAge <= UnsentWindow

(* An undelivered current request stays pending for the number to confirm.   *)
UndeliveredStaysPending == rejectedPending => req # 0 /\ note = "unsent"

Spec == Init /\ [][Next]_vars
====
