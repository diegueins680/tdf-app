-------------------------- MODULE WhatsAppConsent --------------------------
(* WhatsApp marketing consent double opt-in (PRIV-WHATSAPP-001):
   claimWhatsAppConfirmationRequest, releaseWhatsAppConfirmationClaim,
   applyWhatsAppConsentConfirmation and the opt-out writers in
   tdf-hq/src/TDF/Server.hs and tdf-hq/src/TDF/CampaignAutomation.hs.

   One phone row. A public request claims the right to send one confirmation
   message (pending = the request time); the provider accepts the message or
   not, and a refused message releases the claim. The number may send an
   affirmative keyword at any time; the message carries its own Meta timestamp
   (sentAt) and is delivered later, possibly more than once. Consent becomes
   active only when a pending request exists, is inside the confirmation
   window and is no later than the reply's sentAt. Any opt-out (public form,
   campaign keyword, admin revoke) deactivates consent and clears the pending
   request. An admin may capture consent directly.

   Each opt-out starts a new epoch; a claim remembers its epoch. Guards, each
   removed by one negative configuration:
     ReplyAfterRequest  a reply confirms only a request made no later than its sentAt
     WindowEnforced     a request older than the window cannot be confirmed
     ClearOnOptOut      an opt-out clears the pending request
     ConditionalConfirm the confirmation decides and writes in one statement
     IntervalEnforced   a new claim waits for the resend interval *)
EXTENDS Naturals, FiniteSets

CONSTANTS MaxTime, Interval, Window, MaxEpoch,
          ReplyAfterRequest, WindowEnforced, ClearOnOptOut, ConditionalConfirm, IntervalEnforced

None == "none"
Times == 0..MaxTime
Epochs == 0..MaxEpoch

VARIABLES
  clock,       \* server time
  consent,     \* TRUE while consent is active
  pending,     \* confirmation_requested_at, or None
  claimEpoch,  \* epoch in which the last claim was made
  outstanding, \* a claim whose provider result is unknown, or None
  epoch,       \* number of opt-outs so far
  lastSent,    \* [epoch -> claim time of the last accepted confirmation message, or None]
  confirmedBy, \* evidence of the active consent, or None
  staged,      \* a confirmation decided but not yet written (ConditionalConfirm = FALSE)
  doubleSend   \* two accepted confirmation messages within the interval in one epoch

vars == <<clock, consent, pending, claimEpoch, outstanding, epoch, lastSent, confirmedBy, staged, doubleSend>>

Evidence == [kind : {"reply"}, req : Times, sentAt : Times, at : Times, epoch : Epochs]
            \cup [kind : {"admin"}, at : Times, epoch : Epochs]

TypeOK ==
  /\ clock \in Times /\ consent \in BOOLEAN
  /\ pending \in Times \cup {None} /\ claimEpoch \in Epochs
  /\ outstanding \in Times \cup {None}
  /\ epoch \in Epochs
  /\ lastSent \in [Epochs -> Times \cup {None}]
  /\ confirmedBy \in Evidence \cup {None}
  /\ staged \in Evidence \cup {None}
  /\ doubleSend \in BOOLEAN

Init ==
  /\ clock = 0 /\ consent = FALSE /\ pending = None /\ claimEpoch = 0 /\ outstanding = None
  /\ epoch = 0 /\ lastSent = [e \in Epochs |-> None]
  /\ confirmedBy = None /\ staged = None /\ doubleSend = FALSE

Tick == clock < MaxTime /\ clock' = clock + 1
  /\ UNCHANGED <<consent, pending, claimEpoch, outstanding, epoch, lastSent, confirmedBy, staged, doubleSend>>

(* claimWhatsAppConfirmationRequest: one conditional update. *)
ClaimAllowed ==
  /\ ~consent
  /\ \/ pending = None
     \/ ~IntervalEnforced
     \/ (pending # None /\ pending + Interval < clock)
Claim ==
  /\ outstanding = None /\ ClaimAllowed
  /\ pending' = clock /\ outstanding' = clock /\ claimEpoch' = epoch
  /\ UNCHANGED <<clock, consent, epoch, lastSent, confirmedBy, staged, doubleSend>>

(* The provider accepted the confirmation message of the outstanding claim. *)
SendAccepted ==
  /\ outstanding # None
  /\ doubleSend' = (doubleSend \/ (lastSent[claimEpoch] # None /\ outstanding - lastSent[claimEpoch] <= Interval))
  /\ lastSent' = [lastSent EXCEPT ![claimEpoch] = outstanding]
  /\ outstanding' = None
  /\ UNCHANGED <<clock, consent, pending, claimEpoch, epoch, confirmedBy, staged>>

(* releaseWhatsAppConfirmationClaim: undoes only its own unconfirmed claim. *)
SendRefused ==
  /\ outstanding # None
  /\ pending' = IF pending # None /\ pending = outstanding /\ ~consent THEN None ELSE pending
  /\ outstanding' = None
  /\ UNCHANGED <<clock, consent, claimEpoch, epoch, lastSent, confirmedBy, staged, doubleSend>>

(* applyWhatsAppConsentConfirmation for a reply the number sent at sentAt. *)
Confirmable(sentAt) ==
  /\ ~consent /\ pending # None
  /\ (~WindowEnforced \/ pending + Window >= clock)
  /\ (~ReplyAfterRequest \/ pending <= sentAt)
ReplyEvidence(sentAt) ==
  [kind |-> "reply", req |-> pending, sentAt |-> sentAt, at |-> clock, epoch |-> claimEpoch]

Reply(sentAt) ==
  /\ ConditionalConfirm /\ Confirmable(sentAt)
  /\ consent' = TRUE /\ pending' = None /\ confirmedBy' = ReplyEvidence(sentAt)
  /\ UNCHANGED <<clock, claimEpoch, outstanding, epoch, lastSent, staged, doubleSend>>

(* Without the single statement: decide first, write later. *)
DecideReply(sentAt) ==
  /\ ~ConditionalConfirm /\ staged = None /\ Confirmable(sentAt)
  /\ staged' = ReplyEvidence(sentAt)
  /\ UNCHANGED <<clock, consent, pending, claimEpoch, outstanding, epoch, lastSent, confirmedBy, doubleSend>>
WriteReply ==
  /\ staged # None
  /\ consent' = TRUE /\ pending' = None /\ confirmedBy' = staged /\ staged' = None
  /\ UNCHANGED <<clock, claimEpoch, outstanding, epoch, lastSent, doubleSend>>

(* Public opt-out, campaign keyword or admin revoke: one upsert. *)
OptOut ==
  /\ epoch < MaxEpoch
  /\ consent' = FALSE /\ epoch' = epoch + 1
  /\ pending' = IF ClearOnOptOut THEN None ELSE pending
  /\ UNCHANGED <<clock, claimEpoch, outstanding, lastSent, confirmedBy, staged, doubleSend>>

(* Gated admin capture. *)
AdminCapture ==
  /\ ~consent
  /\ consent' = TRUE /\ pending' = None
  /\ confirmedBy' = [kind |-> "admin", at |-> clock, epoch |-> epoch]
  /\ UNCHANGED <<clock, claimEpoch, outstanding, epoch, lastSent, staged, doubleSend>>

Finished == clock = MaxTime /\ UNCHANGED vars

Next ==
  \/ Tick \/ Claim \/ SendAccepted \/ SendRefused \/ OptOut \/ AdminCapture
  \/ \E s \in 0..clock : Reply(s) \/ DecideReply(s)
  \/ WriteReply
  \/ Finished

Spec == Init /\ [][Next]_vars

(* Consent by reply rests on a request that was pending, made no later than
   the reply was sent and confirmed inside the window. *)
ConsentFromTimelyReply ==
  consent /\ confirmedBy # None /\ confirmedBy.kind = "reply" =>
    /\ confirmedBy.req <= confirmedBy.sentAt
    /\ confirmedBy.req + Window >= confirmedBy.at

(* No opt-out since the confirmed request was made: a withdrawal wins over a
   concurrent or later confirmation of an earlier request. *)
WithdrawalWins == consent => (confirmedBy # None /\ confirmedBy.epoch = epoch)

(* At most one accepted confirmation message per interval between opt-outs. *)
OneMessagePerInterval == ~doubleSend

(* Consent never exists without evidence. *)
ConsentEvidenced == consent => confirmedBy # None

=============================================================================
