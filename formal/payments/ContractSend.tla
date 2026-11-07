---- MODULE ContractSend ----
(***************************************************************************)
(* CONTRACT-SEND-001. POST /contracts/{id}/send (TDF.Contracts.Server,     *)
(* sendH). Without email configuration the handler answers 503 and records *)
(* nothing. Otherwise it inserts a contract_delivery row, asks the mail     *)
(* transport to send the contract PDF, and then marks the row accepted     *)
(* (answer "sent") or failed (answer 502). A request may stop at any step  *)
(* (process crash, lost connection).                                       *)
(*                                                                         *)
(* RecordFirst = TRUE  : the delivery row is inserted before SMTP (code).  *)
(* RecordFirst = FALSE : the row is written only after SMTP answers        *)
(*                       (negative control).                               *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  RecordFirst,
  \* @type: Bool;
  EmailConfigured,
  \* @type: Set(Int);
  Requests

VARIABLES
  \* @type: Int -> Str;
  pc,         \* idle | recorded | smtp_ok | smtp_failed | done
  \* @type: Int -> Str;
  delivery,   \* none | requested | accepted | failed
  \* @type: Int -> Bool;
  mailed,     \* the transport accepted a message carrying the PDF
  \* @type: Int -> Str;
  response    \* none | sent | bad_gateway | unavailable

vars == <<pc, delivery, mailed, response>>

Init ==
  /\ pc = [r \in Requests |-> "idle"]
  /\ delivery = [r \in Requests |-> "none"]
  /\ mailed = [r \in Requests |-> FALSE]
  /\ response = [r \in Requests |-> "none"]

NotConfigured(r) ==
  /\ pc[r] = "idle"
  /\ ~EmailConfigured
  /\ pc' = [pc EXCEPT ![r] = "done"]
  /\ response' = [response EXCEPT ![r] = "unavailable"]
  /\ UNCHANGED <<delivery, mailed>>

\* Insert the delivery row; with RecordFirst = FALSE this step records nothing.
Record(r) ==
  /\ pc[r] = "idle"
  /\ EmailConfigured
  /\ pc' = [pc EXCEPT ![r] = "recorded"]
  /\ delivery' = IF RecordFirst THEN [delivery EXCEPT ![r] = "requested"] ELSE delivery
  /\ UNCHANGED <<mailed, response>>

SmtpAccepts(r) ==
  /\ pc[r] = "recorded"
  /\ pc' = [pc EXCEPT ![r] = "smtp_ok"]
  /\ mailed' = [mailed EXCEPT ![r] = TRUE]
  /\ UNCHANGED <<delivery, response>>

SmtpRefuses(r) ==
  /\ pc[r] = "recorded"
  /\ pc' = [pc EXCEPT ![r] = "smtp_failed"]
  /\ UNCHANGED <<delivery, mailed, response>>

MarkAccepted(r) ==
  /\ pc[r] = "smtp_ok"
  /\ pc' = [pc EXCEPT ![r] = "done"]
  /\ delivery' = [delivery EXCEPT ![r] = "accepted"]
  /\ response' = [response EXCEPT ![r] = "sent"]
  /\ UNCHANGED mailed

MarkFailed(r) ==
  /\ pc[r] = "smtp_failed"
  /\ pc' = [pc EXCEPT ![r] = "done"]
  /\ delivery' = [delivery EXCEPT ![r] = "failed"]
  /\ response' = [response EXCEPT ![r] = "bad_gateway"]
  /\ UNCHANGED mailed

Next ==
  \E r \in Requests :
    \/ NotConfigured(r) \/ Record(r) \/ SmtpAccepts(r) \/ SmtpRefuses(r)
    \/ MarkAccepted(r) \/ MarkFailed(r)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ pc \in [Requests -> {"idle", "recorded", "smtp_ok", "smtp_failed", "done"}]
  /\ delivery \in [Requests -> {"none", "requested", "accepted", "failed"}]
  /\ mailed \in [Requests -> BOOLEAN]
  /\ response \in [Requests -> {"none", "sent", "bad_gateway", "unavailable"}]

(* "sent" is answered only after the transport accepted the PDF and the    *)
(* delivery is recorded as accepted.                                       *)
SentMeansAccepted ==
  \A r \in Requests : response[r] = "sent" => (mailed[r] /\ delivery[r] = "accepted")

(* Every message the transport accepted has a delivery row, even if the    *)
(* request stopped before marking it.                                      *)
NoUnrecordedMail == \A r \in Requests : mailed[r] => delivery[r] /= "none"

(* A row marked accepted corresponds to an accepted message.               *)
AcceptedMeansMailed == \A r \in Requests : delivery[r] = "accepted" => mailed[r]

(* Without email configuration nothing is recorded or sent.                *)
UnavailableIsInert ==
  \A r \in Requests : response[r] = "unavailable" => (delivery[r] = "none" /\ ~mailed[r])

Safe == SentMeansAccepted /\ NoUnrecordedMail /\ AcceptedMeansMailed /\ UnavailableIsInert

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ RecordFirst
  /\ \A r \in Requests :
       /\ pc[r] = "idle" => (delivery[r] = "none" /\ ~mailed[r] /\ response[r] = "none")
       /\ pc[r] = "recorded" => (delivery[r] = "requested" /\ ~mailed[r] /\ response[r] = "none")
       /\ pc[r] = "smtp_ok" => (delivery[r] = "requested" /\ mailed[r] /\ response[r] = "none")
       /\ pc[r] = "smtp_failed" => (delivery[r] = "requested" /\ ~mailed[r] /\ response[r] = "none")
       /\ pc[r] = "done" => response[r] /= "none"
       /\ response[r] = "bad_gateway" => (delivery[r] = "failed" /\ ~mailed[r])
       /\ delivery[r] /= "none" => EmailConfigured

CInitRecordFirst ==
  /\ RecordFirst = TRUE
  /\ EmailConfigured \in BOOLEAN
  /\ Requests \in SUBSET (1..4) /\ Requests /= {}

CInitRecordAfter ==
  /\ RecordFirst = FALSE
  /\ EmailConfigured = TRUE
  /\ Requests = {1}

====
