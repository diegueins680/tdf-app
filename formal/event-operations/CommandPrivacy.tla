---- MODULE CommandPrivacy ----
EXTENDS Naturals, TLC
CONSTANTS OpaqueDenial, VisibilityFirst
VARIABLES phase, grant, receipt, version, checkedGrant, absent, existing, effect
vars == <<phase, grant, receipt, version, checkedGrant, absent, existing, effect>>
Grants == {"none", "read", "manage"}
Receipts == {"none", "ownAccepted", "ownRejected", "otherBinding"}
Versions == {"current", "stale"}
Envelope(status, body) == [status |-> status, body |-> body]
Missing == Envelope(404, "not_found")
Forbidden == Envelope(403, "forbidden")
ReceiptResponse(r) == CASE r = "ownAccepted" -> Envelope(200, "historical_payload")
                          [] r = "ownRejected" -> Forbidden
                          [] OTHER -> Envelope(409, "idempotency_conflict")
Response(exists, g, r, v) ==
  IF ~exists THEN Missing
  ELSE IF ~VisibilityFirst /\ r # "none" THEN ReceiptResponse(r)
  ELSE IF g = "none" THEN IF OpaqueDenial THEN Missing ELSE Forbidden
  ELSE IF r # "none" THEN ReceiptResponse(r)
  ELSE IF v = "stale" THEN Envelope(409, "version_conflict")
  ELSE IF g = "read" THEN Forbidden
  ELSE Envelope(200, "new_payload")
Init ==
  /\ phase = "idle" /\ grant = "manage" /\ checkedGrant = "none"
  /\ receipt = "none" /\ version = "current"
  /\ absent = Missing /\ existing = Missing /\ effect = FALSE
Begin(r, v) ==
  /\ phase = "idle" /\ r \in Receipts /\ v \in Versions
  /\ phase' = "waiting" /\ receipt' = r /\ version' = v
  /\ UNCHANGED <<grant, checkedGrant, absent, existing, effect>>
ChangeGrant(g) ==
  /\ phase = "waiting" /\ g \in Grants /\ g # grant
  /\ grant' = g
  /\ UNCHANGED <<phase, receipt, version, checkedGrant, absent, existing, effect>>
Observe ==
  /\ phase = "waiting" /\ phase' = "done" /\ checkedGrant' = grant
  /\ absent' = Response(FALSE, grant, receipt, version)
  /\ existing' = Response(TRUE, grant, receipt, version)
  /\ effect' = (grant = "manage" /\ receipt = "none" /\ version = "current")
  /\ UNCHANGED <<grant, receipt, version>>
Next == \/ \E r \in Receipts, v \in Versions: Begin(r, v)
        \/ \E g \in Grants: ChangeGrant(g)
        \/ Observe
TypeOK == /\ phase \in {"idle", "waiting", "done"}
          /\ grant \in Grants /\ checkedGrant \in Grants
          /\ receipt \in Receipts /\ version \in Versions /\ effect \in BOOLEAN
          /\ absent = Missing
          /\ existing \in {Missing, Forbidden, Envelope(200, "historical_payload"),
                            Envelope(200, "new_payload"), Envelope(409, "idempotency_conflict"),
                            Envelope(409, "version_conflict")}
OpaqueTarget == (phase = "done" /\ checkedGrant = "none") => existing = absent
NoUnauthorizedEffect == effect => checkedGrant = "manage"
ReadOnlyDenial == (phase = "done" /\ checkedGrant = "read" /\ receipt = "none"
                  /\ version = "current") => existing = Forbidden
Spec == Init /\ [][Next]_vars
====
