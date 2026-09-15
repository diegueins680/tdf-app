---- MODULE ArtistFollowConsent ----
EXTENDS Naturals, TLC
CONSTANTS RequireClick, RequireKnown, CheckContext
VARIABLES generation, party, artist, known, phase, captured, dispatchValid,
          visible, currentAtReceipt
vars == <<generation, party, artist, known, phase, captured, dispatchValid,
          visible, currentAtReceipt>>
Context == <<generation, party, artist>>
Init == /\ generation = 0 /\ party = "A" /\ artist = 17 /\ known = FALSE
        /\ phase = "idle" /\ captured = Context /\ dispatchValid = TRUE
        /\ visible = FALSE /\ currentAtReceipt = TRUE
Change(p, a) ==
  /\ generation < 2 /\ p \in {"A", "B", "none"} /\ a \in {17, 99}
  /\ generation' = generation + 1 /\ party' = p /\ artist' = a /\ known' = FALSE
  /\ UNCHANGED <<phase, captured, dispatchValid, visible, currentAtReceipt>>
Lookup == /\ ~known /\ known' = TRUE
          /\ UNCHANGED <<generation, party, artist, phase, captured, dispatchValid,
                          visible, currentAtReceipt>>
Dispatch(clicked) ==
  /\ phase = "idle" /\ party # "none"
  /\ (~RequireClick \/ clicked) /\ (~RequireKnown \/ known)
  /\ phase' = "pending" /\ captured' = Context
  /\ dispatchValid' = (clicked /\ known)
  /\ UNCHANGED <<generation, party, artist, known, visible, currentAtReceipt>>
Finish(success) ==
  /\ phase = "pending" /\ phase' = "done"
  /\ visible' = (success /\ (~CheckContext \/ captured = Context))
  /\ currentAtReceipt' = (captured = Context)
  /\ UNCHANGED <<generation, party, artist, known, captured, dispatchValid>>
Next == \/ \E p \in {"A", "B", "none"}, a \in {17, 99}: Change(p, a)
        \/ Lookup \/ \E click \in BOOLEAN: Dispatch(click)
        \/ \E success \in BOOLEAN: Finish(success)
TypeOK == /\ generation \in 0..2 /\ party \in {"A", "B", "none"}
          /\ artist \in {17, 99} /\ known \in BOOLEAN
          /\ phase \in {"idle", "pending", "done"}
          /\ dispatchValid \in BOOLEAN /\ visible \in BOOLEAN /\ currentAtReceipt \in BOOLEAN
NoUnconfirmedMutation == dispatchValid
CurrentTargetReceipt == visible => currentAtReceipt
Spec == Init /\ [][Next]_vars
====
