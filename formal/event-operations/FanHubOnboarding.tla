---- MODULE FanHubOnboarding ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS RequireConsent, CheckContext, CheckTerminal, CheckFlight
VARIABLES gen, readGen, known, eligible, pending, captured, consentOK,
          hidden, terminalOK, contextOK
vars == <<gen, readGen, known, eligible, pending, captured, consentOK,
          hidden, terminalOK, contextOK>>
Init == /\ gen = 0 /\ readGen = 0 /\ known = FALSE /\ eligible = FALSE
        /\ pending = {} /\ captured = [slot \in 1..2 |-> 0] /\ consentOK = TRUE
        /\ hidden = FALSE /\ terminalOK = TRUE /\ contextOK = TRUE
Change == /\ gen < 2 /\ gen' = gen + 1 /\ known' = FALSE
          /\ eligible' = FALSE /\ hidden' = FALSE
          /\ UNCHANGED <<readGen, pending, captured, consentOK, terminalOK, contextOK>>
StartRead == /\ readGen' = gen
             /\ UNCHANGED <<gen, known, eligible, pending, captured, consentOK,
                             hidden, terminalOK, contextOK>>
Read(value) ==
  /\ value \in {"eligible", "hidden", "invalid"}
  /\ (~CheckContext \/ readGen = gen)
  /\ known' = (value # "invalid") /\ eligible' = (value = "eligible")
  /\ contextOK' = (readGen = gen)
  /\ UNCHANGED <<gen, readGen, pending, captured, consentOK, hidden, terminalOK>>
Exit(clicked, slot) ==
  /\ known /\ eligible /\ ~hidden /\ slot \notin pending
  /\ (~RequireConsent \/ clicked)
  /\ (~CheckFlight \/ ~\E s \in pending: captured[s] = gen)
  /\ pending' = pending \cup {slot} /\ captured' = [captured EXCEPT ![slot] = gen]
  /\ consentOK' = clicked
  /\ UNCHANGED <<gen, readGen, known, eligible, hidden, terminalOK, contextOK>>
Receipt(terminal, slot) ==
  /\ slot \in pending /\ pending' = pending \ {slot}
  /\ IF ~CheckContext \/ captured[slot] = gen
        THEN /\ hidden' = (~CheckTerminal \/ terminal)
             /\ terminalOK' = (~hidden' \/ terminal)
             /\ contextOK' = (captured[slot] = gen)
        ELSE UNCHANGED <<hidden, terminalOK, contextOK>>
  /\ UNCHANGED <<gen, readGen, known, eligible, captured, consentOK>>
Next == \/ Change \/ StartRead \/ \E value \in {"eligible", "hidden", "invalid"}: Read(value)
        \/ \E clicked \in BOOLEAN, slot \in 1..2: Exit(clicked, slot)
        \/ \E terminal \in BOOLEAN, slot \in 1..2: Receipt(terminal, slot)
TypeOK == /\ gen \in 0..2 /\ readGen \in 0..2 /\ captured \in [1..2 -> 0..2]
          /\ pending \subseteq 1..2 /\ known \in BOOLEAN /\ eligible \in BOOLEAN
          /\ hidden \in BOOLEAN /\ consentOK \in BOOLEAN
          /\ terminalOK \in BOOLEAN /\ contextOK \in BOOLEAN
ConsentOnly == consentOK
SingleFlight == \A g \in 0..2: Cardinality({s \in pending: captured[s] = g}) <= 1
TerminalOnly == terminalOK
CurrentContext == contextOK
Spec == Init /\ [][Next]_vars
\* Conditional progress: each dispatched slot eventually returns (success or
\* failure). Without this network/scheduler fairness assumption, no timeout or
\* completion is claimed by this model.
ReturnSlot(slot) == \E terminal \in BOOLEAN: Receipt(terminal, slot)
FairSpec == Spec /\ (\A slot \in 1..2: WF_vars(ReturnSlot(slot)))
RequestsResolve == \A slot \in 1..2: (slot \in pending) ~> (slot \notin pending)
====
