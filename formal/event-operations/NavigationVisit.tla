---- MODULE NavigationVisit ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANT Atomic
Accounts == {"a", "b"}
Requests == {1, 2, 3}
Owner(r) == IF r = 3 THEN "b" ELSE "a"
VARIABLES phase, observed, counts, accepted, failed, pinned, settingsDone
vars == <<phase, observed, counts, accepted, failed, pinned, settingsDone>>
Init == /\ phase = [r \in Requests |-> "new"]
        /\ observed = [r \in Requests |-> FALSE]
        /\ counts = [a \in Accounts |-> 0]
        /\ accepted = {} /\ failed = {} /\ pinned = [a \in Accounts |-> FALSE]
        /\ settingsDone = FALSE
Read(r) == /\ ~Atomic /\ phase[r] = "new"
           /\ phase' = [phase EXCEPT ![r] = "read"]
           /\ observed' = [observed EXCEPT ![r] = counts[Owner(r)] > 0]
           /\ UNCHANGED <<counts, accepted, failed, pinned, settingsDone>>
Commit(r) ==
  /\ IF Atomic THEN phase[r] = "new" ELSE phase[r] = "read"
  /\ LET collision == ~Atomic /\ ~observed[r] /\ counts[Owner(r)] > 0 IN
       /\ counts' = IF collision THEN counts ELSE [counts EXCEPT ![Owner(r)] = @ + 1]
       /\ accepted' = IF collision THEN accepted ELSE accepted \cup {r}
       /\ failed' = IF collision THEN failed \cup {r} ELSE failed
  /\ phase' = [phase EXCEPT ![r] = "done"]
  /\ UNCHANGED <<observed, pinned, settingsDone>>
Settings == /\ ~settingsDone /\ settingsDone' = TRUE
            /\ pinned' = [pinned EXCEPT !["a"] = TRUE]
            /\ UNCHANGED <<phase, observed, counts, accepted, failed>>
Next == (\E r \in Requests: Read(r) \/ Commit(r)) \/ Settings
NoFailedVisits == failed = {}
CountsMatchAccepted == \A a \in Accounts: counts[a] = Cardinality({r \in accepted: Owner(r) = a})
SettingsPreserved == settingsDone => pinned["a"] /\ ~pinned["b"]
AllSettle == <> (\A r \in Requests: phase[r] = "done")
Spec == Init /\ [][Next]_vars /\ (\A r \in Requests: WF_vars(Read(r)) /\ WF_vars(Commit(r)))
====
