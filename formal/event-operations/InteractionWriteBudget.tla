---- MODULE InteractionWriteBudget ----
(* SOC-ABUSE-001: one account's interaction write attempts. Each attempt     *)
(* belongs to the calendar minute in which its transaction began, charges    *)
(* that minute's counter, and runs its write only if the charge was within   *)
(* the limit. The write may be answered or may raise a database error. The   *)
(* counter row is locked from the charge until the attempt ends.             *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Requests, Limit, LastMinute, Nobody,
          KeepChargeOnError, ExactThreshold, LockedIncrement, CountPerMinute
VARIABLES clock, count, holder, pc, minute, charge, charges, chargedAs, executed, ended
vars == <<clock, count, holder, pc, minute, charge, charges, chargedAs, executed, ended>>

Minutes == 0..LastMinute
Row(m) == IF CountPerMinute THEN m ELSE 0

Init == /\ clock = 0 /\ holder = Nobody
        /\ count = [m \in Minutes |-> 0]
        /\ pc = [r \in Requests |-> "idle"]
        /\ minute = [r \in Requests |-> 0]
        /\ charge = [r \in Requests |-> 0]
        /\ chargedAs = [r \in Requests |-> 0]
        /\ charges = [m \in Minutes |-> 0]
        /\ executed = [m \in Minutes |-> 0]
        /\ ended = [m \in Minutes |-> 0]

Tick == /\ clock < LastMinute /\ clock' = clock + 1
        /\ UNCHANGED <<count, holder, pc, minute, charge, charges, chargedAs, executed, ended>>

Begin(r) ==
  /\ pc[r] = "idle"
  /\ minute' = [minute EXCEPT ![r] = clock]
  /\ pc' = [pc EXCEPT ![r] = "begun"]
  /\ UNCHANGED <<clock, count, holder, charge, charges, chargedAs, executed, ended>>

(* The upsert: wait for the row, add one to what is stored, compare.         *)
Charge(r) ==
  /\ pc[r] = "begun"
  /\ LockedIncrement => holder = Nobody
  /\ holder' = IF LockedIncrement THEN r ELSE holder
  /\ LET n == count[Row(minute[r])] + 1
         top == IF ExactThreshold THEN Limit ELSE Limit + 1
     IN /\ charge' = [charge EXCEPT ![r] = n]
        /\ pc' = [pc EXCEPT ![r] = IF n <= top THEN "charged" ELSE "limited"]
  /\ charges' = [charges EXCEPT ![minute[r]] = @ + 1]
  /\ chargedAs' = [chargedAs EXCEPT ![r] = charges[minute[r]] + 1]
  /\ UNCHANGED <<clock, count, minute, executed, ended>>

Run(r) ==
  /\ pc[r] = "charged"
  /\ \E outcome \in {"answered", "raised"} : pc' = [pc EXCEPT ![r] = outcome]
  /\ executed' = [executed EXCEPT ![minute[r]] = @ + 1]
  /\ UNCHANGED <<clock, count, holder, minute, charge, charges, chargedAs, ended>>

(* The transaction ends. A raised write is undone; the charge is kept only   *)
(* when it was protected from that undo.                                     *)
End(r) ==
  /\ pc[r] \in {"answered", "raised", "limited"}
  /\ count' = IF KeepChargeOnError \/ pc[r] # "raised"
              THEN [count EXCEPT ![Row(minute[r])] = charge[r]] ELSE count
  /\ holder' = IF holder = r THEN Nobody ELSE holder
  /\ ended' = [ended EXCEPT ![minute[r]] = @ + 1]
  /\ pc' = [pc EXCEPT ![r] = IF pc[r] = "limited" THEN "refused" ELSE "done"]
  /\ UNCHANGED <<clock, minute, charge, charges, chargedAs, executed>>

Next == Tick \/ \E r \in Requests : Begin(r) \/ Charge(r) \/ Run(r) \/ End(r)
Spec == Init /\ [][Next]_vars

Total(f) == LET S[m \in Minutes] == IF m = 0 THEN f[0] ELSE S[m - 1] + f[m] IN S[LastMinute]
Quiet == \A r \in Requests : pc[r] \in {"idle", "begun", "refused", "done"}

TypeOK == /\ clock \in Minutes /\ holder \in Requests \cup {Nobody}
          /\ pc \in [Requests -> {"idle", "begun", "charged", "limited", "answered", "raised", "refused", "done"}]
          /\ minute \in [Requests -> Minutes]

(* Every attempt that ended is in the stored counters, whatever its write    *)
(* did.                                                                      *)
ChargeCounted == Quiet => Total(count) = Total(ended)
(* No minute runs more writes than the limit.                                *)
LimitHolds == \A m \in Minutes : executed[m] <= Limit
(* An attempt is refused only when its own minute was already spent.         *)
RefusedOnlyWhenSpent == \A r \in Requests : pc[r] \in {"limited", "refused"} => chargedAs[r] > Limit
====
