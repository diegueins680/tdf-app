---- MODULE ReservationRace ----
EXTENDS FiniteSets, Integers, Sequences, TLC

CONSTANTS E1, E2, R1, C1, C2, Owner, OverrideActor, OverrideCommand, OverrideReason

Engagements == {E1, E2}
Commands == {C1, C2}
NoReason == "none"

CommandEngagement == [c \in Commands |-> IF c = C1 THEN E1 ELSE E2]
ResourceOf == [e \in Engagements |-> R1]
StartsAt == [e \in Engagements |-> IF e = E1 THEN 10 ELSE 11]
EndsAt == [e \in Engagements |-> IF e = E1 THEN 12 ELSE 13]
IsExclusive == [r \in {R1} |-> TRUE]

Overlap(left, right) ==
  StartsAt[left] < EndsAt[right] /\ StartsAt[right] < EndsAt[left]

ConflictWithConfirmed(booking, engagement) ==
  \E other \in Engagements:
    /\ other # engagement
    /\ booking[other] = "confirmed"
    /\ ResourceOf[other] = ResourceOf[engagement]
    /\ IsExclusive[ResourceOf[engagement]]
    /\ Overlap(other, engagement)

OverridePermitted(command) ==
  /\ command = OverrideCommand
  /\ OverrideActor = Owner
  /\ OverrideReason # NoReason

(* --algorithm ReservationRace
variables
  booking = [e \in Engagements |-> "none"],
  seenCommands = {},
  overrideUsed = {},
  audit = <<>>;

process client \in Commands
variables engagement = CommandEngagement[self];
begin
Confirm:
  if self \notin seenCommands then
    seenCommands := seenCommands \cup {self};
    if ~ConflictWithConfirmed(booking, engagement) then
      booking[engagement] := "confirmed";
      audit := Append(audit,
        [command |-> self, engagement |-> engagement,
         result |-> "confirmed", override |-> FALSE]);
    elsif OverridePermitted(self) then
      booking[engagement] := "confirmed";
      overrideUsed := overrideUsed \cup {engagement};
      audit := Append(audit,
        [command |-> self, engagement |-> engagement,
         result |-> "confirmed", override |-> TRUE]);
    else
      audit := Append(audit,
        [command |-> self, engagement |-> engagement,
         result |-> "conflict", override |-> FALSE]);
    end if;
  end if;
end process;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "14601121" /\ chksum(tla) = "6057351e")
VARIABLES booking, seenCommands, overrideUsed, audit, pc, engagement

vars == << booking, seenCommands, overrideUsed, audit, pc, engagement >>

ProcSet == (Commands)

Init == (* Global variables *)
        /\ booking = [e \in Engagements |-> "none"]
        /\ seenCommands = {}
        /\ overrideUsed = {}
        /\ audit = <<>>
        (* Process client *)
        /\ engagement = [self \in Commands |-> CommandEngagement[self]]
        /\ pc = [self \in ProcSet |-> "Confirm"]

Confirm(self) == /\ pc[self] = "Confirm"
                 /\ IF self \notin seenCommands
                       THEN /\ seenCommands' = (seenCommands \cup {self})
                            /\ IF ~ConflictWithConfirmed(booking, engagement[self])
                                  THEN /\ booking' = [booking EXCEPT ![engagement[self]] = "confirmed"]
                                       /\ audit' =        Append(audit,
                                                   [command |-> self, engagement |-> engagement[self],
                                                    result |-> "confirmed", override |-> FALSE])
                                       /\ UNCHANGED overrideUsed
                                  ELSE /\ IF OverridePermitted(self)
                                             THEN /\ booking' = [booking EXCEPT ![engagement[self]] = "confirmed"]
                                                  /\ overrideUsed' = (overrideUsed \cup {engagement[self]})
                                                  /\ audit' =        Append(audit,
                                                              [command |-> self, engagement |-> engagement[self],
                                                               result |-> "confirmed", override |-> TRUE])
                                             ELSE /\ audit' =        Append(audit,
                                                              [command |-> self, engagement |-> engagement[self],
                                                               result |-> "conflict", override |-> FALSE])
                                                  /\ UNCHANGED << booking,
                                                                  overrideUsed >>
                       ELSE /\ TRUE
                            /\ UNCHANGED << booking, seenCommands,
                                            overrideUsed, audit >>
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED engagement

client(self) == Confirm(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in Commands: client(self))
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

TypeOK ==
  /\ booking \in [Engagements -> {"none", "confirmed"}]
  /\ seenCommands \subseteq Commands
  /\ overrideUsed \subseteq Engagements
  /\ audit \in Seq([
       command: Commands, engagement: Engagements,
       result: {"confirmed", "conflict"}, override: BOOLEAN])
  /\ engagement \in [Commands -> Engagements]
  /\ pc \in [Commands -> {"Confirm", "Done"}]

NoUnjustifiedExclusiveConflict ==
  \A left, right \in Engagements:
    left # right /\ booking[left] = "confirmed" /\ booking[right] = "confirmed"
    /\ ResourceOf[left] = ResourceOf[right]
    /\ IsExclusive[ResourceOf[left]]
    /\ Overlap(left, right)
      => (left \in overrideUsed \/ right \in overrideUsed)

EveryOverrideIsAudited ==
  \A e \in overrideUsed:
    \E i \in 1..Len(audit):
      /\ audit[i].engagement = e
      /\ audit[i].result = "confirmed"
      /\ audit[i].override

OneSideEffectPerCommand ==
  Cardinality({audit[i].command: i \in 1..Len(audit)}) = Len(audit)

====
