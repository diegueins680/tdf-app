---- MODULE LessonSlotAdvice ----
(***************************************************************************)
(* SCHOOL-SLOT-SUGGESTION-001. A suggested lesson slot is advice, not a     *)
(* reservation. Clients read a suggestion from a snapshot, and some time    *)
(* later try to book it; other clients book in between.                     *)
(*                                                                         *)
(* A slot here is one (teacher | student | resource, interval) cell of      *)
(* school_schedule_occupancy. The handler first checks that the slot is     *)
(* free and then inserts; these are two steps, as in createClassH.          *)
(*                                                                         *)
(* Exclusion = TRUE  : the insert is refused when the slot is occupied      *)
(*                     (exclusion constraint, SQLSTATE 23P01 -> 409).       *)
(* Exclusion = FALSE : before the migration; the insert always succeeds.    *)
(*                                                                         *)
(* Out of the model: interval geometry (proved separately with Z3), the     *)
(* three subjects of one lesson being inserted together (one statement,     *)
(* one trigger, atomic in PostgreSQL), cancellation, and PostgreSQL itself. *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  Exclusion,
  \* @type: Set(Int);
  Clients,
  \* @type: Set(Int);
  Slots

VARIABLES
  \* @type: Int -> Int;
  bookings,   \* rows occupying the slot
  \* @type: Int -> Int;
  holder,     \* client whose booking the slot records (0: none)
  \* @type: Int -> Str;
  pc,         \* idle | advised | checked | booked | rejected
  \* @type: Int -> Int;
  advice      \* slot suggested to the client (0: none)

vars == <<bookings, holder, pc, advice>>

\* @type: () => Bool;
CInitExclusion ==
  /\ Exclusion = TRUE
  /\ Clients \in SUBSET (1..4)
  /\ Slots \in SUBSET (1..3)

\* @type: () => Bool;
CInitLegacy ==
  /\ Exclusion = FALSE
  /\ Clients = {1, 2}
  /\ Slots = {1}

Init ==
  /\ bookings = [s \in Slots |-> 0]
  /\ holder = [s \in Slots |-> 0]
  /\ pc = [c \in Clients |-> "idle"]
  /\ advice = [c \in Clients |-> 0]

\* GET .../suggestions: sound on the snapshot it reads, and writes nothing.
Suggest(c) ==
  /\ pc[c] = "idle"
  /\ \E s \in Slots :
       /\ bookings[s] = 0
       /\ advice' = [advice EXCEPT ![c] = s]
  /\ pc' = [pc EXCEPT ![c] = "advised"]
  /\ UNCHANGED <<bookings, holder>>

\* POST .../class-sessions, first step: the unlocked availability check.
Check(c) ==
  /\ pc[c] = "advised"
  /\ pc' = [pc EXCEPT ![c] = IF bookings[advice[c]] = 0 THEN "checked" ELSE "rejected"]
  /\ UNCHANGED <<bookings, holder, advice>>

\* Second step: the insert, decided by the exclusion constraint when present.
Insert(c) ==
  /\ pc[c] = "checked"
  /\ IF Exclusion /\ bookings[advice[c]] # 0
       THEN /\ pc' = [pc EXCEPT ![c] = "rejected"]
            /\ UNCHANGED <<bookings, holder>>
       ELSE /\ pc' = [pc EXCEPT ![c] = "booked"]
            /\ bookings' = [bookings EXCEPT ![advice[c]] = @ + 1]
            /\ holder' = [holder EXCEPT ![advice[c]] = c]
  /\ UNCHANGED advice

Next == \E c \in Clients : Suggest(c) \/ Check(c) \/ Insert(c)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ bookings \in [Slots -> Nat]
  /\ holder \in [Slots -> Clients \cup {0}]
  /\ pc \in [Clients -> {"idle", "advised", "checked", "booked", "rejected"}]
  /\ advice \in [Clients -> Slots \cup {0}]

(* No slot is ever booked twice.                                            *)
NoDoubleBooking == \A s \in Slots : bookings[s] <= 1

(* A client told "booked" really holds the slot it was advised.             *)
BookedHolds ==
  \A c \in Clients : pc[c] = "booked" => (advice[c] \in Slots /\ holder[advice[c]] = c)

Safe == NoDoubleBooking /\ BookedHolds

(* Inductive invariant for Exclusion = TRUE.                                *)
IndInv ==
  /\ bookings \in [Slots -> {0, 1}]
  /\ holder \in [Slots -> Clients \cup {0}]
  /\ pc \in [Clients -> {"idle", "advised", "checked", "booked", "rejected"}]
  /\ advice \in [Clients -> Slots \cup {0}]
  /\ \A s \in Slots : (bookings[s] = 1) <=> (holder[s] # 0)
  /\ \A s \in Slots : holder[s] # 0 => (pc[holder[s]] = "booked" /\ advice[holder[s]] = s)
  /\ \A c \in Clients : pc[c] \in {"advised", "checked", "booked", "rejected"} => advice[c] \in Slots
  /\ \A c \in Clients : pc[c] = "booked" => holder[advice[c]] = c

(* Reachability witness, expected to be violated: advice does go stale. A   *)
(* client can hold a suggestion for a slot somebody else has since booked.  *)
AdviceNeverStale ==
  \A c \in Clients :
    pc[c] \in {"advised", "checked"} => bookings[advice[c]] = 0
====
