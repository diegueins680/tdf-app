----------------------------- MODULE EventAffinity -----------------------------
(***************************************************************************)
(* EVT-AFFINITY-ORDER-001 (formal/system/event-affinity.md).               *)
(*                                                                         *)
(* A request for the events page with scope "for_you" permutes the page    *)
(* the date-ordered query chose. An event is promoted when an artist of    *)
(* its lineup is evidence: a usable artist the caller has a signal for, or *)
(* a usable artist the stored similarity relates to such a seed.           *)
(*                                                                         *)
(* The stored similarity is old: it was computed when the job last ran.    *)
(* Artists are hidden and shown, and the caller's signals change, at any   *)
(* time. The request reads the listing live (LiveListing = TRUE).          *)
(*                                                                         *)
(* LiveListing = FALSE is the negative control: taking the listing the     *)
(* stored relation was computed from uses a hidden artist as evidence.     *)
(*                                                                         *)
(* Abstractions: weights and the similarity value are not modelled (an     *)
(* artist is evidence or not); the follow policy and the caller's own      *)
(* party are folded into "listed"; the order inside the promoted and the   *)
(* remaining events is not modelled (Z3 and QuickCheck cover the key).     *)
(***************************************************************************)
EXTENDS Integers

CONSTANTS
  \* @type: Set(Int);
  Artists,
  \* @type: Set(Int);
  Events,
  \* @type: Bool;
  LiveListing

VARIABLES
  \* @type: Set(Int);
  listed,
  \* @type: Set(Int);
  seeds,
  \* @type: Int -> Set(Int);
  lineup,
  \* @type: Set(Int);
  tableListed,
  \* @type: Set(<<Int, Int>>);
  table,
  \* @type: Set(Int);
  page,
  \* @type: Set(Int);
  readListed,
  \* @type: Set(Int);
  evidence,
  \* @type: Set(Int);
  promoted,
  \* @type: Set(Int);
  returned

vars == <<listed, seeds, lineup, tableListed, table, page, readListed, evidence, promoted, returned>>

Init ==
  /\ listed \in SUBSET Artists
  /\ seeds \in SUBSET Artists
  /\ lineup \in [Events -> SUBSET Artists]
  /\ tableListed = {}
  /\ table = {}
  /\ page = {}
  /\ readListed = {}
  /\ evidence = {}
  /\ promoted = {}
  /\ returned = {}

\* An artist is hidden or shown; the caller's signals change.
ChangeListing ==
  /\ listed' \in SUBSET Artists
  /\ UNCHANGED <<seeds, lineup, tableListed, table, page, readListed, evidence, promoted, returned>>

ChangeSeeds ==
  /\ seeds' \in SUBSET Artists
  /\ UNCHANGED <<listed, lineup, tableListed, table, page, readListed, evidence, promoted, returned>>

\* The job stores a relation between artists listed at that moment.
Generate ==
  /\ tableListed' = listed
  /\ table' \in SUBSET (listed \X listed)
  /\ UNCHANGED <<listed, seeds, lineup, page, readListed, evidence, promoted, returned>>

\* @type: (Set(Int), Int) => Bool;
Related(usableSeeds, c) ==
  \E s \in usableSeeds: <<s, c>> \in table \/ <<c, s>> \in table

\* One request: the date-ordered query chose `p`; the listing is read once.
Request ==
  \E p \in SUBSET Events:
    LET usable == IF LiveListing THEN listed ELSE tableListed
        usableSeeds == seeds \cap usable
        ev == usableSeeds \union {c \in usable: Related(usableSeeds, c)}
    IN /\ page' = p
       /\ readListed' = listed
       /\ evidence' = ev
       /\ promoted' = {e \in p: lineup[e] \cap ev # {}}
       /\ returned' = p
       /\ UNCHANGED <<listed, seeds, lineup, tableListed, table>>

Next == ChangeListing \/ ChangeSeeds \/ Generate \/ Request

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ listed \in SUBSET Artists
  /\ seeds \in SUBSET Artists
  /\ lineup \in [Events -> SUBSET Artists]
  /\ tableListed \in SUBSET Artists
  /\ table \in SUBSET (Artists \X Artists)
  /\ page \in SUBSET Events
  /\ readListed \in SUBSET Artists
  /\ evidence \in SUBSET Artists
  /\ promoted \in SUBSET Events
  /\ returned \in SUBSET Events

\* No artist that was hidden when the request read the listing is evidence.
NoHiddenEvidence == evidence \subseteq readListed

\* The response holds exactly the events of the page: none added, none lost.
PagePreserved == returned = page /\ promoted \subseteq page

\* A promoted event has a listed lineup artist that is evidence.
PromotedHasEvidence == \A e \in promoted: lineup[e] \cap evidence \cap readListed # {}

Safe == NoHiddenEvidence /\ PagePreserved /\ PromotedHasEvidence

IndInv == TypeOK /\ Safe

CInitShared ==
  /\ Artists = {1, 2, 3}
  /\ Events = {1, 2}

CInitImplementation == CInitShared /\ LiveListing = TRUE

CInitStaleListing == CInitShared /\ LiveListing = FALSE
================================================================================
