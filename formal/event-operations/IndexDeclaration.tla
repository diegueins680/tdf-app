---------------------------- MODULE IndexDeclaration ----------------------------
(* A table of entries written under a key (a conversation, a contact, a course
   registration) at a time, each with its own identity. The rule: an entry with
   a new identity is always stored, whatever key and time it carries, and an
   identity is stored once. KeyUnique and KeyTimeUnique are the constraints the
   schema carried by mistake; IdentityUnique is the one it keeps. *)
EXTENDS Naturals, FiniteSets

CONSTANTS Keys, Times, Identities, KeyUnique, KeyTimeUnique, IdentityUnique

VARIABLES rows, refused

vars == <<rows, refused>>

Row == [slot : 1..Cardinality(Identities) + 1, key : Keys, time : Times, identity : Identities]

TypeOK == rows \subseteq Row /\ refused \in BOOLEAN

Init == rows = {} /\ refused = FALSE

Stored == {r.identity : r \in rows}

Permitted(k, t, i) ==
  /\ KeyUnique => \A r \in rows : r.key # k
  /\ KeyTimeUnique => \A r \in rows : ~(r.key = k /\ r.time = t)
  /\ IdentityUnique => i \notin Stored

Store(k, t, i) ==
  rows' = rows \cup {[slot |-> Cardinality(rows) + 1, key |-> k, time |-> t, identity |-> i]}

(* An entry nobody has stored yet. *)
Write(k, t, i) ==
  /\ i \notin Stored
  /\ IF Permitted(k, t, i)
       THEN Store(k, t, i) /\ UNCHANGED refused
       ELSE refused' = TRUE /\ UNCHANGED rows

(* The same entry presented again. *)
Repeat(k, t, i) ==
  /\ i \in Stored
  /\ Cardinality(rows) <= Cardinality(Identities)
  /\ IF Permitted(k, t, i) THEN Store(k, t, i) ELSE UNCHANGED rows
  /\ UNCHANGED refused

Next == \E k \in Keys, t \in Times, i \in Identities : Write(k, t, i) \/ Repeat(k, t, i)

Spec == Init /\ [][Next]_vars

NothingNewRefused == refused = FALSE

IdentityStoredOnce == \A a, b \in rows : a.identity = b.identity => a = b
=============================================================================
