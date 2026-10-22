-------------------------- MODULE PublicRouteAdmission --------------------------
(* Public route admission gate (AUTH-PUBLIC-001): scripts/check-public-routes.py
   over formal/system/compiled-api-surface.json and public-routes.json.

   Not a state machine: one state, and the invariants quantify over every
   finite situation of N abstract routes. A route may be served without
   AuthProtect (in the compiled surface), registered (in the reviewed
   register) with a boundary, mutating, and its boundary may be mutating,
   or the known-debt boundary. The gate admits a register iff every rule
   holds; each rule is a guard, removed by one negative configuration:
     CheckUnregistered   a served unauthenticated route without an entry fails
     CheckStale          an entry whose route is no longer served fails
     CheckMutating       a mutating route on a non-mutating boundary fails
     DebtExcluded        a known-debt entry is never counted as conformant *)
EXTENDS Naturals, FiniteSets

CONSTANTS N, CheckUnregistered, CheckStale, CheckMutating, DebtExcluded

Routes == 1..N
Boundaries == {"read", "mutating", "debt"}

(* One situation: for each route, whether it is served unauthenticated,
   whether it is registered, with which boundary, and whether it mutates. *)
Situations ==
  [served: [Routes -> BOOLEAN], registered: [Routes -> BOOLEAN],
   boundary: [Routes -> Boundaries], mutating: [Routes -> BOOLEAN]]

(* The gate as coded, guard by guard. *)
Admits(s) ==
  /\ (CheckUnregistered => \A r \in Routes : s.served[r] => s.registered[r])
  /\ (CheckStale => \A r \in Routes : s.registered[r] => s.served[r])
  /\ (CheckMutating => \A r \in Routes :
        (s.registered[r] /\ s.mutating[r]) => s.boundary[r] \in {"mutating", "debt"})

(* What an admitted register may count as conformant: a served route whose
   reviewed entry is not debt. *)
Conformant(s, r) == s.served[r] /\ s.registered[r] /\ (DebtExcluded => s.boundary[r] # "debt")

VARIABLE checked
Init == checked = TRUE
Next == UNCHANGED checked
Spec == Init /\ [][Next]_checked

(* Every served unauthenticated route of an admitted register is registered. *)
ServedRoutesRegistered ==
  \A s \in Situations : Admits(s) => \A r \in Routes : s.served[r] => s.registered[r]

(* Every entry of an admitted register names a served route. *)
NoStaleEntries ==
  \A s \in Situations : Admits(s) => \A r \in Routes : s.registered[r] => s.served[r]

(* No mutating route of an admitted register sits on a read boundary. *)
MutationReviewed ==
  \A s \in Situations : Admits(s) => \A r \in Routes :
    (s.served[r] /\ s.mutating[r]) => s.boundary[r] # "read"

(* A known-debt entry is never conformant, admitted or not. *)
DebtNeverConformant ==
  \A s \in Situations : \A r \in Routes : s.boundary[r] = "debt" => ~Conformant(s, r)

=============================================================================
