----------------------------- MODULE DriveReplay -----------------------------
(* Reuse of a Drive upload by idempotency key (MEDIA-DRIVE-001;
   uploadToDrive and findDriveUploadByIdempotencyKey in tdf-hq/src/TDF/Server.hs).

   One idempotency key. The provider holds at most one findable entry for
   it: none, a legacy entry without a stored fingerprint, or an entry whose
   fingerprint binds the creating actor and the request shape (name, MIME,
   folder and bytes, abstracted as one value). Requests look the entry up,
   then create (entry absent), reuse (entry bound to the same actor and
   shape) or conflict (anything else, with no provider mutation). The lookup
   and the create are separate provider calls, so two requests may both see
   no entry and both create: SingleObject is not an invariant of this design
   and the Race configuration shows the counterexample.

   Guards, each removed by one negative configuration:
     FingerprintChecked  reuse requires the stored fingerprint to match
     ActorBound          the fingerprint includes the authenticated actor
     LegacyRejected      an entry without a fingerprint is never reused *)
EXTENDS Naturals, FiniteSets

CONSTANTS FingerprintChecked, ActorBound, LegacyRejected

Requests == {1, 2}
Actors == {"a1", "a2"}
Shapes == {"s1", "s2"}

None == [kind |-> "none"]
Legacy == [kind |-> "legacy"]
Bound(a, s) == [kind |-> "bound", actor |-> a, shape |-> s]
Entries == {None, Legacy} \cup {Bound(a, s) : a \in Actors, s \in Shapes}

VARIABLES
  entry,    \* the findable provider entry for the key
  objects,  \* provider objects created for the key
  req       \* [Requests -> [actor, shape, phase, seen, result]]

vars == <<entry, objects, req>>

TypeOK ==
  /\ entry \in Entries
  /\ objects \in 0..2
  /\ \A r \in Requests :
       /\ req[r].actor \in Actors
       /\ req[r].shape \in Shapes
       /\ req[r].phase \in {"idle", "looked", "done"}
       /\ req[r].seen \in Entries
       /\ req[r].result \in {"none", "created", "reused", "conflict"}

Init ==
  /\ entry \in Entries
  /\ objects = 0
  /\ req \in [Requests -> [actor : Actors, shape : Shapes, phase : {"idle"},
                           seen : {None}, result : {"none"}]]

Lookup(r) ==
  /\ req[r].phase = "idle"
  /\ req' = [req EXCEPT ![r].phase = "looked", ![r].seen = entry]
  /\ UNCHANGED <<entry, objects>>

(* The multipart create: the entry is written with the request's fingerprint.
   A second create for the same key adds an object; a later lookup would then
   fail closed (two matches), which no request in this model reaches. *)
Create(r) ==
  /\ req[r].phase = "looked" /\ req[r].seen = None
  /\ objects' = objects + 1
  /\ entry' = IF entry = None THEN Bound(req[r].actor, req[r].shape) ELSE entry
  /\ req' = [req EXCEPT ![r].phase = "done", ![r].result = "created"]

Matches(e, r) ==
  /\ e.shape = req[r].shape
  /\ ActorBound => e.actor = req[r].actor

Reusable(e, r) ==
  /\ e # None
  /\ e.kind = "legacy" => ~LegacyRejected
  /\ (e.kind = "bound" /\ FingerprintChecked) => Matches(e, r)

Reuse(r) ==
  /\ req[r].phase = "looked" /\ Reusable(req[r].seen, r)
  /\ req' = [req EXCEPT ![r].phase = "done", ![r].result = "reused"]
  /\ UNCHANGED <<entry, objects>>

Conflict(r) ==
  /\ req[r].phase = "looked" /\ req[r].seen # None /\ ~Reusable(req[r].seen, r)
  /\ req' = [req EXCEPT ![r].phase = "done", ![r].result = "conflict"]
  /\ UNCHANGED <<entry, objects>>

(* Every request has answered; the system stutters from here. *)
Finished == (\A r \in Requests : req[r].phase = "done") /\ UNCHANGED vars

Next ==
  \/ \E r \in Requests : Lookup(r) \/ Create(r) \/ Reuse(r) \/ Conflict(r)
  \/ Finished

Spec == Init /\ [][Next]_vars

Reused == {r \in Requests : req[r].result = "reused"}

(* A reused entry carries a fingerprint equal to the request's own. *)
ReuseBinds == \A r \in Reused : entry.kind = "bound" /\ entry.shape = req[r].shape

(* A request never reuses another actor's upload. *)
NoForeignReuse == \A r \in Reused : entry.kind = "bound" /\ entry.actor = req[r].actor

(* A legacy entry without a fingerprint is never adopted. *)
NoLegacyAdoption == \A r \in Reused : entry.kind # "legacy"

(* Objects are created only by requests that report a creation: neither a
   conflict nor a reuse creates a provider object. *)
ConflictNoMutation == objects = Cardinality({r \in Requests : req[r].result = "created"})

(* Not an invariant of the lookup-then-create design: two requests that
   both see no entry both create. Checked only by the Race configuration,
   which is expected to fail. *)
SingleObject == objects <= 1

=============================================================================
