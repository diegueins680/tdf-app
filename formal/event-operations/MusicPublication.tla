---------------------------- MODULE MusicPublication ----------------------------
(* Scheduled publication of music release versions (MEDIA-MUSIC-RELEASE-001).

   One release with three versions: v1 is live, v2 and v3 are corrections of
   v1. Staff schedule approved versions; the scheduler (music_publish_due)
   publishes due ones and the takedown scheduler (music_withdraw_due) withdraws
   them. A version can turn invalid while it waits ("poison": a rule that no
   longer holds at publication time).

   The retirement chain published -> replacement_pending -> takedown_scheduled
   -> withdrawn runs inside one statement and is one step here.

   Guards, each removed by one negative configuration:
     Isolate        each candidate publishes or fails in its own subtransaction
     RetireAll      publication retires every other live version, not only the
                    one the candidate replaces
     Reopen         publication clears the release's withdrawn mark and is not
                    filtered by it
     PointerTrigger a version leaving the live states through any path clears
                    the release pointer *)
EXTENDS Naturals, FiniteSets

CONSTANTS Isolate, RetireAll, Reopen, PointerTrigger

Versions == {"v1", "v2", "v3"}
None == "none"
Replaces == [v \in Versions |-> IF v = "v1" THEN None ELSE "v1"]

States == {"approved", "scheduled", "published", "takedown_scheduled",
           "suspended", "withdrawn"}
Live == {"published", "takedown_scheduled", "suspended"}

VARIABLES state, pointer, withdrawn, due, poison, failed
vars == <<state, pointer, withdrawn, due, poison, failed>>

TypeOK ==
  /\ state \in [Versions -> States]
  /\ pointer \in Versions \cup {None}
  /\ withdrawn \in BOOLEAN
  /\ due \in [Versions -> BOOLEAN]
  /\ poison \in [Versions -> BOOLEAN]
  /\ failed \in [Versions -> BOOLEAN]

Init ==
  /\ state = [v \in Versions |-> IF v = "v1" THEN "published" ELSE "approved"]
  /\ pointer = "v1"
  /\ withdrawn = FALSE
  /\ due = [v \in Versions |-> FALSE]
  /\ poison = [v \in Versions |-> FALSE]
  /\ failed = [v \in Versions |-> FALSE]

(* Staff actions. None is assumed to happen. *)
Schedule(v) ==
  /\ state[v] = "approved"
  /\ state' = [state EXCEPT ![v] = "scheduled"]
  /\ due' = [due EXCEPT ![v] = FALSE]
  /\ UNCHANGED <<pointer, withdrawn, poison, failed>>

Elapse(v) ==
  /\ state[v] = "scheduled" /\ ~due[v]
  /\ due' = [due EXCEPT ![v] = TRUE]
  /\ UNCHANGED <<state, pointer, withdrawn, poison, failed>>

Poison(v) ==
  /\ state[v] = "scheduled" /\ ~poison[v]
  /\ poison' = [poison EXCEPT ![v] = TRUE]
  /\ UNCHANGED <<state, pointer, withdrawn, due, failed>>

Takedown(v) ==
  /\ state[v] = "published"
  /\ state' = [state EXCEPT ![v] = "takedown_scheduled"]
  /\ UNCHANGED <<pointer, withdrawn, due, poison, failed>>

Suspend(v) ==
  /\ state[v] \in {"published", "scheduled"}
  /\ state' = [state EXCEPT ![v] = "suspended"]
  /\ UNCHANGED <<pointer, withdrawn, due, poison, failed>>

(* Re-approval re-validates the version; a formerly live version leaves the
   live states here, so the release stops pointing at it. *)
Approve(v) ==
  /\ state[v] = "suspended"
  /\ state' = [state EXCEPT ![v] = "approved"]
  /\ poison' = [poison EXCEPT ![v] = FALSE]
  /\ failed' = [failed EXCEPT ![v] = FALSE]
  /\ IF PointerTrigger /\ pointer = v
       THEN pointer' = None /\ withdrawn' = TRUE
       ELSE UNCHANGED <<pointer, withdrawn>>
  /\ UNCHANGED due

(* Withdrawal through the API (suspended or takedown_scheduled -> withdrawn). *)
HandlerWithdraw(v) ==
  /\ state[v] \in {"suspended", "takedown_scheduled"}
  /\ state' = [state EXCEPT ![v] = "withdrawn"]
  /\ IF PointerTrigger /\ pointer = v
       THEN pointer' = None /\ withdrawn' = TRUE
       ELSE UNCHANGED <<pointer, withdrawn>>
  /\ UNCHANGED <<due, poison, failed>>

(* The scheduler. *)
Candidate(v) ==
  /\ state[v] = "scheduled" /\ due[v]
  /\ Reopen \/ ~withdrawn

(* The versions after v publishes: every other formerly published version
   that is still published or takedown_scheduled is retired; without RetireAll
   only the version v replaces. *)
Retired(v) ==
  [u \in Versions |-> IF u = v THEN "published"
                      ELSE IF RetireAll /\ state[u] \in {"published", "takedown_scheduled"}
                        THEN "withdrawn"
                      ELSE IF ~RetireAll /\ u = Replaces[v] /\ state[u] = "published"
                        THEN "withdrawn"
                      ELSE state[u]]

(* The partial unique index: publication fails when another version would
   still be published afterwards. *)
Blocked(v) == \E u \in Versions \ {v} : Retired(v)[u] = "published"

Fails(v) == poison[v] \/ Blocked(v)

(* Without Isolate one failing candidate aborts the whole batch, so no
   candidate can publish while any due candidate fails. *)
BatchStuck == ~Isolate /\ \E u \in Versions : Candidate(u) /\ Fails(u)

PublishSucceeds(v) ==
  /\ Candidate(v) /\ ~Fails(v) /\ ~BatchStuck
  /\ state' = Retired(v)
  /\ pointer' = v
  /\ withdrawn' = FALSE
  /\ UNCHANGED <<due, poison, failed>>

PublishFails(v) ==
  /\ Candidate(v) /\ Fails(v) /\ Isolate
  /\ state' = [state EXCEPT ![v] = "suspended"]
  /\ failed' = [failed EXCEPT ![v] = TRUE]
  /\ UNCHANGED <<pointer, withdrawn, due, poison>>

RunPublish(v) == PublishSucceeds(v) \/ PublishFails(v)

RunWithdraw(v) ==
  /\ state[v] = "takedown_scheduled"
  /\ state' = [state EXCEPT ![v] = "withdrawn"]
  /\ IF pointer = v
       THEN pointer' = None /\ withdrawn' = TRUE
       ELSE UNCHANGED <<pointer, withdrawn>>
  /\ UNCHANGED <<due, poison, failed>>

Next ==
  \E v \in Versions :
    \/ Schedule(v) \/ Elapse(v) \/ Poison(v) \/ Takedown(v) \/ Suspend(v)
    \/ Approve(v) \/ HandlerWithdraw(v) \/ RunPublish(v) \/ RunWithdraw(v)

(* The schedulers run; nothing else is assumed to. *)
Fairness == \A v \in Versions : WF_vars(RunPublish(v)) /\ WF_vars(RunWithdraw(v))

Spec == Init /\ [][Next]_vars /\ Fairness

(* At most one version of the release is published (the partial unique index). *)
OnePublished == Cardinality({v \in Versions : state[v] = "published"}) <= 1

(* The release points at a live version or at nothing. *)
PointerLive == pointer # None => state[pointer] \in Live

(* A published version is the one the release points at. *)
PublishedIsPointed == \A v \in Versions : state[v] = "published" => pointer = v

(* A release marked withdrawn has no live pointer. *)
WithdrawnConsistent == withdrawn => pointer = None

(* The scheduler suspends only versions that are invalid. *)
OnlyInvalidFails == \A v \in Versions : failed[v] => poison[v]

(* Every due scheduled version eventually leaves 'scheduled', whatever the
   release's withdrawn mark. *)
Progress == \A v \in Versions :
  [](state[v] = "scheduled" /\ due[v] => <>(state[v] # "scheduled"))

(* A due scheduled version that is valid leaves 'scheduled' (or turns invalid
   itself) even while a sibling candidate fails. *)
ValidProgress == \A v \in Versions :
  [](state[v] = "scheduled" /\ due[v] /\ ~poison[v] => <>(state[v] # "scheduled" \/ poison[v]))

=============================================================================
