---- MODULE TaskRaci ----
EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANTS Owner, Worker, Replacement, TaskA, TaskB, OverrideReason

Actors == {Owner, Worker, Replacement}
Tasks == {TaskA, TaskB}

VARIABLES dependencies, taskState, responsible, accountable,
          collaborators, overridden, history

vars == <<dependencies, taskState, responsible, accountable,
          collaborators, overridden, history>>

HistoryRecords == [
  operation: {"dependency", "complete", "override", "reassign", "remove"},
  task: Tasks,
  actor: Actors]

PathExists(from, to, relation) ==
  \E length \in 2..Cardinality(Tasks):
    \E path \in [1..length -> Tasks]:
      /\ path[1] = from
      /\ path[length] = to
      /\ \A i \in 1..(length - 1):
           <<path[i], path[i + 1]>> \in relation

DependenciesDone(task) ==
  \A prerequisite \in Tasks:
    <<task, prerequisite>> \in dependencies =>
      taskState[prerequisite] = "completed"

Init ==
  /\ dependencies = {}
  /\ taskState = [task \in Tasks |-> "open"]
  /\ responsible = [task \in Tasks |-> {Worker}]
  /\ accountable = [task \in Tasks |-> Owner]
  /\ collaborators = Actors
  /\ overridden = {}
  /\ history = {}

AddDependency(task, prerequisite) ==
  /\ task \in Tasks
  /\ prerequisite \in Tasks
  /\ task # prerequisite
  /\ taskState[task] = "open"
  /\ <<task, prerequisite>> \notin dependencies
  /\ ~PathExists(prerequisite, task, dependencies)
  /\ dependencies' = dependencies \cup {<<task, prerequisite>>}
  /\ history' = history \cup
       {[operation |-> "dependency", task |-> task, actor |-> Owner]}
  /\ UNCHANGED <<taskState, responsible, accountable, collaborators, overridden>>

Complete(task, actor) ==
  /\ task \in Tasks
  /\ actor \in responsible[task] \cup {Owner}
  /\ taskState[task] = "open"
  /\ DependenciesDone(task)
  /\ taskState' = [taskState EXCEPT ![task] = "completed"]
  /\ history' = history \cup
       {[operation |-> "complete", task |-> task, actor |-> actor]}
  /\ UNCHANGED <<dependencies, responsible, accountable, collaborators, overridden>>

CompleteWithOverride(task) ==
  /\ task \in Tasks
  /\ taskState[task] = "open"
  /\ ~DependenciesDone(task)
  /\ OverrideReason # "none"
  /\ taskState' = [taskState EXCEPT ![task] = "completed"]
  /\ overridden' = overridden \cup {task}
  /\ history' = history \cup
       {[operation |-> "override", task |-> task, actor |-> Owner]}
  /\ UNCHANGED <<dependencies, responsible, accountable, collaborators>>

ReassignResponsible(task, from, to) ==
  /\ task \in Tasks
  /\ from \in responsible[task]
  /\ to \in collaborators
  /\ from # to
  /\ responsible' = [responsible EXCEPT
       ![task] = (responsible[task] \ {from}) \cup {to}]
  /\ history' = history \cup
       {[operation |-> "reassign", task |-> task, actor |-> Owner]}
  /\ UNCHANGED <<dependencies, taskState, accountable, collaborators, overridden>>

RemoveCollaborator(actor) ==
  /\ actor \in collaborators \ {Owner}
  /\ \A task \in Tasks:
       actor \notin responsible[task] /\ accountable[task] # actor
  /\ collaborators' = collaborators \ {actor}
  /\ history' = history \cup
       {[operation |-> "remove", task |-> TaskA, actor |-> Owner]}
  /\ UNCHANGED <<dependencies, taskState, responsible, accountable, overridden>>

Next ==
  \/ \E task, prerequisite \in Tasks: AddDependency(task, prerequisite)
  \/ \E task \in Tasks, actor \in Actors: Complete(task, actor)
  \/ \E task \in Tasks: CompleteWithOverride(task)
  \/ \E task \in Tasks, from, to \in Actors:
       ReassignResponsible(task, from, to)
  \/ \E actor \in Actors: RemoveCollaborator(actor)

TypeOK ==
  /\ dependencies \subseteq (Tasks \X Tasks)
  /\ taskState \in [Tasks -> {"open", "completed"}]
  /\ responsible \in [Tasks -> SUBSET Actors]
  /\ accountable \in [Tasks -> Actors]
  /\ collaborators \subseteq Actors
  /\ overridden \subseteq Tasks
  /\ history \subseteq HistoryRecords

NoCircularDependencies ==
  \A task \in Tasks: ~PathExists(task, task, dependencies)

ExactlyOneAccountable ==
  \A task \in Tasks: accountable[task] \in Actors

ResponsibleNeverEmpty ==
  \A task \in Tasks: responsible[task] # {}

NoOrphanAssignments ==
  \A task \in Tasks:
    responsible[task] \subseteq collaborators
    /\ accountable[task] \in collaborators

CompletedDependenciesSatisfiedOrOverridden ==
  \A task \in Tasks:
    taskState[task] = "completed" =>
      DependenciesDone(task) \/ task \in overridden

OverridesAreAudited ==
  \A task \in overridden:
    \E entry \in history:
      entry.operation = "override" /\ entry.task = task

Spec == Init /\ [][Next]_vars

====
