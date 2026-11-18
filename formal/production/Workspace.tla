----------------------------- MODULE Workspace -----------------------------
EXTENDS Integers, FiniteSets
CONSTANT Scenario, Unsafe
VARIABLES edges, version, history, completed, plan, proposal, date, audit, leak
vars == <<edges,version,history,completed,plan,proposal,date,audit,leak>>
Actors == {"producer","engineer","artist","outside"}
Versions == {3,4}
Choices == {"approved","changes"}
Nodes == {1,2,3}
Arcs == {e \in Nodes \X Nodes : e[1] # e[2]}
Cyclic(g) == \/ \E a,b \in Nodes : <<a,b>> \in g /\ <<b,a>> \in g
             \/ \E a,b,c \in Nodes : <<a,b>> \in g /\ <<b,c>> \in g /\ <<c,a>> \in g
Init == /\ edges={} /\ version=3 /\ history={} /\ completed=FALSE
        /\ plan=0 /\ proposal=-1 /\ date=0 /\ audit=TRUE /\ leak=FALSE
Edge(e) == /\ Unsafe \/ ~Cyclic(edges \cup {e})
           /\ edges'=edges \cup {e}
           /\ UNCHANGED <<version,history,completed,plan,proposal,date,audit,leak>>
Publish == /\ version=3 /\ version'=4 /\ completed'=FALSE
           /\ UNCHANGED <<edges,history,plan,proposal,date,audit,leak>>
Decide(actor,seen,choice) ==
    LET old == {h \in history : h[1]=seen}
        record == <<seen,actor,choice>>
    IN /\ actor="artist"
       /\ IF record \in old THEN UNCHANGED <<history,completed>>
             ELSE /\ (Unsafe \/ seen=version) /\ old={}
                  /\ history'=history \cup {record} /\ completed'=(choice="approved")
       /\ UNCHANGED <<edges,version,plan,proposal,date,audit,leak>>
Propose == /\ proposal'=plan
           /\ UNCHANGED <<edges,version,history,completed,plan,date,audit,leak>>
Edit == /\ plan<2 /\ plan'=plan+1
        /\ UNCHANGED <<edges,version,history,completed,proposal,date,audit,leak>>
Confirm(actor) == /\ actor="producer" /\ proposal>=0 /\ plan<2
                  /\ (Unsafe \/ proposal=plan) /\ audit'=(proposal=plan)
                  /\ date'=date+1 /\ plan'=plan+1 /\ proposal'=-1
                  /\ UNCHANGED <<edges,version,history,completed,leak>>
\* Client has been granted only P's v3, while staff belong to P.
Read(actor,p,v) ==
    LET allowed == p="P" /\ (actor \in {"producer","engineer"} \/ (actor="artist" /\ v=3))
    IN /\ (allowed \/ Unsafe) /\ leak'=~allowed
       /\ UNCHANGED <<edges,version,history,completed,plan,proposal,date,audit>>
Next == CASE Scenario="graph" -> \E e \in Arcs : Edge(e)
          [] Scenario="review" -> Publish \/ \E a \in Actors,v \in Versions,c \in Choices : Decide(a,v,c)
          [] Scenario="plan" -> Propose \/ Edit \/ \E a \in Actors : Confirm(a)
          [] OTHER -> \E a \in Actors,p \in {"P","Q"},v \in Versions : Read(a,p,v)
Spec == Init /\ [][Next]_vars
Acyclic == ~Cyclic(edges)
CurrentApproval == ~completed \/ <<version,"artist","approved">> \in history
UniqueDecision == \A v \in Versions : Cardinality({h \in history : h[1]=v})<=1
PlanSafe == audit /\ date<=plan
Private == ~leak
=============================================================================
