----------------------- MODULE RelationshipReads -----------------------
EXTENDS Naturals, FiniteSets
CONSTANTS a, b, c, UnsafePolicy, UnsafeCounts, UnsafeDeleted
VARIABLES enabled, activated, pairExists, blocked, closed, live,
          original, edges, route, done, observed
vars == <<enabled,activated,pairExists,blocked,closed,live,original,edges,route,done,observed>>
People == {a,b,c}
Mutable == {a,b}
Graphs == {{<<a,b>>,<<b,a>>,<<b,c>>}, {<<a,b>>,<<a,c>>,<<c,a>>},
           {<<b,a>>,<<b,c>>}, {}}
Required(t) == activated \/ (t=b /\ pairExists) \/ a \in closed \/ t \in closed
Allowed(t) == ~Required(t) \/ (a \in live /\ t \in live /\ a \notin closed
                /\ t \notin closed /\ (t # b \/ ~blocked))
LegacySuggestions == ~activated /\ ~pairExists /\ closed={}
Other(e) == IF e[1]=a THEN e[2] ELSE e[1]
Matches(e, graph) == CASE route="followers" -> e[2]=a
 [] route="following" -> e[1]=a
 [] route="friends" -> e[1]=a /\ <<e[2],a>> \in graph
 [] OTHER -> FALSE
Direct(graph) == {a} \cup {Other(e): e \in {r \in graph: a \in {r[1],r[2]}}}
LegacyCount(graph) == Cardinality({s \in Direct(graph) \ {a}: <<s,c>> \in graph /\ c \notin Direct(graph)})
Expected == {e \in edges: Matches(e,edges) /\ Allowed(Other(e))}
ReturnedGraph == IF UnsafeDeleted THEN original ELSE edges
Returned == {e \in ReturnedGraph: Matches(e,ReturnedGraph) /\ (UnsafePolicy \/ Allowed(Other(e)))}
Init == /\ enabled=FALSE /\ activated=FALSE /\ pairExists=FALSE /\ blocked=FALSE
 /\ closed={} /\ live=People /\ original \in Graphs /\ edges=original
 /\ route \in {"followers","following","friends","suggestions"} /\ done=FALSE
 /\ observed=[rows |-> {}, expected |-> {}, count |-> 0, expectedCount |-> 0, legacy |-> TRUE]
Activate == /\ ~enabled /\ enabled'=TRUE /\ activated'=TRUE
 /\ UNCHANGED <<pairExists,blocked,closed,live,original,edges,route,done,observed>>
Pause == /\ enabled /\ enabled'=FALSE
 /\ UNCHANGED <<activated,pairExists,blocked,closed,live,original,edges,route,done,observed>>
Block == /\ ~blocked /\ blocked'=TRUE /\ pairExists'=TRUE
 /\ UNCHANGED <<enabled,activated,closed,live,original,edges,route,done,observed>>
Unblock == /\ blocked /\ blocked'=FALSE
 /\ UNCHANGED <<enabled,activated,pairExists,closed,live,original,edges,route,done,observed>>
Close(t) == /\ t \in Mutable \ closed /\ closed'=closed \cup {t}
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,live,original,edges,route,done,observed>>
Revoke(t) == /\ t \in Mutable \cap live /\ live'=live \ {t}
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,original,edges,route,done,observed>>
Delete(e) == /\ e \in edges /\ edges'=edges \ {e}
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,live,original,route,done,observed>>
Read == /\ ~done /\ done'=TRUE
 /\ observed'=[rows |-> Returned, expected |-> Expected,
      count |-> IF route="suggestions" /\ (LegacySuggestions \/ UnsafeCounts) THEN LegacyCount(edges) ELSE 0,
      expectedCount |-> IF route="suggestions" /\ LegacySuggestions THEN LegacyCount(edges) ELSE 0,
      legacy |-> LegacySuggestions]
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,live,original,edges,route>>
Next == ~done /\ (Activate \/ Pause \/ Block \/ Unblock \/ Read
 \/ (\E t \in Mutable: Close(t) \/ Revoke(t)) \/ (\E e \in edges: Delete(e)))
Spec == Init /\ [][Next]_vars /\ WF_vars(Read)
RowsAuthorized == observed.rows \subseteq observed.expected
ExactProjection == observed.rows=observed.expected /\ observed.count=observed.expectedCount
NoPrivateCounts == ~observed.legacy => observed.count=0
Progress == ~done ~> done
=============================================================================
