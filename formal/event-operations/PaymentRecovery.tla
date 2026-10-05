---- MODULE PaymentRecovery ----
EXTENDS FiniteSets
CONSTANTS Checkouts, UnsafeOverwrite, UnsafeReturn, UnsafeCompletedFlow
VARIABLES started, stored, completed, wrongReturn, completedFlow
vars == <<started, stored, completed, wrongReturn, completedFlow>>
Init == /\ started = {} /\ stored = {} /\ completed = {}
        /\ wrongReturn = FALSE /\ completedFlow = FALSE
Start(c) == /\ c \notin started
            /\ started' = started \cup {c}
            /\ stored' = IF UnsafeOverwrite THEN {c} ELSE stored \cup {c}
            /\ UNCHANGED <<completed, wrongReturn, completedFlow>>
Complete(c) == /\ c \in stored /\ c \notin completed
               /\ completed' = completed \cup {c}
               /\ UNCHANGED <<started, stored, wrongReturn, completedFlow>>
Return(c) == /\ c \in started /\ stored # {}
             /\ \E selected \in stored:
                  /\ (UnsafeReturn \/ selected = c)
                  /\ wrongReturn' = (selected # c)
             /\ UNCHANGED <<started, stored, completed, completedFlow>>
Discover(c) == /\ c \in stored
               /\ (UnsafeCompletedFlow \/ c \notin completed)
               /\ completedFlow' = (c \in completed)
               /\ UNCHANGED <<started, stored, completed, wrongReturn>>
Next == \E c \in Checkouts : Start(c) \/ Complete(c) \/ Return(c) \/ Discover(c)
NoLostRecovery == started \subseteq stored
CorrectReturn == ~wrongReturn
NoCompletedFlowHijack == ~completedFlow
TypeOK == /\ started \subseteq Checkouts /\ stored \subseteq Checkouts
          /\ completed \subseteq Checkouts /\ wrongReturn \in BOOLEAN /\ completedFlow \in BOOLEAN
Spec == Init /\ [][Next]_vars
====
