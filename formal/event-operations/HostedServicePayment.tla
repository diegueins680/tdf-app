---- MODULE HostedServicePayment ----
EXTENDS Naturals, FiniteSets
CONSTANTS Workers, UnsafeAtomic, UnsafeReplay, UnsafeProgress
VARIABLES paid, order, audits, observed, advanced, validBinding
vars == <<paid, order, audits, observed, advanced, validBinding>>
Init == /\ paid = FALSE /\ order = "unpaid" /\ audits = 0
        /\ observed = {} /\ advanced = FALSE /\ validBinding \in BOOLEAN
Apply(worker) ==
  /\ worker \notin observed /\ validBinding
  /\ paid' = TRUE
  /\ order' = IF UnsafeAtomic THEN order
               ELSE IF order = "unpaid" \/ UnsafeProgress THEN "paid" ELSE order
  /\ audits' = IF ~paid \/ UnsafeReplay THEN audits + 1 ELSE audits
  /\ observed' = observed \cup {worker}
  /\ UNCHANGED <<advanced, validBinding>>
Fulfill == /\ paid /\ order = "paid" /\ ~advanced
           /\ order' = "in_progress" /\ advanced' = TRUE
           /\ UNCHANGED <<paid, audits, observed, validBinding>>
Next == (\E worker \in Workers : Apply(worker)) \/ Fulfill
TypeOK == /\ paid \in BOOLEAN /\ order \in {"unpaid", "paid", "in_progress"}
          /\ audits \in 0..Cardinality(Workers) /\ observed \subseteq Workers
          /\ advanced \in BOOLEAN /\ validBinding \in BOOLEAN
NoMissingFulfillment == paid => order # "unpaid"
ExactlyOnePaidAudit == audits = IF paid THEN 1 ELSE 0
NoFulfillmentRegression == advanced => order = "in_progress"
BindingRequired == paid => validBinding
Spec == Init /\ [][Next]_vars
====
