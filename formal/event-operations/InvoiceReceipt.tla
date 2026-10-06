--------------------------- MODULE InvoiceReceipt ---------------------------
EXTENDS Naturals, FiniteSets, Sequences
CONSTANTS IgnoreInvoiceLock, StaleCounter, IgnoreReplayBinding, IgnoreAuthorization
Actors == {1,2}
Invoices == {1,2}
Payloads == {"original","changed"}
VARIABLES stage, target, payload, seen, allocated, active, counter, receipts, responses
vars == <<stage,target,payload,seen,allocated,active,counter,receipts,responses>>
Init == /\ stage=[a \in Actors |-> "idle"]
        /\ target=[a \in Actors |-> 1] /\ payload=[a \in Actors |-> "original"]
        /\ seen=[a \in Actors |-> 0] /\ allocated=[a \in Actors |-> 0]
        /\ active=TRUE /\ counter=0 /\ receipts = <<>> /\ responses = <<>>
Begin(a,i,p) ==
  /\ stage[a]="idle"
  /\ IgnoreInvoiceLock \/ ~\E b \in Actors: stage[b]="ready" /\ target[b]=i
  /\ stage'=[stage EXCEPT ![a]="ready"]
  /\ target'=[target EXCEPT ![a]=i] /\ payload'=[payload EXCEPT ![a]=p]
  /\ seen'=[seen EXCEPT ![a]=Cardinality({n \in 1..Len(receipts): receipts[n].invoice=i})]
  /\ allocated'=[allocated EXCEPT ![a]=counter+1]
  /\ UNCHANGED <<active,counter,receipts,responses>>
Revoke == /\ active /\ active'=FALSE
          /\ UNCHANGED <<stage,target,payload,seen,allocated,counter,receipts,responses>>
Issue(a) ==
  /\ stage[a]="ready" /\ seen[a]=0
  /\ active \/ IgnoreAuthorization
  /\ stage'=[stage EXCEPT ![a]="done"] /\ counter'=counter+1
  /\ receipts'=Append(receipts,[invoice |-> target[a], body |-> payload[a],
       number |-> IF StaleCounter THEN allocated[a] ELSE counter+1, authorized |-> active])
  /\ UNCHANGED <<target,payload,seen,allocated,active,responses>>
Replay(a) ==
  /\ stage[a]="ready" /\ seen[a]>0
  /\ active \/ IgnoreAuthorization
  /\ \E n \in 1..Len(receipts):
       /\ receipts[n].invoice=target[a]
       /\ IgnoreReplayBinding \/ receipts[n].body=payload[a]
       /\ responses'=Append(responses,[requested |-> payload[a], returned |-> receipts[n].body, authorized |-> active])
  /\ stage'=[stage EXCEPT ![a]="done"]
  /\ UNCHANGED <<target,payload,seen,allocated,active,counter,receipts>>
Next == Revoke \/ (\E a \in Actors,i \in Invoices,p \in Payloads: Begin(a,i,p))
        \/ (\E a \in Actors: Issue(a) \/ Replay(a))
UniqueInvoice == \A x,y \in 1..Len(receipts): receipts[x].invoice=receipts[y].invoice => x=y
UniqueNumber == \A x,y \in 1..Len(receipts): receipts[x].number=receipts[y].number => x=y
BoundReplay == \A n \in 1..Len(responses): responses[n].requested=responses[n].returned
AuthorizedCommit == (\A n \in 1..Len(receipts): receipts[n].authorized)
                    /\ (\A n \in 1..Len(responses): responses[n].authorized)
Spec == Init /\ [][Next]_vars
=============================================================================
