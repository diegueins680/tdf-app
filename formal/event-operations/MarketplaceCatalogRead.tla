--------------------- MODULE MarketplaceCatalogRead ---------------------
EXTENDS Naturals, TLC
CONSTANTS SelectedIds, Approved
VARIABLES active, selected, phase, hasTerms, price
vars == <<active, selected, phase, hasTerms, price>>
Init == /\ active = TRUE /\ selected = FALSE /\ phase = "arrival"
        /\ hasTerms = FALSE /\ price = 0
SelectListing == /\ phase = "arrival" /\ selected' = active
                 /\ phase' = "selected"
                 /\ UNCHANGED <<active, hasTerms, price>>
DeliverSale == /\ active /\ active' = FALSE
               /\ UNCHANGED <<selected, phase, hasTerms, price>>
ReadTerms == /\ phase = "selected"
             /\ hasTerms' = (Approved /\ selected /\ (SelectedIds \/ active))
             /\ phase' = "terms"
             /\ UNCHANGED <<active, selected, price>>
Render == /\ phase = "terms" /\ phase' = "done"
          /\ price' = IF ~selected THEN 0 ELSE IF hasTerms THEN 2 ELSE 10
          /\ UNCHANGED <<active, selected, hasTerms>>
Next == SelectListing \/ DeliverSale \/ ReadTerms \/ Render
Spec == Init /\ [][Next]_vars /\ WF_vars(SelectListing) /\ WF_vars(ReadTerms) /\ WF_vars(Render)
SelectedRentalKeepsApprovedTerms == phase = "done" /\ selected /\ Approved => price = 2
UnapprovedTermsNotUsed == phase = "done" /\ selected /\ ~Approved => price = 10
NoUnselectedListing == phase = "done" /\ ~selected => price = 0
RequestFinishes == <> (phase = "done")
=============================================================================
