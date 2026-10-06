------------------ MODULE DirectoryFavoriteAuthority ------------------
EXTENDS Naturals, TLC
CONSTANTS GuardDispatch, GuardReceipt
VARIABLES epoch, rendered, mounted, phase, owner, dispatchedAt, persisted,
          visible, receiptEpoch, kind
vars == <<epoch, rendered, mounted, phase, owner, dispatchedAt, persisted,
          visible, receiptEpoch, kind>>
Init == /\ epoch = 0 /\ rendered = 0 /\ mounted = TRUE
        /\ phase = "idle" /\ owner = 0 /\ dispatchedAt = 0
        /\ persisted = FALSE /\ visible = FALSE /\ receiptEpoch = 0
        /\ kind \in {"read", "save"}
Queue == /\ mounted /\ phase = "idle"
         /\ phase' = "queued" /\ owner' = rendered
         /\ UNCHANGED <<epoch, rendered, mounted, dispatchedAt, persisted, visible, receiptEpoch, kind>>
Dispatch == /\ phase = "queued"
            /\ IF ~GuardDispatch \/ (mounted /\ owner = epoch)
                  THEN /\ phase' = "pending" /\ dispatchedAt' = epoch
                  ELSE /\ phase' = "done" /\ UNCHANGED dispatchedAt
            /\ UNCHANGED <<epoch, rendered, mounted, owner, persisted, visible, receiptEpoch, kind>>
Response(ok) == /\ phase = "pending" /\ phase' = "done"
                /\ persisted' = ok
                /\ IF ok /\ (IF GuardReceipt THEN mounted /\ owner = epoch
                              ELSE (owner % 2) = (rendered % 2))
                      THEN /\ visible' = TRUE /\ receiptEpoch' = owner
                      ELSE UNCHANGED <<visible, receiptEpoch>>
                /\ UNCHANGED <<epoch, rendered, mounted, owner, dispatchedAt, kind>>
Switch == /\ epoch < 2 /\ epoch' = epoch + 1
          /\ visible' = FALSE
          /\ UNCHANGED <<rendered, mounted, phase, owner, dispatchedAt, persisted, receiptEpoch, kind>>
Render == /\ mounted /\ rendered # epoch /\ rendered' = epoch
          /\ visible' = FALSE
          /\ UNCHANGED <<epoch, mounted, phase, owner, dispatchedAt, persisted, receiptEpoch, kind>>
Unmount == /\ mounted /\ mounted' = FALSE /\ visible' = FALSE
           /\ UNCHANGED <<epoch, rendered, phase, owner, dispatchedAt, persisted, receiptEpoch, kind>>
Next == Queue \/ Dispatch \/ Response(TRUE) \/ Response(FALSE) \/ Switch \/ Render \/ Unmount
Spec == Init /\ [][Next]_vars /\ WF_vars(Dispatch) /\ WF_vars(Response(TRUE) \/ Response(FALSE))
AuthorizedDispatch == phase = "pending" => owner = dispatchedAt
CurrentSessionReceipt == visible => mounted /\ receiptEpoch = epoch
PersistedReceipt == visible => persisted
RequestSettles == phase = "queued" ~> phase = "done"
=============================================================================
