------------------------ MODULE CalendarConnection ------------------------
EXTENDS Naturals, TLC
CONSTANTS ConsumeOnce, RequirePersistence, GuardSession
VARIABLES epoch, consumed, autoRequests, pending, requestEpoch, persisted,
          displayed, displayEpoch, preference, settled
vars == <<epoch, consumed, autoRequests, pending, requestEpoch, persisted,
          displayed, displayEpoch, preference, settled>>
Init == /\ epoch = 0 /\ consumed = FALSE /\ autoRequests = 0
        /\ pending = FALSE /\ requestEpoch = 0 /\ persisted = FALSE
        /\ displayed = FALSE /\ displayEpoch = 0 /\ preference = TRUE
        /\ settled = FALSE
AutoReturn == /\ ~pending /\ (~settled \/ ~ConsumeOnce)
              /\ autoRequests < 2
              /\ (~ConsumeOnce \/ ~consumed)
              /\ consumed' = TRUE /\ autoRequests' = autoRequests + 1
              /\ pending' = TRUE /\ requestEpoch' = epoch
              /\ UNCHANGED <<epoch, persisted, displayed, displayEpoch, preference, settled>>
(* A retained browser marker cannot establish a persisted connection. *)
LoadPreference == /\ preference /\ ~displayed /\ ~RequirePersistence
                  /\ displayed' = TRUE /\ displayEpoch' = epoch
                  /\ UNCHANGED <<epoch, consumed, autoRequests, pending, requestEpoch, persisted, preference, settled>>
Response(ok) == /\ pending /\ pending' = FALSE /\ settled' = TRUE
                /\ persisted' = (persisted \/ ok)
                /\ IF ok /\ (~GuardSession \/ requestEpoch = epoch)
                      THEN /\ displayed' = TRUE /\ displayEpoch' = requestEpoch
                      ELSE UNCHANGED <<displayed, displayEpoch>>
                /\ UNCHANGED <<epoch, consumed, autoRequests, requestEpoch, preference>>
SwitchSession == /\ epoch < 2 /\ epoch' = epoch + 1
                 /\ displayed' = FALSE /\ displayEpoch' = epoch + 1
                 /\ UNCHANGED <<consumed, autoRequests, pending, requestEpoch, persisted, preference, settled>>
Next == AutoReturn \/ LoadPreference \/ Response(TRUE) \/ Response(FALSE) \/ SwitchSession
Spec == Init /\ [][Next]_vars /\ WF_vars(Response(TRUE) \/ Response(FALSE))
AtMostOneAutomaticExchange == autoRequests <= 1
OnlyPersistedConnection == displayed => persisted
CurrentSessionReceipt == displayed => displayEpoch = epoch
RequestSettles == pending ~> ~pending
=============================================================================
