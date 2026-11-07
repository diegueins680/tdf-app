------------------------ MODULE TermsIdentityUpgrade ------------------------
EXTENDS TLC
CONSTANTS FenceWriters, AtomicInstall, ValidateHistory
VARIABLES phase, conflictingScopes, fenced
vars == <<phase, conflictingScopes, fenced>>
Scopes == {"course", "event"}
Init == /\ phase = "idle"
        /\ conflictingScopes = {}
        /\ fenced = FALSE
Writer == /\ ~fenced /\ phase # "installed"
          /\ \E scope \in Scopes \ conflictingScopes:
               /\ conflictingScopes' = conflictingScopes \cup {scope}
               /\ UNCHANGED <<phase, fenced>>
Begin == /\ phase = "idle"
         /\ phase' = "validating"
         /\ fenced' = FenceWriters
         /\ UNCHANGED conflictingScopes
Preflight == /\ phase = "validating"
             /\ phase' = IF conflictingScopes = {} \/ ~ValidateHistory
                          THEN "checked" ELSE "aborted"
             /\ fenced' = IF phase' = "aborted" THEN FALSE ELSE fenced
             /\ UNCHANGED conflictingScopes
Install == /\ phase = "checked"
           /\ phase' = "installed"
           /\ UNCHANGED <<conflictingScopes, fenced>>
EarlyUnlock == /\ phase = "checked" /\ fenced /\ ~AtomicInstall
               /\ fenced' = FALSE
               /\ UNCHANGED <<phase, conflictingScopes>>
Finish == /\ phase = "installed" /\ fenced
          /\ fenced' = FALSE
          /\ UNCHANGED <<phase, conflictingScopes>>
Next == Writer \/ Begin \/ Preflight \/ Install \/ EarlyUnlock \/ Finish \/ UNCHANGED vars
TypeOK == /\ phase \in {"idle", "validating", "checked", "aborted", "installed"}
          /\ conflictingScopes \subseteq Scopes
          /\ fenced \in BOOLEAN
InstalledHistoryValid == phase = "installed" => conflictingScopes = {}
Spec == Init /\ [][Next]_vars
=============================================================================
