---- MODULE AbortRecovery ----
EXTENDS Naturals
CONSTANTS HonorLatch, PersistIntent, RequireFreshBoot, RejectPostWrite
VARIABLES latch, normalWrite, intent, request, boot, admitted, resumed
vars == <<latch, normalWrite, intent, request, boot, admitted, resumed>>
Init == /\ latch = FALSE /\ normalWrite = FALSE /\ intent = FALSE
        /\ request = FALSE /\ boot = 0 /\ admitted = FALSE /\ resumed = FALSE
NormalWrite == /\ ~normalWrite /\ (~latch \/ ~HonorLatch)
               /\ normalWrite' = TRUE /\ resumed' = latch
               /\ UNCHANGED <<latch,intent,request,boot,admitted>>
Latch == /\ ~latch /\ (~normalWrite \/ ~RejectPostWrite)
         /\ latch' = TRUE
         /\ UNCHANGED <<normalWrite,intent,request,boot,admitted,resumed>>
RebootIntent == /\ latch /\ ~request
                /\ intent' = PersistIntent /\ request' = TRUE
                /\ UNCHANGED <<latch,normalWrite,boot,admitted,resumed>>
Reboot == /\ request /\ boot = 0 /\ boot' = 1
          /\ UNCHANGED <<latch,normalWrite,intent,request,admitted,resumed>>
Admit == /\ request /\ (~RequireFreshBoot \/ boot = 1)
         /\ admitted' = TRUE
         /\ UNCHANGED <<latch,normalWrite,intent,request,boot,resumed>>
Next == NormalWrite \/ Latch \/ RebootIntent \/ Reboot \/ Admit
TypeOK == /\ latch \in BOOLEAN /\ normalWrite \in BOOLEAN /\ intent \in BOOLEAN
          /\ request \in BOOLEAN /\ boot \in {0,1} /\ admitted \in BOOLEAN /\ resumed \in BOOLEAN
NoReleaseResume == ~resumed
PreWriteAbortOnly == ~(latch /\ normalWrite)
IntentBeforeReboot == request => intent
FreshBootAdmission == admitted => boot = 1
Spec == Init /\ [][Next]_vars
====
