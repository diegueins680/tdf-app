---- MODULE AbortServiceRecovery ----
EXTENDS Integers, Sequences
CONSTANTS RequireFreshBoot, PersistIntent, RejectReplay, RequireCluster, BindReceipt
VARIABLES boot, epochBoot, rebootFrom, stage, pending, submitted, effects,
          durableIntent, staleEffect, badReceipt, cluster, initialized
vars == <<boot,epochBoot,rebootFrom,stage,pending,submitted,effects,
          durableIntent,staleEffect,badReceipt,cluster,initialized>>
Init == /\ boot = 0 /\ epochBoot = 0 /\ rebootFrom = 0
        /\ stage = 0 /\ pending = FALSE /\ submitted = FALSE /\ effects = <<>>
        /\ durableIntent = FALSE /\ staleEffect = FALSE /\ badReceipt = FALSE
        /\ cluster \in BOOLEAN /\ initialized = FALSE
Reboot == /\ rebootFrom >= 0 /\ boot < 2 /\ boot' = boot+1
          /\ UNCHANGED <<epochBoot,rebootFrom,stage,pending,submitted,effects,
                         durableIntent,staleEffect,badReceipt,cluster,initialized>>
BeginEpoch == /\ rebootFrom >= 0 /\ (boot # rebootFrom \/ ~RequireFreshBoot)
              /\ epochBoot' = boot /\ rebootFrom' = -1 /\ stage' = 0
              /\ pending' = FALSE /\ submitted' = FALSE
              /\ staleEffect' = (staleEffect \/ boot = rebootFrom)
              /\ UNCHANGED <<boot,effects,durableIntent,badReceipt,cluster,initialized>>
Intent == /\ rebootFrom = -1 /\ epochBoot = boot /\ stage < 6 /\ ~pending
          /\ pending' = TRUE /\ submitted' = FALSE /\ durableIntent' = PersistIntent
          /\ UNCHANGED <<boot,epochBoot,rebootFrom,stage,effects,
                         staleEffect,badReceipt,cluster,initialized>>
Effect == /\ pending /\ rebootFrom = -1 /\ epochBoot = boot
          /\ (~submitted \/ ~RejectReplay) /\ Len(effects) < 13
          /\ (stage # 1 \/ cluster \/ ~RequireCluster)
          /\ effects' = Append(effects,<<boot,stage,durableIntent>>)
          /\ submitted' = TRUE
          /\ initialized' = (initialized \/ (stage = 1 /\ ~cluster))
          /\ UNCHANGED <<boot,epochBoot,rebootFrom,stage,pending,durableIntent,staleEffect,badReceipt,cluster>>
Observe == /\ pending /\ submitted /\ rebootFrom = -1 /\ epochBoot = boot
           /\ \E matches \in BOOLEAN:
                /\ (matches \/ ~BindReceipt)
                /\ badReceipt' = (badReceipt \/ ~matches)
           /\ stage' = stage+1 /\ pending' = FALSE /\ submitted' = FALSE
           /\ UNCHANGED <<boot,epochBoot,rebootFrom,effects,durableIntent,staleEffect,cluster,initialized>>
RequestReboot == /\ rebootFrom = -1 /\ epochBoot = boot /\ stage < 6 /\ boot < 2
                 /\ rebootFrom' = boot
                 /\ UNCHANGED <<boot,epochBoot,stage,pending,submitted,effects,
                                durableIntent,staleEffect,badReceipt,cluster,initialized>>
Next == Reboot \/ BeginEpoch \/ Intent \/ Effect \/ Observe \/ RequestReboot
TypeOK == /\ boot \in 0..2 /\ epochBoot \in 0..2 /\ rebootFrom \in {-1,0,1,2}
          /\ stage \in 0..6 /\ pending \in BOOLEAN /\ submitted \in BOOLEAN
          /\ durableIntent \in BOOLEAN /\ staleEffect \in BOOLEAN /\ badReceipt \in BOOLEAN
          /\ cluster \in BOOLEAN /\ initialized \in BOOLEAN
FreshEpoch == ~staleEffect
IntentBeforeEffect == \A i \in 1..Len(effects): effects[i][3]
NoSameBootReplay == \A i,j \in 1..Len(effects): i # j => <<effects[i][1],effects[i][2]>> # <<effects[j][1],effects[j][2]>>
NoImplicitInitialization == ~initialized
BoundObservation == ~badReceipt
Spec == Init /\ [][Next]_vars
====
