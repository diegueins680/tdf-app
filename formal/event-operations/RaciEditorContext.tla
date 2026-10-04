---- MODULE RaciEditorContext ----
EXTENDS Naturals, TLC
CONSTANTS FreshAuthority, FilterCandidates, FenceMetadata
VARIABLES phase, grant, now, matching, candidateEligible, earlyGrant, earlyTime,
          revision, captured, badPrivacy, badCandidate, badRevision
vars == <<phase, grant, now, matching, candidateEligible, earlyGrant, earlyTime,
          revision, captured, badPrivacy, badCandidate, badRevision>>
CanRead(g,t) == g # "none" /\ t < 2 /\ matching
CanManage(g,t) == g = "manage" /\ CanRead(g,t)
Init == /\ phase = "new" /\ grant = "manage" /\ now = 0
        /\ matching \in BOOLEAN /\ candidateEligible \in BOOLEAN
        /\ earlyGrant = "manage" /\ earlyTime = 0 /\ revision = 1 /\ captured = 1
        /\ badPrivacy = FALSE /\ badCandidate = FALSE /\ badRevision = FALSE
Begin == /\ phase = "new" /\ phase' = "waiting"
         /\ earlyGrant' = grant /\ earlyTime' = now
         /\ UNCHANGED <<grant,now,matching,candidateEligible,revision,captured,
                        badPrivacy,badCandidate,badRevision>>
Acquire == /\ phase = "waiting" /\ phase' = "held" /\ captured' = revision
           /\ UNCHANGED <<grant,now,matching,candidateEligible,earlyGrant,earlyTime,
                          revision,badPrivacy,badCandidate,badRevision>>
Project == /\ phase = "held" /\ phase' = "done"
           /\ LET g == IF FreshAuthority THEN grant ELSE earlyGrant
                  t == IF FreshAuthority THEN now ELSE earlyTime
                  options == CanManage(g,t)
              IN /\ badPrivacy' = (badPrivacy \/ (options /\ ~CanManage(grant,now)))
                 /\ badCandidate' = (badCandidate \/ (options /\ ~FilterCandidates /\ ~candidateEligible))
                 /\ badRevision' = (badRevision \/ (CanRead(g,t) /\ captured # revision))
           /\ UNCHANGED <<grant,now,matching,candidateEligible,earlyGrant,earlyTime,revision,captured>>
ChangeGrant(g) == /\ phase \in {"new","waiting"} /\ g \in {"manage","read","none"}
                  /\ grant' = g
                  /\ UNCHANGED <<phase,now,matching,candidateEligible,earlyGrant,earlyTime,
                                 revision,captured,badPrivacy,badCandidate,badRevision>>
Tick == /\ now < 2 /\ now' = now + 1
        /\ UNCHANGED <<phase,grant,matching,candidateEligible,earlyGrant,earlyTime,
                       revision,captured,badPrivacy,badCandidate,badRevision>>
Write == /\ revision = 1 /\ (~FenceMetadata \/ phase # "held") /\ revision' = 2
         /\ UNCHANGED <<phase,grant,now,matching,candidateEligible,earlyGrant,earlyTime,
                        captured,badPrivacy,badCandidate,badRevision>>
Next == Begin \/ Acquire \/ Project \/ Tick \/ Write \/ (\E g \in {"manage","read","none"}: ChangeGrant(g))
TypeOK == /\ phase \in {"new","waiting","held","done"} /\ grant \in {"manage","read","none"}
          /\ earlyGrant \in {"manage","read","none"} /\ now \in 0..2 /\ earlyTime \in 0..2
          /\ revision \in 1..2 /\ captured \in 1..2 /\ matching \in BOOLEAN /\ candidateEligible \in BOOLEAN
          /\ badPrivacy \in BOOLEAN /\ badCandidate \in BOOLEAN /\ badRevision \in BOOLEAN
PrivateOptions == ~badPrivacy
EligibleOptions == ~badCandidate
CoherentContext == ~badRevision
Spec == Init /\ [][Next]_vars
====
