---- MODULE TaskRevisionRead ----
EXTENDS Naturals, TLC
CONSTANTS FenceMetadata, FreshAuthorization
VARIABLES generation, revision, reader, writer, authLock, metadataLock,
          grant, now, earlyAllowed, capturedRevision, payloadGeneration,
          visible, allowedAtProjection
vars == <<generation, revision, reader, writer, authLock, metadataLock,
          grant, now, earlyAllowed, capturedRevision, payloadGeneration,
          visible, allowedAtProjection>>
Allowed == grant /\ now < 2
Init ==
  /\ generation = 0 /\ revision = 1
  /\ reader = "idle" /\ writer = "idle"
  /\ authLock = FALSE /\ metadataLock = FALSE
  /\ grant = TRUE /\ now = 0 /\ earlyAllowed = FALSE
  /\ capturedRevision = 1 /\ payloadGeneration = 0
  /\ visible = FALSE /\ allowedAtProjection = FALSE
BeginRead ==
  /\ reader = "idle"
  /\ reader' = "waiting" /\ authLock' = TRUE /\ earlyAllowed' = Allowed
  /\ UNCHANGED <<generation, revision, writer, metadataLock, grant, now,
                 capturedRevision, payloadGeneration, visible, allowedAtProjection>>
CaptureRevision ==
  /\ reader = "waiting" /\ writer # "locked"
  /\ reader' = "captured" /\ capturedRevision' = revision
  /\ metadataLock' = FenceMetadata
  /\ UNCHANGED <<generation, revision, writer, authLock, grant, now,
                 earlyAllowed, payloadGeneration, visible, allowedAtProjection>>
Project ==
  /\ reader = "captured"
  /\ reader' = "done" /\ payloadGeneration' = generation
  /\ allowedAtProjection' = Allowed
  /\ visible' = IF FreshAuthorization THEN Allowed ELSE earlyAllowed
  /\ authLock' = FALSE /\ metadataLock' = FALSE
  /\ UNCHANGED <<generation, revision, writer, grant, now, earlyAllowed, capturedRevision>>
BeginWrite ==
  /\ writer = "idle" /\ writer' = "uncommitted"
  /\ UNCHANGED <<generation, revision, reader, authLock, metadataLock, grant, now,
                 earlyAllowed, capturedRevision, payloadGeneration, visible, allowedAtProjection>>
LockWrite ==
  /\ writer = "uncommitted" /\ ~metadataLock /\ writer' = "locked"
  /\ UNCHANGED <<generation, revision, reader, authLock, metadataLock, grant, now,
                 earlyAllowed, capturedRevision, payloadGeneration, visible, allowedAtProjection>>
CommitWrite ==
  /\ writer = "locked" /\ writer' = "done"
  /\ generation' = generation + 1 /\ revision' = revision + 1
  /\ UNCHANGED <<reader, authLock, metadataLock, grant, now,
                 earlyAllowed, capturedRevision, payloadGeneration, visible, allowedAtProjection>>
Revoke ==
  /\ grant /\ ~authLock /\ grant' = FALSE
  /\ UNCHANGED <<generation, revision, reader, writer, authLock, metadataLock, now,
                 earlyAllowed, capturedRevision, payloadGeneration, visible, allowedAtProjection>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<generation, revision, reader, writer, authLock, metadataLock, grant,
                 earlyAllowed, capturedRevision, payloadGeneration, visible, allowedAtProjection>>
Next == BeginRead \/ CaptureRevision \/ Project \/ BeginWrite \/ LockWrite \/ CommitWrite \/ Revoke \/ Tick
TypeOK ==
  /\ generation \in 0..1 /\ revision \in 1..2
  /\ reader \in {"idle", "waiting", "captured", "done"}
  /\ writer \in {"idle", "uncommitted", "locked", "done"}
  /\ authLock \in BOOLEAN /\ metadataLock \in BOOLEAN /\ grant \in BOOLEAN
  /\ now \in 0..3 /\ earlyAllowed \in BOOLEAN
  /\ capturedRevision \in 1..2 /\ payloadGeneration \in 0..1
  /\ visible \in BOOLEAN /\ allowedAtProjection \in BOOLEAN
CoherentRevisionRead == reader = "done" /\ visible => capturedRevision = payloadGeneration + 1
NoExpiredDisclosure == reader = "done" /\ visible => allowedAtProjection
Spec == Init /\ [][Next]_vars
====
