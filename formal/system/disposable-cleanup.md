# Disposable recovery: DEPLOY-JOURNAL-001

`original-disposable-cleanup.py` is an abort-stage adapter, not a deployment
entrypoint. The service journal must bind a recorded changed boot, the original
prepared admission and the permanent restore lock. Legacy markers and partial
publications confer no deletion authority. The version-2 marker binds the release
nonce, plan/images, original admission and descriptor directory. Descriptors bind
exact creation configuration, immutable images, names and private directory
identities; current source-policy mismatch denies adoption.

Before each removal, the adapter re-admits original runtime configuration,
volumes, database presence and registered units, then requires a complete Docker
inventory containing the three originals and at most the two recorded disposables.
Unknown extras, missing originals, running disposables or changed descriptors
reject. A stopped canary with its database already absent can be removed without
starting that dependency. When both exist, their dependency CID must agree.

Only admitted full CIDs are removed, canary before database. Original CIDs are
excluded independently. No container is started or unpaused and no bind data,
archive, descriptor, volume or image is deleted. Lost/failed removal replies leave
the journal pending even if Docker completed removal. Same-epoch replay is denied;
a recorded subsequent boot allows a new attempt using retained evidence. Only
successful complete absence observations allow unlinking the matching marker.
Unknown or partially synchronized records remain operator-recovery obligations.

## Bounded model

`DisposableCleanup.tla` abstracts one protected original identity and two
disposables, three recorded recovery epochs and at most six removal submissions.
It models successful acknowledgement, loss before the external effect and loss
after the effect. The intended configuration checks `OriginalPreserved`,
`CanaryRemovedFirst`, `NoSameEpochReplay` and `MarkerRequiresAbsence`. Four controlled
configurations independently remove each guard and must produce the named invariant
violation. Deadlock checking is disabled: the final state and exhausted bounds can
stutter. No fairness or liveness property is claimed.

The abstraction assumes authenticated durable descriptors, exact original identity,
serial ownership, truthful complete Docker observations, and that a recorded new
boot quiesces preceding daemon requests. It excludes source-installation attacks,
privileged uncoordinated creation/rename, filesystem durability failures and
container-internal effects. Real no-follow file checks, journals and failure tests
cover portions of those boundaries separately; there is no whole-program refinement
proof.

| Requirement property | Model | Implementation | Executable evidence |
| --- | --- | --- | --- |
| Preserve original services | OriginalPreserved | typed descriptor admission; explicit original-CID exclusion | original-disposable-cleanup tests; descriptor tests |
| Remove dependencies in order | CanaryRemovedFirst | fixed canary then database loop; dependency CID binding | actual Linux creation/cleanup fixture; portable ordering test |
| Do not replay uncertain effects in an epoch | NoSameEpochReplay | service journal pending intent; recorded boot admission | lost reply after real removal; portable before/after controls |
| Retain reservation until absence is observed | MarkerRequiresAbsence | complete inventory and inode-bound marker release | missing/unknown/partial/inventory-error tests |

The owned Linux extension uses real PostgreSQL and immutable backend images,
records creation before Docker dispatch, runs the real isolated canary, injects
cleanup interruption and exercises actual original-runtime admission. Its boot
identities and capture/encryption/retrieval journal observations are synthetic;
it proves neither actual reboot behavior nor encrypted off-host custody. The
complete release coordinator, first legacy shutdown treatment, production custody
and rollout remain separate unfinished obligations.
