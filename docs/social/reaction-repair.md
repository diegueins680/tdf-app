# Explicit reaction retry model

`formal/social/Reaction.tla` explores four explicit active/inactive writes from an
empty reaction state. Its 15 bounded states check that a repeated active write does
not add evidence twice and removal does not erase prior addition evidence.

The updated audit dependency includes the upstream reaction helper, row locking,
explicit-state handling, original-time evidence repair and reaction identity
redaction. Those changes are preserved. This PR now adds the small executable model
and its runner integration; it does not replace that upstream implementation.

`evidence/backend-runtime-result.txt` records the historical 2,540-example test run
against the earlier candidate repair, before the dependency refresh. It is retained
as historical evidence, not qualification of the updated upstream implementation.
Current Stack/CI results must be recorded separately. The model does not cover
legacy evidence repair, timestamp collisions, all reaction-type transitions or
revocation-safe event authorization. These limits remain explicit.
