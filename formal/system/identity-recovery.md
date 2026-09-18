# Identity-preserving release recovery

Scope addition SYS-D05, discovered while preparing this audit's release. The accepted contracts
in `docs/identity/intake-prevention.md`, `course-prevention.md`, `guest-booking-prevention.md`
and `trial-prevention.md` prohibit routine restoration of contact-based identity adoption and
require retention of request receipts/replay protections. These are existing requirements.

## Concrete counterexample

At audit head `af9309c2892ac84b377999143cd47a41dc2ed90d`, executing the actual
`rollbackCompatibility` with the full 113-migration manifest and current production source
`ab9bbacc9da845b6bfe70ac3fda2ace44f17c918` returned `compatible: true`. It checked only
provider floor `c53b33e7ef868fb7b64f876199ed66be0f617efc`. The prior image predates the
intake/course/trial corrections. In a failed canary rollout, `selectRecoveryTarget` would choose
that prior image even if a newer fallback were supplied. No failing production rollout was
performed to reproduce this; the actual decision function was executed without a deploy callback.

## Contract and implementation correspondence

The canonical implementation is reused from concurrent PR #453, source
`261bfdfbbdab0520b35725a72b776f160b608dd3`, rather than maintaining a competing floor policy.
The initial audit-only snapshot floors in `evidence/identity-recovery.json` are historical and
superseded by the domain's reviewed writer floors below. Those earlier floors unnecessarily
required later merge commits; actual writer provenance supplies a more precise boundary.

Let `R(M)` be `requiredIdentityCommit` for target migration manifest M, and `A(r,c)` the trusted
Git ancestry predicate. The actual `rollbackCompatibility` admits candidate c exactly when
`R(M) = null` or `A(R(M), c)`. Missing Git history/errors propagate as failure.

| Target migration present (strongest first) | Required reviewed implementation snapshot |
|---|---|
| course, trial or ads request receipts | `6eab8592744015124b0162ce9e9361f51a04f538` |
| intake idempotency | `02115f7d1b0786f3cdd4287a9466dd22682f603b` |
| provider subject identity | `c53b33e7ef868fb7b64f876199ed66be0f617efc` |
| none | no identity floor from these contracts |

These are implementation snapshots, not SQL-introduction commits. Actual Git ancestry checks
confirm that each later writer snapshot includes the earlier floor. The full target includes
the trial/ads writer, also carrying the guest-booking corrections. The minimum is used by dry-run
reporting, remote preflight, fallback validation and the final pre-deploy guard. A prior failing
this predicate requires a distinct reviewed, immutable fallback with the same migration manifest
and SQL checksums. Existing recovery handles every unsafe replica after a deploy attempt.

Regression tests invoke the actual guard and verify zero deploy calls for a provider-safe but
contact-unsafe candidate, successful forward recovery, all earlier migration subsets/orderings
with trial/ads requirements, missing-history failure, and existing mixed-fleet/failure behavior.
This is executable contract and bounded correspondence evidence, not an unrestricted program proof.

`formal/event-operations/ProviderRollback.tla` and its six existing configurations model the
recovery protocol using abstract compatibility predicates. Four successful configurations and
two required counterexamples test fleet safety, retained binding state and fairness-conditional
recovery progress. The Boolean protocol is unchanged; this correction strengthens the concrete
classification to discharge the domain assumption that a "safe" binary preserves applicable
identity requirements. The model does not prove SQL receipt contents, Git, Fly, or availability.
Commit ancestry assumes descendants do not deliberately revert the contract; an intentional
revert requires a new reviewed policy, not automatic approval from this ancestry check.

Reproduce with `node --test scripts/__tests__/provider-rollback.test.mjs`, the existing pinned
formal runner, and `node scripts/production-release.mjs plan --sha <full-reviewed-sha>`.
No migrations, identity merges, receipt deletion, or production cleanup are part of this repair.
