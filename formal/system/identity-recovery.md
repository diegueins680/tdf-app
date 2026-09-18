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

Let `R(M)` be `minimumIdentityCommit` for target migration manifest M, and `A(r,c)` the trusted
Git ancestry predicate. The actual `rollbackCompatibility` admits candidate c exactly when
`R(M) = null` or `A(R(M), c)`. Missing Git history/errors propagate as failure.

| Target migration present (strongest first) | Required reviewed implementation snapshot |
|---|---|
| trial or ads request receipts | `d7ebacbff0f0e35dbd57238a8afa6f11e86cdb0e` |
| course request receipts | `418c0da63a8a95639866f1e92619e6a00b7640c4` |
| intake idempotency | `497286e82ca4d6a52cdf2b52e7fa8d65e92e0711` |
| provider subject identity | `c53b33e7ef868fb7b64f876199ed66be0f617efc` |
| none | no identity floor from these contracts |

These are implementation snapshots, not SQL-introduction commits. Actual Git ancestry checks
confirm that each later snapshot includes the earlier floor. The full target includes the final
trial/ads snapshot, also carrying the guest-booking corrections. The minimum is used by dry-run
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
