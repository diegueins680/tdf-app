# Parallel development and serial main integration

Branches, reviews and CI run in parallel. The additional required status
`main-integration-admission` admits the oldest open, non-draft PR targeting main.
All other PRs wait for that integration while their work and checks continue.
Mark an intentionally unfinished candidate draft to let the next ready PR proceed.
PRs sharing a commit head require consolidation because a commit status cannot
distinguish between them. Non-main dependency branches retain their own policies.

The metadata-only controller runs from **main**, including on
`pull_request_target`, in one non-cancelling Actions concurrency group. It never
checks out or executes a PR head, approves reviews, merges PRs, changes branch
protection, or reports a quality test as passed. It revokes other admissions before
granting one, compares fresh head/base/queue observations, and fails closed on an
incomplete observation or status write. Repeated observations do not repost an
unchanged status. Push-to-main and a five-minute reconciliation repair missed or
coalesced events. GitHub scheduling is not a five-minute availability guarantee.

Main must continue to enforce strict up-to-date checks, stale-review dismissal,
independent approval, conversation resolution and every existing required check.
The admission is an extra scheduling condition, not release evidence. Strict
base protection rejects an admitted head if main advances after observation.
Only GitHub can atomically enforce that final branch update; this script does not
claim a cross-API transaction or protection against a repository administrator
deliberately changing settings/statuses. Other writers should leave source PRs
open until their successor is merged and verified; no source auto-merge needs to
be disabled merely to protect the integration slot.

Inspect without writing:

```sh
python3 scripts/merge-admission.py
python3 scripts/test-merge-admission.py
```

Bootstrap the required status only after these sources/tests are validated and
prepared for independent review: retain
the existing protection JSON, use this same controller once from the clean tested
checkout to publish metadata statuses, then add its context to required checks
without removing or weakening any existing policy. Bootstrap grants only a scheduling slot; independent approval and every existing
quality gate must still pass before the implementation can merge. Once merged, the main workflow
owns reconciliation; do not run a competing local writer. The controller needs
only repository metadata reads and commit-status writes. Keep the status issuer
compatible with both the bootstrap operator and the GitHub Actions token.

If the workflow is unavailable, merges fail closed while development continues.
Repair/rerun the main workflow. A deliberate retirement of this mechanism must
remove only this scheduling context under a reviewed policy change, preserving
all quality/review checks; do not use removal to waive a failing candidate gate.
