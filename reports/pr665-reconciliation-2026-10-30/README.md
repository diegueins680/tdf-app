# PR 665 reconciliation checkpoint — 30 October 2026

Status: **BLOCKED; preparation only. Not hosted-CI-qualified, merged or deployed.**

The local composition joins original PR head `944f821409a3a6d445164adc5403649457008ec3`
and protected main `e1cec5b59723c2ff9828ac4fce45abb930d73055`.
The tested source checkpoint is `7df06d95df036376a8ce9baf72195696d8bac86b`.
Main includes booking repair #638, artist review #641, and music search #640.
The original #665 head is left unchanged until its dependency can be published.

## Composition and preservation

- Web implementation merged without code conflicts. No new product behavior was added.
- Preserve every one of main's 158 requirement objects exactly and append the exact
  existing `WEB-CONTENT-001` object. Regenerate inventory and traceability; no claim
  of parser correctness or universal refinement is added.
- Preserve catalog review meanings from both parents. The removed dispatch decision
  `56e2b5d3ce52bde290e8` remains verbatim in main's existing
  `musicTrackSearchDispatchSuccessor.previousDecision` history.
- Preserve main's booking implementation and recovery test byte for byte. The
  public-welcome/gallery browser test also matches main; no threshold, assertion,
  retry, exclusion or accessibility rule was relaxed.
- Mobile pin `88d583f32bd6ac398dc0c2d7d36ac310568acf3b` is a **local, unpublished**
  descendant of main's `7586be92f447faaf05d1f7d7073af6321ad1496a` pin. It changes
  only `src/api/generated/types.ts`: eight additions and one removal. Generated
  bytes match the web client exactly. All existing music-search and scoped
  event-memory contract fields remain. No mobile runtime code changes.

## Dependency requiring authorization

The root-declared submodule destination is
`https://github.com/diegueins680/tdf-mobile.git`.
Automatic approval review rejected publishing the local mobile commit to branch
`integrate/markdown-profile-contract-20261030`, treating that separate-repository
write as outside the authorization for #665, including after destination,
changed-path, exact nine-line scope and generated-byte checks. No mobile write
succeeded. Do not retry via another tool, repository or indirect mechanism.

`mobile-api-contract.patch` is the exact reviewable change. Publishing it requires
explicit user authorization. Until then, the local composition's gitlink cannot
be fetched by hosted CI; do not update #665 to this incomplete remote dependency
or describe its old checks/approvals as qualifying this composition.

## Protected integration and deployment

The metadata read identifies `main` as protected and lists twenty required status
contexts, including `main-integration-admission`. Full protection read returned
403 for the GitHub integration, so that read does not establish every review rule.
The repository's `docs/main-integration-admission.md` requires strict up-to-date
checks, stale-review dismissal, independent approval, conversation resolution and
FIFO admission. Earlier non-draft PRs remain ahead of #665, starting with #642
at the observation in this session. None were altered to obtain a turn.

The original card-navigation review thread is resolved. Existing tdfrecords
approvals are on the old head; they do not constitute approval of this new
composition. After publishing the mobile dependency, update #665, run all hosted
checks on the final head, obtain current independent approval/review, inspect
threads again, and wait for protected admission. Merge through the ordinary PR
path only when all controls pass. Authorized deployment verification follows an
actual merge and successful release; no production claim is made here.

## Browser limitation and unresolved finding

The local Playwright download returned a 195-byte HTML response instead of a
browser archive and extraction failed. No current-composition WebKit/browser
pass exists. The historical iPhone 320px overflow failure remains **unresolved by
execution**: preserving #638's fix is not evidence that it disappears. Hosted
CI must actually execute booking recovery at 320px, native resize acknowledgment,
root width/scale, helper geometry, keyboard and scroll assertions, alongside
Markdown/profile navigation, accessibility and privacy checks.

Any gallery contrast failure must keep its own evidence and diagnosis; neither
its assertions nor its product code was changed here.

Validation results and exact limitations are recorded in `validation.json`.
The remote `review/pr665-mobile-dependency-20261030` branch contains only the
review packet, based on protected main. The reconciled source and complete logs
remain in the local integration checkout: ordinary Git push to the root
repository failed because this terminal has no GitHub credential. The GitHub
connector is used only to publish the small review packet, without changing
#665 or its mobile dependency. The review-packet branch is not a merge candidate.
