# Historical memory deletion after canonical cutover

Review [4385289256](https://github.com/diegueins680/tdf-app/pull/608#discussion_r4385289256) identified a case outside the earlier qualification: an author's memory can still have frozen legacy comments/reactions after canonical interactions are activated. The handler explicitly deleted those children at trigger depth one, which correctly trips the existing legacy-write fence and rolls back the whole HTTP request.

## Observed reproduction

The actual67f API/build2026-10-27T04:56:30Z, SHA25685dac8cbb8f1bc43df9f58446c7d399b24ee23285645919385da707aa315d051, returned500 for the author's DELETE after activation. A pre-activation DELETE returned204, and unauthorized/cross-event attempts returned404. Historical moments, reactions and comments were created through real HTTP handlers before the actual guarded activation; no trigger, fence or canonical row was fabricated. The failed runner cleaned up its API and disposable database. [Failed reproduction receipt](event-memory-source-evidence-20261025/main-1129/legacy-delete-reproduction.json).

The earlier127-request/13-browser qualification and c26 report retain their original scope; they did not exercise frozen historical children. They are not a passing result for this new regression. PR608 was returned to draft while it is repaired; the reviewer thread is not resolved by the author.

## Correction and acceptance

Reuse `Interactions.activated`, which checks `activated_once`, rather than the current command-enable flag. Before cutover, retain explicit child cleanup for legacy persistence, including SQLite. After cutover, delete the parent only: the existing `interaction_delete_moment_source()` trigger clears frozen children at the permitted nested depth, erases canonical discussion bodies/reactions and retains audit/tombstone identity. The existing retirement trigger retires the target. No migration, trigger bypass or permission change is introduced.

The existing actual-HTTP runner now requires all of these outcomes:

- Legacy parent deletion with real children succeeds before activation.
- Parent deletion with migrated children succeeds both while commands are enabled and after they are paused.
- Direct child deletion remains fenced with SQLSTATE55000.
- Another author and an incorrect event still receive404.
- Parent/legacy children are removed, canonical bodies are erased, reactions cleared and targets retired.

The initial correction commit recorded qualification as pending. The fresh4ced binary now passes the new regression and the complete existing API/browser suite: **143requests/21groups/277current+committed source hashes/13browser assertions**. Actual API4ced/build2026-10-27T05:27:53Z, Darwin SHA25651037e6c5e75524cb773850afac1fd3833c2e1804c7683f5ed9a11a562539aa3, PostgreSQL16/Chrome150. All three owner deletions return204, unauthorized/cross-event requests404; both direct-child fence checks still raise55000. Canonical bodies/reactions are cleared and targets retired. Registration/photograph and recovery/text still survive API restarts, without fixture role grants or email delivery.

[Corrected qualification and limits](event-memory-source-evidence-20261025/main-1129/legacy-delete-qualification.json) bind the separate positive execution; the failed original receipt remains unchanged. The new formal execution binds1406current+committed files exactly but keeps154global requirements UNPROVEN/releaseBLOCKED/executionAllowedfalse. HostedCI and independent final-head review retain their own status. The author does not resolve the review conversation. No production write, deployment or invitation occurs.
