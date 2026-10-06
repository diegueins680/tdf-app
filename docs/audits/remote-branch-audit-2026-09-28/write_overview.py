import pathlib,json,collections,datetime
P=pathlib.Path(__file__).parent;R=P/'report'
F=sorted(p for p in P.glob('final-*') if p.is_dir() and (p/'completed.txt').exists())[-1]
def read(name,default=None):
 f=P/name
 return json.loads(f.read_text()) if f.exists() else default
def flat(file):return [x for pg in json.loads(file.read_text()) for x in pg]
rec=read('report/reconciliation.json');initial=read('report/branches.json');added=read('report/added-branches.json');rows=initial+added
mut=[json.loads(x) for x in (P/'mutations.jsonl').read_text().splitlines()]
manual={m['pr']:m for m in mut if m.get('action')=='merged_pull_request'}
automatic={m['pr']:m for m in mut if m.get('action')=='observed_automatic_merge'}
automatic.setdefault(460,{'merge_commit':'de76dc7df247937144e1802c17d1842f983cbe13'})
merges=set(manual)|set(automatic);closures={m['pr']:m for m in mut if m.get('action')=='closed_unmerged'}
counts=collections.Counter(r['classification'] for r in rows);icounts=collections.Counter(r['classification'] for r in initial)
ops=[r for r in rows if r['classification']=='PROTECTED_OR_OPERATIONAL'];live_ops=sum(r['remote_present'] for r in ops)
def prlink(n):return f'[#{n}](https://github.com/diegueins680/tdf-app/pull/{n})'
event=read('merge465-verified.json');event_ci=read('events-closure-main-ci.json',{});event_closed=[n for n,m in closures.items() if m.get('replacement')==465]
event_auto=[n for n,m in automatic.items() if m.get('replacement')==465]
latestprs={p['number']:p for p in flat(F/'pulls.json')}
text=f'''# Complete remote-branch audit

**All134 initial branches and12 additional historical branches are accounted for.** The latest complete snapshot contains **{rec['current_branches']} live branches and{rec['current_open_prs']} open PRs**. This audit verified {len(merges)} integrated PRs ({len(manual)} direct normal merges and{len(automatic)} automatically recognized integrations), closed {len(closures)} proven integrated source PRs without merging them, and deleted **zero branches**. Valuable blocked work remains intact.

Another session deleted73 root-repository branches. The user explicitly instructed: **“Keep the concurrent deletions; retain recovery evidence.”** All73 recorded heads are retained in a verified standalone Git bundle and are ancestors of main. No restoration is pending. The companion mobile repository also removed its source branch after its concurrent merge; that is recorded separately and is not an audit mutation.

## Complete report and evidence

- [Every initial branch: complete matrix](branch-matrix.md), [CSV](branches.csv), [detailed JSON](branches.json):134 unique rows, immutable initial heads, last observed/current heads, owners when available, targets, protection/operational references, ahead/behind/merge bases, checks/reviews/conflicts, dependencies, unique changes, classifications, rationale, actions and recovery commands.
- [All12 added historical branches](added-branches.md), [detailed JSON](added-branches.json), including the seven subsequently deleted by another session.
- [Executed validations and retained failures](validation.md), [per-branch blockers](blocked.md), [timestamped mutation ledger](mutations.jsonl), [count reconciliation](reconciliation.json), [final outcome verification](../final-outcome-verification.json), [final live recheck](../final-live-recheck.json), [matrix/link verification](../report-verification.json).
- [All73 concurrent deletion records and recovery commands](concurrent-deletions.md), [machine-readable evidence](../concurrent-deletions-recovery.json), [verified Git recovery bundle](../concurrent-deletions-recovery.bundle), [verification log](../concurrent-deletions-recovery.log), [user direction](../concurrent-deletions-user-direction.json).

The adjacent timestamped API snapshots, commit/patch comparisons and logs are the underlying evidence. No source checkout, dependency cache or database directory is included in the archive. A branch has no “closed” state; every closure reported here is a pull-request closure.

## Capability check and limitations

The capability check preceded audit mutations. Git/SSH repository access and GitHub authentication as `diegueins680` worked. [Repository permissions](../repository.json) provide read, push, triage, maintain and admin capabilities. Normal branch pushes, PR creation/update, evidence comments, review replies/resolutions, PR closures and normal merges were directly verified. Issue-write and ordinary branch-deletion permissions are supported by repository permissions; issue closure and branch deletion were not performed merely to exercise them. Administrator bypass was never used.

Main requires one approving review, dismissal of stale approvals and conversation resolution. Captured repository evidence contains no CODEOWNERS, merge queue or named required status contexts. Repository-prescribed checks were nevertheless required to pass. [Latest main protection](../{F.name}/main-protection.json) and [ruleset9478019](../ruleset-9478019.json) prohibit default-branch deletion and non-fast-forward updates. Every merge rechecked live head, target, reviews, threads, checks/status, rules and merge settings. Normal merge preserves the repository's migration-introduction ancestry; no native-stack rewrite, force push, protection change or weakened CI requirement was used.

Git/gh, Node/npm, Python3, Stack3.7.1 with prescribed GHC9.10.3, and native PostgreSQL16 were usable. The system GHC was not substituted. Local Docker later stopped responding; its daemon and unrelated containers were left untouched, and owned native PostgreSQL clusters ran the isolated database checks. Local Java/model-checker files were missing during the latest continuation; prescribed hosted TLA+/Alloy checks passed and no unavailable local rerun is claimed. Earlier dependency-cache/transport failures and corrected invocations remain in the validation table.

The attached browser profile is occupied by another session, so actual Google interactive-login and authenticated-upload acceptance remain unverified. The production SSH connection initially timed out, then recovered. The current helper now passes authenticated image/volume verification, the full777-record/652-table read-only catalog with post-query public checks, and the installed monitor's unchanged scheduled command with eight aggregate reports and zero errors. Its prior file was backed up and exact hashes checked; the LaunchAgent and monitor program were unchanged. [Current live proof](../cutover-provenance-live-verified.json) retains the initial failures separately. No credentials were printed, mail sent, application data written, service restarted or deployment manually triggered.

Earlier authorized operational repairs autonomously identified the existing Hetzner connection, verified the production image/database/volume, installed the read-only mail adapter and exercised the read-only catalog. A dedicated SELECT-only role, reviewed table grants and denied-write checks are recorded in [catalog evidence](../cutover-live-volume-verified.json) and [monitor evidence](../cutover-live-mail-volume-verified.json). The separate current live proof above validates the new helper; earlier results remain dated evidence. Normal automation triggered by commits and merges was allowed to run.

Earlier audit checkout Git metadata was missing at continuation. Existing evidence was recovered and hash-verified from the archive, and a new full-history clone and isolated worktrees were created without overwriting the old directories. [Workspace preservation](../workspace-preservation-20261004.json) confirms all66 original directories and the active audit directory remain; it explicitly distinguishes directory presence from pre-existing missing Git metadata.

## Timestamped inventory and reconciliation

Initial paginated snapshot: **2026-09-28T03:24:04Z —134 branches,67 open PRs,452 total PRs**. The reported77 branches/27 open PRs understated live state by **57 branches and40 open PRs**. [Initial branch pages](../branches-pages.json) and [all initial PR pages](../pulls-pages.json) include historical PR states, non-default targets and branches without PRs. GitHub does not expose a definitive branch creator; PR/commit authors are recorded without inventing ownership.

Latest complete snapshot: **{rec['timestamp_start'].strip()}–{rec['timestamp_end'].strip()} —{rec['current_branches']} branches,{rec['current_open_prs']} open PRs,{rec['current_total_prs']} total PRs**. Collection is paginated and timestamped, not atomic. All134 initial names remain in the matrix:68 live and66 concurrently absent. All12 added names are also retained:five live andseven concurrently absent. Zero names are unaccounted for.

Branch reconciliation: `134 initial +7 concurrent additions +5 audit additions -73 concurrent deletions =73 live`. PR reconciliation: `67 initial open +7 concurrent creations +5 audit creations -6 concurrent integrations -{len(merges)} audit integrations -{len(closures)} audit closures ={rec['current_open_prs']} open`. Total PR reconciliation: `452 +7 +5 =464`. Mobile115 is outside root totals. Individual DeleteEvents are available for32 missing refs; all73 absences were independently verified against paginated live state. Earlier continuation notes incorrectly carried the145 count forward; raw snapshots show72 after the23:38 cleanup, then73 afterPR476 creation.

## Verified outcome counts

| Result | Count and scope |
|---|---|
| Merged/integrated PRs | {len(merges)}: {', '.join(prlink(n) for n in sorted(merges))}; {len(manual)} direct merges, {len(automatic)} automatic recognitions |
| Repaired/consolidated implementation PRs published |8: root463,465,468,469,472,475,476 and companion mobile115 |
| Existing PR target repaired |1: root460 |
| Newly created / reopened PRs |6 /0: five root PRs plus mobile115 |
| Closed unmerged PRs |{len(closures)}: {', '.join(prlink(n) for n in sorted(closures))} |
| Branches deleted by this audit |0 |
| Concurrent root deletions retained per user direction |73, with verified recovery evidence |
| Protected/operational branches |{len(ops)} classified; {live_ops} currently live, {len(ops)-live_ops} concurrently absent with recovery evidence |
| Primary blocked/ambiguous classification |{counts['BLOCKED_OR_AMBIGUOUS']} |
| Linked issues closed / retained open |0 /2: issues128 and130 |

Companion mobile115 was independently reviewed and merged by another actor at `bca7f39cebcae24b62a4404bf5fd76bb9328f95a`; both post-merge workflows passed. The audit repaired and published its branch but did not perform that merge or deletion. [Verification](../mobile115-concurrent-merge-verified.json) distinguishes these actions.

## Classification matrix totals

| Classification | Initial134 | All146 historical names |
|---|---:|---:|
'''
for cls in sorted(counts):text+=f'| {cls} |{icounts[cls]} |{counts[cls]} |\n'
text+='''
Classification follows current architecture/tests/contracts/schemas/docs, explicit issue/PR criteria, review discussion, live callers/flags and newer implementations. Age, inactivity, naming, closed PR state or failing CI alone never establishes obsolescence. Exact ancestry, merged records and [squash/whole-range patch equivalence](../squash-proof.json) establish integration. They do not establish operational deletion safety. The user-directed retention of concurrent deletions is documented separately from this audit's safeguards.

## Dependency-ordered mutation history

1. Closed415→409→402→397→394→391→390 leaf-first after fresh exact-head ancestry evidence into merged424/main. Each received a classification, successor and recovery-SHA comment. No branch deletion was issued.
2. Retargeted460 to main without changing its head. GitHub rejected ordinary merging because of native-stack requirements. No history-rewriting endpoint was used; standalone469 subsequently preserved the source history.
3. Created companion mobile115 and root465's event consolidation, preserving35 event source histories plus the separate339 task-guard contribution. Repaired integrity, privacy/session, identity, analytics, browser, mobile, replay, generated clients and migration ancestry boundaries through normal commits.
4. Repaired463's canonical capacity and atomic publication completion, preserving source464 failure propagation with attribution. Published standalone469 on current main with all migration introductions retained. After exact-head approval and passing checks, normally merged469 atde76dc7d and verified its exact tested tree. GitHub automatically recognized460 as integrated.
5. Normal main CI then found published ip-address advisories. Focused472 updated only the compatible locked dependency to10.7.2; old/new vulnerability cases, controls and the unchanged security gate passed. Normally merged approved472 at76c2c05c. After all target CI passed, closed integrated463 with evidence. No security threshold or allowlist was weakened.
6. Repaired468 operational paths, messaging diagnostics, current-host read-only catalog/mail access, TLS/SSH address binding, named-volume binding and guarded webhook commands. Installed/live-verified earlier read-only adapters with exact evidence. Currentce1a71fe2 additionally validates configured PostgreSQL image provenance and repeats bound public checks after SQL;23 focused tests, strict1162-candidate catalog and inventory gates pass. Both new code-review threads were resolved after verified push. Current authenticated live metadata/catalog/mail checks pass and the installed helper is verified; manual browser acceptance remains blocked.
7. Normally reconciled mobile115 with current mobile main atede1f2a, preserving signing/runtime/API gates.87 suites/553 tests and12 Python signing checks passed. Another actor later merged it; root465's pinned ede1f2a is retained in mobile main ancestry.
8. Published replacement475 for source464, fixing ownership/private metadata, recurring reconciliation and locked writers. Added one immutable forward migration and retained all159 previous SQL files/entries. Current4100a04c passed3553 backend examples, compiled PostgreSQL/runtime/schema checks, the160-migration rehearsal and repository gates. After exact-head approval, normally merged475 atf8925e339 with exact tested tree. Closed464 only after all normal target CI passed.
9. Normally incorporated mainf8925 into465 at08acdfa, preserving event catalog decisions and current-main ownership/migration decisions. Repaired both lifecycle review findings at34ca75d: retain the enabled-feature share lock through commit and validate/bind receipt contents inside the transaction.110 native authenticated HTTP examples and six observed SQL lock-order/isolation cases passed; the old flag-race negative control failed as expected. Both threads were resolved after verified push.
10. Corrected the task-completion test's15-second expiry window versus approximately10-second polling budget at154f968aa, retaining every security assertion. Both native baseline and corrected suites passed on the slower local client; no reproduced hosted failure is falsely claimed. The corrected hosted test passed32 decision cases,12 isolation races and four expiry checks. Full154f968aa hosted backend passed3589 Hspec examples,1141 social HTTP examples,110 event HTTP examples and every isolated runtime/schema stage.
11. Created476 to rescue closed441's unique September18 delivery/recovery notes into the existing notification document, retaining exact source text and attribution with historical scope. Corrected only its generated provenance hash in a normal follow-up; no gitlink change or new device-qualification claim. After exact-head approval and passing checks, normally merged476 atc4d479c7 with exact tested tree. All normal target CI passed; source441's retained text and original mobile-pin ancestry are verified.
12. A465 merge guard stopped before mutation because GitHub still calculated against the earlier base after476. Normally reconciled current main into465 at ee58bd13; only the reviewed notification document/hash were added. Exact document, generated inventory and independently computed merge-tree checks passed. Current-head approval and all checks subsequently passed before the merge recorded below.
'''
if event:
 text+=f"\n13. Normally merged465 at `{event['pr']['merge_commit_sha']}` after fresh current-head approval, checks, threads, target, rules and settings verification. Verified the exact tested candidate tree, both parents, all35 source heads and160 migration introductions. Its branch remains. Normal target CI is {'verified passing' if event_ci.get('all_passed') else 'still pending; no source closure is justified by the merge alone'}."
 if event_closed or event_auto:text+=f" After target CI passed, closed{len(event_closed)} source PRs leaf-first with fresh ancestry/recovery comments and observed{len(event_auto)} automatic integrations. All35 source branches were rechecked and retained."
 text+='\n'
text+='''
The [append-only ledger](mutations.jsonl) supplies every mutation's timestamp, SHA, URL and verification. [Completed source closures](../events-source-closures-complete.json), [normal post-merge CI](../events-closure-main-ci.json), and the [final backend log](../events-main-backend-b07f67c3.log) retain the final integration proof. Rejected commands and stopped guards are never counted as completed merges. Concurrent PR creation/merges and branch deletions are separately attributed. No manual deployment, force push, shared-history rewrite or branch/rule/check bypass occurred.

## Current candidate state

| PR | Snapshot head | State | Review / unresolved threads | Check conclusions |
|---|---|---|---|---|
'''
for n in [465,468,475,476]:
 info=read(f'{F.name}/prs/{n}/info.json');ts=read(f'{F.name}/prs/{n}/threads.json');state=ts[0]['data']['repository']['pullRequest'];checks=read(f'{F.name}/checks/{info["head"]["sha"]}.json',[])
 conclusions=dict(collections.Counter(c['conclusion'] or c['status'] for pg in checks for c in pg['check_runs']));unresolved=sum(not t['isResolved'] for pg in ts for t in pg['data']['repository']['pullRequest']['reviewThreads']['nodes'])
 text+=f'| {prlink(n)} |`{info["head"]["sha"]}` |{info["state"]}; merged={info["merged"]} |{state["reviewDecision"]}; {unresolved} unresolved |{conclusions} |\n'
text+='''
## Remaining blockers and smallest next action

- **22 payment branches / root331:** obtain an authenticated merchant/environment-qualified terminal-resource or cancellation contract that excludes pending or late authorization/capture/hosted retry, with sandbox traces bound to checkout identity. Then repair/review the root and integrate dependents topologically. Current main intentionally defers provider expansion; transport errors do not prove no-charge finality. All unique work and PRs remain intact.
- **Operational468:** currentce1a71fe2 has independent approval and passing checks, but actual Google interactive login and authenticated upload remain unverified. Current authenticated metadata/catalog/mail validation and installed-helper verification pass. An authorized operator must record the actual login/upload acceptance. The occupied browser profile was not disturbed; no waiver or successful browser acceptance is claimed.
- **Issues128/130:** remain open.128 requires first-install/signup enrollment, three arms across web/mobile, RSVP plus fanclub broadcast, shared analytics and6–8weeks of evaluation.130 requires headliner resolution, deduplicated broadcast and preference/privacy behavior. Code integration alone satisfies neither issue. [Current issue states](../final-linked-issues.json) and [source issue bodies](../issues.json) are retained; no issue was closed.
'''
if not event:text+='- **Event465 and35 sources:** current ee58bd13 approval is verified; finish current CI, requery all merge gates, merge and verify target, then wait for normal post-merge CI before source closures. No source is closed solely to reduce the count.\n'
elif not event_ci.get('all_passed'):text+='- **Event465 post-merge CI:** integration is verified, but target checks must complete successfully before source closures.\n'
text+='''
The [per-branch blocker matrix](blocked.md) preserves exact branch-level decisions. Concurrent operational deletions require no restoration: the user explicitly directed keeping them, and their complete recovery evidence is retained.

## Validation and preservation

[Validation.md](validation.md) records the actual commands/logs for every repaired or merged implementation, including initial failures, negative controls, local/hosted distinctions, schema/migration checks, generated clients, security checks, UI/browser/mobile suites and live operational limitations. No test was deleted, skipped or weakened to obtain a passing result. The unchanged repository security allowlist still covers four pre-existing high transitive findings; a passing policy check is not a zero-vulnerability claim. The task-expiry polling correction allows the fixture's actual validity window to elapse while preserving every denial/locking/history assertion. Six Hspec pending cases were exercised by their separate prescribed runners; they are not reported as silently passed unit cases. Bounded model verification is not a universal implementation proof.

All66 original worktree directories and `/private/tmp/tdf-branch-audit-20260928` remain. [Current preservation/disk evidence](../workspace-preservation-20261004.json) reports the three pre-existing missing Git entries separately. Earlier authorized cleanup reclaimed regenerable uv/npm caches after active-use checks; the browser cache and existing worktrees were preserved. Disk availability fluctuates with concurrent builds and is not guaranteed to remain at its measured value. Unrelated workspace changes and production credentials were preserved.
'''
# Add spacing to prose only; keep SHA, URL, path and code text exact.
import re
prose_words='All|all|and|deleted|with|source|sources|root|PR|PRs|Issues|issues|issue|Operational|Event|main|through|approved|merged|retargeted|Retargeted|published|Published|closed|Closed|existing|retaining|retained|full|Stack|GHC|PostgreSQL|ruleset|reported|the|by|of|at|into|for|current|Current|initial|Initial|after|before|from|one|every|all|both'
parts=re.split(r'(`[^`]*`|\]\([^)]*\))',text)
for i in range(0,len(parts),2):
 parts[i]=re.sub(r'\b('+prose_words+r')(?=\d)',r'\1 ',parts[i])
 parts[i]=re.sub(r'(?<=\d)(?=(?:weeks|years|days|hours|months)\b)',' ',parts[i])
text=''.join(parts)
(R/'README.md').write_text(text)
assert 67+7+5-6-len(merges)-len(closures)==rec['current_open_prs'], 'Live PR reconciliation changed; refresh snapshot and attribution before publishing'
print('Wrote complete current overview',F.name)
