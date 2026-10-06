import collect,subprocess,json,datetime
P=collect.ROOT
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
for repository,n,filename,sha,body in [
 ('tdf-app',465,'events-repair-pr.json','afd86265f90378000cb3b5723f3406ced9793bbc','''The event delivery chain conflicted with current main, and its older task adapter overwrote stronger deletion, dependency-approval and RACI guards. This PR consolidates the source history through #417, includes the additional #339 successor omitted from that chain, and preserves current integrity and authentication behavior.

Scoped task reads, revision checks, RACI reassignment and completion retain authenticated-session revocation fencing. Normal merge commits preserve source authors and migration ancestry. Main's current invitation/logistics handlers, identity fields, lazy analytics, replay safeguards, all-change formal checks and exact production migration manifest are retained. Event adapter migrations remain disabled and unregistered. No source PR is superseded before this replacement actually merges.

The repairs retain foundation guards during adapter installation and rollback; add stale-graph/deletion negative controls; make race fixtures obey current mandatory RACI constraints; exercise real database-clock expiry across HTTP; preserve unrelated artist-follow URL state; and reconcile reviewed catalog history and specification inventory. Canonical OpenAPI projections are regenerated for both clients. Mobile companion: https://github.com/diegueins680/TDF-mobile/pull/115, pinned at 2ec145f17d76938ef7a8c042c949e2ad64222c41, awaiting independent review.

Validation executed locally:
- Stack build and full suite: 3,573 examples, zero failures, six cases delegated to the prescribed separate runtime runners; all eight isolated runtime runners pass.
- Production-auth HTTP: 107 examples, zero failures. Real RACI browser/API/PostgreSQL: all eight desktop/phone cases pass, including replay, concurrency, revocation and least privilege.
- Event foundation/API/task commit/read/revision/revisioned-read/RACI/editor-context/completion SQL suites and complete-schema rehearsal pass. Completion includes 32 decisions, isolation/expiry/revocation races and recovery controls.
- Formal TLA+/Alloy models, liveness/negative controls, repository quality, catalog gate and five retirement tests, specification inventory and three generator tests pass.
- UI typecheck/lint, 338 focused tests and the production build/bundle guard pass. Full local UI run initially had 29 failures across seven suites during heavy builds; those same suites passed unchanged on rerun (179 tests), and hosted ui-quality passed on the previous published head. Latest artist-follow component/intent regressions: 40 tests pass. Targeted persona tests: 19 pass across configured browsers with eight pre-existing project-specific skips; no skips/timeouts were added.
- Mobile companion: 510 tests in 85 suites, typecheck, lint, release check and hosted validation pass. Its unchanged dependencies have 34 existing audit findings, six high; no threshold or security control was weakened.

Current-head hosted checks are running after the final HTTP/browser repairs. Keep this draft until complete; independent approval, resolved blocking discussions and applicable checks remain required. No deployment or feature activation is requested. Source details and compatibility decisions: docs/event-operations/integration-2026-09-28.md.

Original PRs retained pending successful consolidation: #337 #338 #339 #341 #342 #345 #346 #348 #349 #351 #352 #354 #357 #359 #364 #368 #372 #373 #375 #379 #381 #383 #384 #387 #388 #395 #398 #399 #403 #407 #410 #411 #413 #416 #417.
'''),
 ('TDF-mobile',115,'mobile-repair-pr.json','2ec145f17d76938ef7a8c042c949e2ad64222c41','''Regenerate the mobile TypeScript client from the canonical OpenAPI contract in https://github.com/diegueins680/tdf-app/pull/465. This adds scoped task reads, revision checks, RACI and completion projections and reconciles existing contract drift. Runtime mobile flows and dependency manifests are unchanged.

The parent integration pins this commit. Existing main commit 2a0e5a99535d9ef199a3e3464a660192f882f72b is retained as its parent; no history rewrite.

Verified validation: isolated locked npm install; typecheck and lint; all 510 tests across 85 suites; release:check; exact-head hosted validate and synthetic checks. The unchanged dependency manifests have 34 existing audit findings (six high), with no audit-fix or threshold bypass. Independent review remains pending. This PR is ready for review and unmerged; no build publication, feature activation or deployment is requested.
''')]:
 path=f'repos/diegueins680/{repository}/pulls/{n}'
 before=api(path);record=json.load(open(P/filename))
 assert before['state']=='open' and before['head']['sha']==sha and before['base']['ref']=='main'
 assert before['body']==record['body'],'Concurrent body edit; preserve it'
 (P/f'{repository}-{n}-body-before.json').write_text(json.dumps(before,indent=2))
 api(path,'--method','PATCH','-f','body='+body)
 after=api(path);assert after['body']==body and after['head']['sha']==sha
 (P/filename).write_text(json.dumps(after,indent=2))
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':repository,'action':'updated_validation_description','pr':n,'sha':sha,'url':after['html_url']})+'\n')
 print('Verified validation description',repository,n)
