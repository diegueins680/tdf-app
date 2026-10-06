import collect,subprocess,json,datetime
p=collect.ROOT;repo=collect.REPO;cwd=p/'events';branch='audit/event-stack-consolidation-20260928'
def api(path,*args,allow404=False):
 r=subprocess.run(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True,capture_output=True)
 if r.returncode:
  if allow404 and 'HTTP 404' in r.stderr:return None
  raise RuntimeError(r.stderr)
 return json.loads(r.stdout)
assert api(repo+'/branches/main')['commit']['sha']=='cc244b1f86603055997b51379b297baebfd3e7ce'
assert api(repo+'/git/ref/heads/'+branch,allow404=True) is None
# Preserve source tips and verify no concurrent source work will be represented as consolidated.
sources=json.load(open(p/'git-inventory-enriched.json'));chain=set(json.load(open(p/'event-chain.json')))
for b in sources:
 if b['name'] in chain:
  live=api(repo+'/git/ref/heads/'+b['name']);assert live['object']['sha']==b['sha'],b['name']+' changed'
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'pushed_repair_branch','branch':branch,'sha':sha})+'\n')
body='''The existing event delivery chain conflicts with current main and its older task adapter would overwrite stronger deletion, dependency-approval and RACI guards. This candidate consolidates the source history through #417 with current main, restores those guards, and preserves the additional #339 successor fixes that were absent from the chain tip.

The integrated behavior provides scoped event/task reads, revision checks, RACI reassignment and task completion with authenticated-session fencing. It preserves main’s current identity fields, lazy analytics, replay safeguards, all-change formal verification and exact production migration manifest. Event adapter migrations remain disabled/unregistered. Normal merges preserve source authors and migration ancestry; no source PR is superseded or branch deleted until an approved replacement actually merges.

Mobile generated-client companion: https://github.com/diegueins680/TDF-mobile/pull/115 (pinned commit 2ec145f17d76938ef7a8c042c949e2ad64222c41). Canonical OpenAPI regenerated for both clients. Four new catalog projections have individual reviews; historical retirement metadata is preserved with explicit current-successor mappings.

Validation completed locally: event formal suite and negative controls; repository quality gate; UI typecheck/lint and 338 focused tests; 54 event-tooling tests; five catalog retirement tests and catalog gate; regenerated specification checks; foundation/API/task-commit/task-read/task-revision/RACI-reassignment/task-completion database suites; complete-schema rehearsal. Task-completion covers 32 decisions, all isolation races and expiry/revocation checks. Mobile typecheck/lint pass with locked dependencies.

Pending: full backend build/test and HTTP/browser harnesses; remaining database suites; complete UI/mobile validation and hosted exact-head checks. The full UI run has failures including timeouts, still under investigation; this draft is not merge-ready. Mobile dependency audit has 34 existing findings (six high), with no dependency changes. Required independent approval, resolved blocking discussions, and passing applicable checks remain mandatory; no bypass or deployment requested.

Compatibility and repair details: docs/event-operations/integration-2026-09-28.md. Original chain includes #337 #338 #339 #341 #342 #345 #346 #348 #349 #351 #352 #354 #357 #359 #364 #368 #372 #373 #375 #379 #381 #383 #384 #387 #388 #395 #398 #399 #403 #407 #410 #411 #413 #416 #417. Those originals remain intact pending verified consolidation.
'''
(p/'events-pr-body.md').write_text(body)
pr=api(repo+'/pulls','--method','POST','-f','title=Consolidate event operations against current integrity and authentication guards','-f','head='+branch,'-f','base=main','-f','body='+body,'-F','draft=true')
a=api(repo+'/pulls/'+str(pr['number']));assert a['head']['sha']==sha and a['draft'] and a['state']=='open'
(p/'events-repair-pr.json').write_text(json.dumps(a,indent=2))
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'created_draft_pr','pr':a['number'],'sha':sha,'url':a['html_url']})+'\n')
print(a['html_url'],sha)
