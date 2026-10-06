import collect,subprocess,json,datetime,time
P=collect.ROOT;cwd=P/'events';old='86d4b9bb4dad81771a2e3de4739814c62c206d30';base='49f1f0ec087067d17f62d91df1b616eb053eb894';branch='audit/event-stack-consolidation-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==old
assert api(collect.REPO+'/git/ref/heads/main')['object']['sha']==base
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()==old
assert subprocess.check_output(['git','rev-parse','MERGE_HEAD'],cwd=cwd,text=True).strip()==base
assert not subprocess.check_output(['git','diff','--name-only'],cwd=cwd,text=True).strip()
assert not subprocess.check_output(['git','diff','--name-only','--diff-filter=U'],cwd=cwd,text=True).strip()
for file,marker in [('events-current-main-release-tests.log','ℹ pass 83'),('events-current-main-ci-tests.log','ℹ pass 32'),('events-current-main-security.log','Passed npm security audit.'),('events-current-main-ui-targeted.log','Tests:       35 passed')]:assert marker in (P/file).read_text(),file
catalog=json.load(open(P/'events-current-main-catalog-2.json'));assert catalog['candidateCount']==1170 and all(c['decision']!='unreviewed' for c in catalog['candidates'])
subprocess.run(['git','diff','--cached','--check'],cwd=cwd,check=True)
subprocess.run(['git','commit','-m','Merge current main into event consolidation and reconcile companion contracts'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
for ancestor in [old,base]:subprocess.run(['git','merge-base','--is-ancestor',ancestor,sha],cwd=cwd,check=True)
# Re-query again immediately before the remote mutation.
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main';assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==old
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(collect.REPO+'/pulls/465')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha
(P/'events-current-main-after.json').write_text(json.dumps(x,indent=2)+'\n');ledger(action='pushed_event_current_main_conflict_repair',pr=465,old_sha=old,sha=sha,target=base,url=x['html_url'],validation='35 focused UI tests, 83 release tests, 32 CI contract tests, full repo-quality, zero-warning UI lint/typecheck, 1170 reviewed catalog entries, unchanged security gate, reproducible web/mobile clients. Full local UI/Stack work ongoing; local Docker unavailable; hosted full checks and mobile115 integration remain required.')
body=x['body']+'\n\nCurrent-main reconciliation `'+sha+'`: normally merged main49f1f0ec, preserving both event and interaction histories and the reviewed security fixes. Artist follow race/return-path handling coexists with actual paginated publication selection; all original assertions remain. Reconciled 1,170 reviewed catalog decisions and regenerated the specification inventory. Pin ede1f2a contains both previous mobile histories and produces exact clients from the merged schema.\n\nVerified so far: 35 focused UI tests, 83 release and 32 CI contract tests, repository quality, UI zero-warning lint/typecheck, catalog/security gates and web/mobile generated-client reproducibility. Full local UI and Stack validation are still running; no full-suite pass is claimed for this head yet. Local Docker is unresponsive, so current full-schema/HTTP validation remains required in hosted CI. Mobile115 remains pending independent review and integration. This new root head requires fresh independent review and passing current-head checks before merge. No deployments, branch deletions or shared-history rewrites.\n'
y=api(collect.REPO+'/pulls/465');assert y['state']=='open' and y['head']['sha']==sha and y['body']==x['body'];api(collect.REPO+'/pulls/465','--method','PATCH','-f','body='+body);y=api(collect.REPO+'/pulls/465');assert y['body']==body and y['head']['sha']==sha
(P/'events-current-main-description-after.json').write_text(json.dumps(y,indent=2)+'\n');ledger(action='updated_validation_description',pr=465,sha=sha,url=y['html_url']);print('Verified normal event repair push',sha)
