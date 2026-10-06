import collect,subprocess,json,datetime,time
P=collect.ROOT; repo=collect.REPO; cwd=P/'cutover'; old='9c29fd5bd19d256a60e8a877552a854fee881be6'; branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
assert 'ℹ pass 111' in (P/'cutover-targeted-tests.log').read_text() and 'ℹ fail 0' in (P/'cutover-targeted-tests.log').read_text()
assert 'ℹ pass 25' in (P/'cutover-ci-tests.log').read_text() and 'ℹ fail 0' in (P/'cutover-ci-tests.log').read_text()
assert all(r['exit_code']==0 for r in json.loads((P/'cutover-course-checks.json').read_text()))
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
before=api(repo+'/pulls/468'); assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-followup-before.json').write_text(json.dumps(before,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for attempt in range(10):
 after=api(repo+'/pulls/468')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['state']=='open' and after['head']['sha']==sha and after['base']['ref']=='main'
(P/'cutover-followup-after.json').write_text(json.dumps(after,indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'pushed_cutover_maintenance_fix','pr':468,'old_sha':old,'sha':sha,'url':after['html_url'],'validation':'111 messaging/enrichment tests; 25 CI contract tests; three mocked course requests; YAML parse and git diff --check passed. Retargets current API, prevents retired Fly token writes, preserves expiry failure notifications. Login/upload remain unperformed and explicitly incomplete. No deployment.'})+'\n')
print('Verified PR468 normal push',sha)
