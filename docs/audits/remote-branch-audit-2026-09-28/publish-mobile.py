import collect,subprocess,json,datetime
p=collect.ROOT;repo='repos/diegueins680/TDF-mobile';cwd=p/'events/tdf-mobile';branch='audit/event-operations-client-20260928'
def api(path,*args,allow404=False):
 r=subprocess.run(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True,capture_output=True)
 if r.returncode:
  if allow404 and 'HTTP 404' in r.stderr:return None
  raise RuntimeError(r.stderr)
 return json.loads(r.stdout)
rules=api(repo+'/rulesets/9414797');(p/'mobile-ruleset-9414797.json').write_text(json.dumps(rules,indent=2))
main=api(repo+'/branches/main');assert main['commit']['sha']=='2a0e5a99535d9ef199a3e3464a660192f882f72b'
assert api(repo+'/git/ref/heads/'+branch,allow404=True) is None
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
ref=api(repo+'/git/ref/heads/'+branch);assert ref['object']['sha']==sha
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile','action':'pushed_repair_branch','branch':branch,'sha':sha})+'\n')
body='''Regenerate the mobile TypeScript client from the canonical tdf-app OpenAPI contract used by the event-stack consolidation (source PR337 through PR417). This adds the scoped task read, revision, RACI and completion projections and reconciles existing contract drift without changing runtime mobile flows or dependency manifests.

Companion source: tdf-app branch `audit/event-stack-consolidation-20260928`. The parent integration will pin this commit. Existing main mobile commit is preserved as the parent; no history rewrite.

Validation: isolated locked dependencies installed; local typecheck and lint passed. Full mobile tests and release checks are running; hosted checks and independent review are pending. Dependency audit reports 34 pre-existing findings (6 high); the dependency manifests are unchanged. This is a draft, not evidence of merge readiness. No activation, build publication or deployment is requested.
'''
pr=api(repo+'/pulls','--method','POST','-f','title=Regenerate mobile contracts for scoped event operations','-f','head='+branch,'-f','base=main','-f','body='+body,'-F','draft=true')
verified=api(repo+'/pulls/'+str(pr['number']));assert verified['head']['sha']==sha and verified['draft'] and verified['state']=='open'
(p/'mobile-repair-pr.json').write_text(json.dumps(verified,indent=2))
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile','action':'created_draft_pr','pr':verified['number'],'sha':sha,'url':verified['html_url']})+'\n')
print(verified['html_url'],sha)
