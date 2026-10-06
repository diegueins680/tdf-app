import collect,subprocess,json,datetime
P=collect.ROOT;repo=collect.REPO;sha='cda2c5f7790126f35bd7e09bed35b52b5a0012ef'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
before=api(repo+'/pulls/469');record=json.load(open(P/'discovery-repair-pr.json'))
assert before['head']['sha']==sha and before['base']['ref']=='main' and before['state']=='open' and before['body']==record['body']
assert '3542 examples, 0 failures, 6 pending' in (P/'discovery-hosted-backend.log').read_text()
assert 'Automatic migrations passed against the fully cut-over production schema and were idempotent' in (P/'discovery-hosted-backend.log').read_text()
jobs=api(repo+'/actions/runs/36442970971/attempts/2/jobs?per_page=100')['jobs'];assert any(j['name']=='social-client' and j['conclusion']=='success' and j['head_sha']==sha for j in jobs)
old='The full integrated backend build, suite, production-schema rehearsal and current-head hosted CI remain pending, so this PR is draft. No deployment or feature activation is requested.'
assert old in before['body']
new='''Exact-head hosted validation now passes the backend build and full suite (3,542 examples, zero failures, six cases executed by the prescribed separate runners), 1,141 social HTTP examples, all runtime checks, shared-boundary regression and idempotent automatic production-schema rehearsal. The duplicate local compiler was stopped after verifying those hosted results; no local full-build pass is claimed. All implementation checks pass after one unchanged social-client test rerun; the initial preview timeout and a transient local ripple-contrast finding are retained as evidence, with no assertion or timeout changes.

The remaining failed check is the Datadog production API probe configured at https://tdf-hq.fly.dev/health, which timed out after the concurrent hosting cutover. The web probe passed. Its target needs to be corrected by the monitoring owner and the check rerun successfully; no monitor or failure setting was disabled. Result: https://app.datadoghq.com/synthetics/details/r2d-i82-3jy?resultId=1513866335113007515&batch_id=d415bd4d-b3ea-47eb-b7e7-6011cb118c52&from_ci=true

This remains draft pending the operational check and current-head independent review. No deployment or feature activation is requested. The original PR463 also retains an open review citing an unavailable release SHA39120; its actual published head and this replacement both preserve the introduction ancestor, and the comparison evidence was added without falsely resolving that thread.'''
body=before['body'].replace(old,new)
api(repo+'/pulls/469','--method','PATCH','-f','body='+body)
after=api(repo+'/pulls/469');assert after['body']==body and after['head']['sha']==sha and after['draft']
(P/'discovery-repair-pr.json').write_text(json.dumps(after,indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'updated_validation_description','pr':469,'sha':sha,'url':after['html_url']})+'\n')
print('Verified PR469 validation and exact remaining blocker description')
