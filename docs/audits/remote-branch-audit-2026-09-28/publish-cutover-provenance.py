import collect,subprocess,json,datetime,time
P=collect.ROOT;W=P/'cutover-provenance-followup-20261003';old='762e1ff44c6b2e86fc21630bced3b39d242b1fbb'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+path,*args],env=collect.ENV,text=True))
def run(*args):return subprocess.check_output(list(args),cwd=W,env=collect.ENV,text=True).strip()
# Execute only after the complete strict catalog gate has finished successfully.
assert (P/'cutover-provenance-local-validation.json').exists()
assert json.loads((P/'cutover-provenance-local-validation.json').read_text())['catalog_gate']=='PASS'
assert run('git','rev-parse','HEAD')==old
pr=api('/pulls/468');assert pr['state']=='open' and pr['head']['sha']==old
assert api('/git/ref/heads/'+pr['head']['ref'])['object']['sha']==old
(P/'cutover-provenance-before-commit.json').write_text(json.dumps(pr,indent=2))
expected={'ops/hetzner/README.md','scripts/production_access.py','scripts/test_production_access.py','scripts/production-catalog-inventory.mjs','scripts/__tests__/production-catalog-inventory.test.mjs'}
assert set(run('git','diff','--name-only').splitlines())==expected
run('git','diff','--check');run('git','add',*sorted(expected));print(run('git','commit','-m','fix(ops): verify database image and post-query public origin'),flush=True)
head=run('git','rev-parse','HEAD');again=api('/pulls/468');assert again['head']['sha']==old and again['state']=='open'
assert api('/git/ref/heads/'+pr['head']['ref'])['object']['sha']==old
run('git','push','origin','HEAD:refs/heads/'+pr['head']['ref'])
assert api('/git/ref/heads/'+pr['head']['ref'])['object']['sha']==head
for _ in range(10):
 after=api('/pulls/468')
 if after['head']['sha']==head:break
 time.sleep(1)
assert after['head']['sha']==head
(P/'cutover-provenance-push-verified.json').write_text(json.dumps(after,indent=2))
with (P/'mutations.jsonl').open('a') as out:out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'pushed_repair_followup','pr':468,'old_sha':old,'sha':head,'url':after['html_url'],'evidence':'cutover-provenance-push-verified.json'})+'\n')
print('VERIFIED',head)
