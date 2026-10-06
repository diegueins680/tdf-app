import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='7def35976f62dd153127d25217e22d3f0d19f3aa';branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert all(r['exit_code']==0 for r in json.load(open(P/'cutover-spec-tests.json')))
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert subprocess.check_output(['git','diff','--name-only',old,sha],cwd=cwd,text=True).strip()=='formal/system/inventory.json'
before=api(repo+'/pulls/468');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-inventory-before.json').write_text(json.dumps(before,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 after=api(repo+'/pulls/468')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['head']['sha']==sha
(P/'cutover-inventory-after.json').write_text(json.dumps(after,indent=2));ledger(action='pushed_generated_inventory_fix',pr=468,old_sha=old,sha=sha,url=after['html_url'],validation='Exact specification-contracts gate passed locally: inventory check, 3 Python regression tests, 9 Node evidence/escrow tests. One generated heading update; original hosted failure retained.')
before=api(repo+'/pulls/468');assert before['state']=='open' and before['head']['sha']==sha
body=before['body']+'\nGenerated-artifact follow-up `'+sha+'`: hosted specification discovery detected the renamed incomplete-validation heading; regenerated the single inventory heading and passed the exact specification check, three inventory regressions and nine evidence/escrow tests. No validation rule or CI requirement changed.\n'
title='fix: align Hetzner cutover maintenance and record pending validation'
api(repo+'/pulls/468','--method','PATCH','-f','body='+body,'-f','title='+title)
after=api(repo+'/pulls/468');assert after['body']==body and after['title']==title and after['head']['sha']==sha
(P/'cutover-inventory-description-after.json').write_text(json.dumps(after,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=after['html_url'])
print('Verified generated-artifact follow-up and final PR description',sha)
