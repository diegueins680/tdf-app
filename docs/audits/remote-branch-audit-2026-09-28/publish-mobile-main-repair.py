import collect,subprocess,json,datetime,time
P=collect.ROOT;cwd=P/'events/tdf-mobile';repo='repos/diegueins680/TDF-mobile';old='cb7426258e2aa87b1fcd52b275a85be83394132f';branch='audit/event-operations-client-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile',**kw})+'\n')
assert 'Tests:       553 passed, 553 total' in (P/'mobile-main-tests.log').read_text()
assert 'Ran 12 tests' in (P/'mobile-main-python.log').read_text() and '\nOK\n' in (P/'mobile-main-python.log').read_text()
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();main=subprocess.check_output(['git','rev-parse','origin/main'],cwd=cwd,text=True).strip()
assert api(repo+'/git/ref/heads/main')['object']['sha']==main
x=api(repo+'/pulls/115');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main';assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
(P/'mobile-main-before.json').write_text(json.dumps(x,indent=2));subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True);assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/115')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'mobile-main-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_mobile_current_main_conflict_repair',pr=115,old_sha=old,sha=sha,main_sha=main,url=x['html_url'],validation='87 suites/553 tests; release assets/lint/typecheck/public configuration; 12 Python signing/artifact tests. Retained stronger current native gates; all original event contracts preserved. No native build, publication or deployment.')
body='Generate the event-operations API contracts required by root PR465 and document canonical production API/upload configuration. The normal merge from current mobile main preserves its interaction client and stronger signing, runtime, association and API artifact gates; generated event additions remain additive. Both source histories are retained.\n\nValidation on the integrated head: 87 Jest suites / 553 tests pass; release assets, lint, typecheck and actual public Expo configuration pass; 12 Python signing/artifact tests pass. Only four files differ from current mobile main (generated types and canonical-host documentation/examples). No native binary was built or published; physical login/upload and rollout gates remain separate. Independent current-head review and hosted validation are required. Root PR465 must reconcile its companion pin after this branch is approved and integrated.\n'
assert api(repo+'/pulls/115')['head']['sha']==sha
api(repo+'/pulls/115','--method','PATCH','-f','title=Align event API contracts with current mobile release guards','-f','body='+body)
y=api(repo+'/pulls/115');assert y['body']==body and y['head']['sha']==sha;(P/'mobile-main-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=115,sha=sha,url=y['html_url']);print('Verified mobile repair',sha)
