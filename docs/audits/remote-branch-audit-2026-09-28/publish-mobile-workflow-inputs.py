import collect,subprocess,json,time,datetime
P=collect.ROOT;cwd=P/'events/tdf-mobile';repo='repos/diegueins680/TDF-mobile';old='c921f5d7d0dc7b0119588b962587ed1d6885163f';branch='audit/event-operations-client-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile',**kw})+'\n')
assert len(json.load(open(P/'mobile-cutover-workflow-config.json')))==2
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert set(subprocess.check_output(['git','diff','--name-only',old,sha],cwd=cwd,text=True).splitlines())=={'.github/workflows/android-release-build.yml','.github/workflows/ios-release-build.yml'}
before=api(repo+'/pulls/115');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='main';assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'mobile-workflow-inputs-before.json').write_text(json.dumps(before,indent=2));subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True);assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 after=api(repo+'/pulls/115')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['head']['sha']==sha;(P/'mobile-workflow-inputs-after.json').write_text(json.dumps(after,indent=2));ledger(action='pushed_mobile_native_workflow_inputs',pr=115,old_sha=old,sha=sha,url=after['html_url'],validation='Only four API/upload environment values changed across the Android/iOS native build workflows. Actual Expo configuration with both workflow inputs resolves canonical endpoints; all other environment values preserved. No workflow was dispatched. Application files are identical to c921f5d, whose full suite/release checks passed.')
before=api(repo+'/pulls/115');assert before['state']=='open' and before['head']['sha']==sha
body=before['body']+'\nNative-workflow input follow-up `'+sha+'`: updates only the four API/upload values injected by Android/iOS build workflows. Both actual Expo configuration checks pass and the other environment values are preserved. Application files and artifact-check logic are unchanged from tested c921f5d. No native workflow was dispatched; current-head hosted validation and independent review remain required.\n'
api(repo+'/pulls/115','--method','PATCH','-f','body='+body);after=api(repo+'/pulls/115');assert after['head']['sha']==sha and after['body']==body;(P/'mobile-workflow-inputs-description-after.json').write_text(json.dumps(after,indent=2));ledger(action='updated_validation_description',pr=115,sha=sha,url=after['html_url']);print('Verified mobile workflow inputs',sha)
