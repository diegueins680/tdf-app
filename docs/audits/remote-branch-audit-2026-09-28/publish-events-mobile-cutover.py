import collect,subprocess,json,datetime,time
P=collect.ROOT;cwd=P/'events';old='afd86265f90378000cb3b5723f3406ced9793bbc';mobile='cb7426258e2aa87b1fcd52b275a85be83394132f';branch='audit/event-stack-consolidation-20260928';repo=collect.REPO
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert (P/'events-mobile-cutover-catalog.json').is_file()
assert all(x['exit_code']==0 for x in json.load(open(P/'mobile-cutover-validation.json')))
assert len(json.load(open(P/'mobile-cutover-workflow-config.json')))==2
subprocess.run(['python3','scripts/specification-inventory.py','--check'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert subprocess.check_output(['git','diff','--name-only',old,sha],cwd=cwd,text=True).strip()=='tdf-mobile'
assert subprocess.check_output(['git','rev-parse','HEAD:tdf-mobile'],cwd=cwd,text=True).strip()==mobile
m=api('repos/diegueins680/TDF-mobile/pulls/115');assert m['state']=='open' and m['head']['sha']==mobile
before=api(repo+'/pulls/465');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'events-mobile-cutover-before.json').write_text(json.dumps(before,indent=2));subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True);assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 after=api(repo+'/pulls/465')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['head']['sha']==sha;(P/'events-mobile-cutover-after.json').write_text(json.dumps(after,indent=2));ledger(action='pushed_current_mobile_pin',pr=465,old_sha=old,sha=sha,mobile_sha=mobile,url=after['html_url'],validation='Only gitlink changed. Mobile 85 suites/510 tests, release checks, 7 Python tests, 5 profile/override configurations and 2 workflow input configurations pass; root catalog --fail-on-unreviewed and specification inventory pass. Earlier root approval/checks must not be treated as approval of this new head.')
before=api(repo+'/pulls/465');assert before['state']=='open' and before['head']['sha']==sha
body=before['body']+'\n\nCanonical mobile API follow-up `'+sha+'`: pins mobile115 at `'+mobile+'`, including the regenerated event contracts plus current production/preview API/upload endpoints and Android/iOS workflow inputs. Only the root gitlink changed; web/backend source is identical to the previously tested root head. Mobile application tests pass (85 suites / 510 tests), release assets/lint/typecheck and seven Python release tests pass, and actual Expo resolution passes five profile/override and two workflow-injection cases. Root catalog and specification gates pass. No native build, submission or deployment occurred. Current-head CI and renewed independent review on this root PR, plus mobile115 review/integration, are required; prior approval and checks are historical.\n'
api(repo+'/pulls/465','--method','PATCH','-f','body='+body);after=api(repo+'/pulls/465');assert after['head']['sha']==sha and after['body']==body;(P/'events-mobile-cutover-description-after.json').write_text(json.dumps(after,indent=2));ledger(action='updated_validation_description',pr=465,sha=sha,url=after['html_url']);print('Verified root mobile pin',sha)
