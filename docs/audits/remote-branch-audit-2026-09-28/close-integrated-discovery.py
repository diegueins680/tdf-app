import collect,subprocess,json,datetime
P=collect.ROOT;repo=collect.REPO;main='76c2c05c536586fd814f7a68d2052f0fd9d900ed'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
def verified_target():
 assert api(repo+'/git/ref/heads/main')['object']['sha']==main
 runs=api(repo+'/actions/runs?head_sha='+main+'&per_page=100');assert runs['total_count']==len(runs['workflow_runs']) and all(r['status']=='completed' and r['conclusion'] in ('success','skipped') for r in runs['workflow_runs']),[(r['name'],r['status'],r['conclusion']) for r in runs['workflow_runs']]
 checks=api(repo+'/commits/'+main+'/check-runs?per_page=100');assert checks['total_count']==len(checks['check_runs']) and checks['total_count']>0 and all(c['status']=='completed' and c['conclusion'] in ('success','skipped') for c in checks['check_runs'])
 assert api(repo+'/commits/'+main+'/status')['state']=='success'
 for n in [469,472]:assert api(repo+'/pulls/'+str(n))['merged']
 (P/'discovery-closure-main-ci.json').write_text(json.dumps({'main':main,'runs':runs,'checks':checks},indent=2))
for n,sha in [(463,'a125a347cacb8d941a515171e2a81d4c92f2c2d2'),(460,'648f2d6dc69080d4438ae2a25309497012ded2d1')]:
 verified_target();x=api(repo+'/pulls/'+str(n));assert x['state']=='open' and x['head']['sha']==sha
 assert api(repo+'/git/ref/heads/'+x['head']['ref'])['object']['sha']==sha
 proof=api(repo+'/compare/'+sha+'...'+main);assert proof['behind_by']==0 and proof['merge_base_commit']['sha']==sha
 (P/f'close-{n}-integrated-proof.json').write_text(json.dumps({'pr':x,'compare':proof},indent=2))
 body=f'Classification: ALREADY_MERGED. Exact head `{sha}` is an ancestor of current main `{main}`. Its original history and repaired discovery behavior were integrated through approved, normally merged #469 (`de76dc7df247937144e1802c17d1842f983cbe13`); the resulting tree matched the tested candidate. The subsequent security follow-up #472 is merged and current main checks/workflows pass. Focused discovery, real PostgreSQL boundary/completion/rollback, full hosted backend/runtime/schema and migration-ancestry evidence are retained in the branch audit. Useful failure propagation from #464 was rescued with attribution; that concurrent editorial PR and its unique work remain intact. Closing only this integrated PR; its branch is retained, including for dependent work. Recovery head: `{sha}`.'
 comment=api(repo+'/issues/'+str(n)+'/comments','--method','POST','-f','body='+body);assert comment['body']==body;ledger(action='commented_integrated_closure_evidence',pr=n,sha=sha,url=comment['html_url'])
 verified_target();fresh=api(repo+'/pulls/'+str(n));assert fresh['state']=='open' and fresh['head']['sha']==sha and fresh['base']['ref']==x['base']['ref']
 api(repo+'/pulls/'+str(n),'--method','PATCH','-f','state=closed')
 y=api(repo+'/pulls/'+str(n));assert y['state']=='closed' and not y['merged'] and y['head']['sha']==sha
 assert api(repo+'/git/ref/heads/'+x['head']['ref'])['object']['sha']==sha
 (P/f'close-{n}-integrated-after.json').write_text(json.dumps(y,indent=2));ledger(action='closed_unmerged',pr=n,sha=sha,url=y['html_url'],comment=comment['html_url'],replacement=469,branch_retained=True)
 print('Verified closed integrated PR',n,flush=True)
