import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='9a8357065fb45d4565e1bd4a764d5f9fe739a40d';branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(q):return api('graphql','-f','query='+q)['data']
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert 'ℹ pass 122' in (P/'cutover-managed-image-tests.log').read_text() and 'ℹ fail 0' in (P/'cutover-managed-image-tests.log').read_text()
subprocess.run(['python3','scripts/specification-inventory.py','--check'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-origin-before.json').write_text(json.dumps(x,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/468')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'cutover-origin-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_verified_public_origin_binding',pr=468,old_sha=old,sha=sha,url=x['html_url'],validation='9 access, 6 mail and 5 catalog tests pass; specification gate passes. Live inventory passes with 777 records/652 tables and actual TLS peer matching authenticated SSH server. Two initial coverage failures preserved; 29 explicitly reviewed SELECT grants restored coverage, zero write privileges.')
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [
 ('PRRT_kwDOQPdUrM6nfSiR','Bound both public health/version requests to the server address reported by the authenticated SSH connection: every public DNS answer and the actual HTTPS socket peer must match, while normal TLS certificate/hostname verification remains enabled and redirects are rejected. A healthy same-SHA deployment on a different host and mixed DNS answers fail before querying. A future CDN/load balancer requires an explicitly reviewed origin binding. Five catalog, nine access and six mail regressions pass; live current-host inventory passes with 777 records/652 tables. The reader remains SELECT-only; 29 new tables from the concurrent merged release were traced to their migrations and explicitly granted SELECT after fail-closed coverage rejection. No application data, mail or deployment changed.')
]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2));body=f'Fixed in {sha}. '+msg+' The actual authentication/upload acceptance thread remains open.'
 r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=r['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 assert gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread']['isResolved']
 current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(current,indent=2));ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=r['url']);print('Verified resolved',tid,flush=True)
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
body=x['body'].replace('Twenty-two concrete review findings', 'Twenty-three concrete review findings')+'\n\nOrigin-binding follow-up `'+sha+'`: public HTTPS DNS answers and actual socket peer must match the SSH-authenticated host, with normal TLS verification; same-SHA foreign deployments fail closed. Five catalog, nine access and six mail regressions and the specification gate pass. Actual production inventory returns 777 records across 652 tables. Initial coverage rejection after a concurrent release was resolved by explicitly reviewing and granting SELECT on 29 added tables; reader write/create privileges remain absent. No application-data changes or manual deployment. Google login/authenticated upload and current-head independent review remain required.\n'
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body;(P/'cutover-origin-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=y['html_url'])
print('Verified operator follow-up',sha)
