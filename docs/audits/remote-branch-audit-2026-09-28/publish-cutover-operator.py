import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='68558f5409f9a976c0a4a9bd742e627f75f089cf';branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(q):return api('graphql','-f','query='+q)['data']
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert 'ℹ pass 113' in (P/'cutover-operator-tests-2.log').read_text() and 'ℹ fail 0' in (P/'cutover-operator-tests-2.log').read_text()
assert all(x['exit_code']==0 for x in json.load(open(P/'cutover-operator-spec.json')))
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-operator-before.json').write_text(json.dumps(x,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/468')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'cutover-operator-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_cutover_operator_fixes',pr=468,old_sha=old,sha=sha,url=x['html_url'],validation='113 tests including manual default/override and diagnostic callback/secret-output regression; specification inventory plus 3 Python and 9 Node regressions; two generated document hashes. No production calls or deployment.')
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [
 ('PRRT_kwDOQPdUrM6mxHze','Manual enrichment now defaults to https://api.tdfrecords.net and retains explicit TDF_API_BASE/API_BASE overrides. The runbook documents these and points backup/recovery operations to the current guarded Hetzner procedure, preserving post-cutover writes. A mocked production-API boundary regression verifies all three routing cases without network writes.'),
 ('PRRT_kwDOQPdUrM6mxHzk','Updated the primary messaging guide: scheduled/manual checks are read-only, action=refresh is rejected, and legacy Fly maintenance is explicitly retired. The guide states automated Hetzner rotation is not implemented and gives the authorized operator sequence: validated replacement, secure current runtime persistence under existing deployment controls, live verification, then GitHub check-secret synchronization. Failures remain visible. No credential rotation, deployment or completion claim was made.'),
 ('PRRT_kwDOQPdUrM6mxHzr','Both missing-subscription repair commands now target the canonical API callbacks. They print environment placeholders instead of supplied secret values; the refresh guidance uses the current secret-store procedure and no longer recommends Fly updates/restarts. The mocked diagnostic regression verifies both URLs and absence of supplied credentials/retired repair commands. No provider subscription was changed.')]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2));body=f'Fixed in {sha}. '+msg+' All 113 messaging/enrichment tests and the exact specification gate (3 inventory and 9 evidence/escrow regressions) pass.'
 r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=r['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 assert gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread']['isResolved']
 current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(current,indent=2));ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=r['url']);print('Verified resolved',tid,flush=True)
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
body=x['body']+'\nOperator-path follow-up `'+sha+'`: retargets manual enrichment with tested override compatibility, updates the current backup/recovery and messaging rotation guides, and corrects both webhook repair URLs without printing supplied credentials. All 113 messaging/enrichment tests pass, as does the exact specification gate (3 Python and 9 Node regressions). Six concrete review concerns are now fixed; the unperformed login/upload gate remains open. No provider writes, credential rotation, deployment, or CI weakening.\n'
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body;(P/'cutover-operator-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=y['html_url'])
print('Verified operator follow-up',sha)
