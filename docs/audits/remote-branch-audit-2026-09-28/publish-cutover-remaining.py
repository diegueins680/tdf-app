import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='e762f67b2720b3337c8409b6c3b652224f090083';branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(q):return api('graphql','-f','query='+q)['data']
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert 'ℹ pass 121' in (P/'cutover-remaining-path-tests.log').read_text() and 'ℹ fail 0' in (P/'cutover-remaining-path-tests.log').read_text()
subprocess.run(['python3','scripts/specification-inventory.py','--check'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-remaining-before.json').write_text(json.dumps(x,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/468')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'cutover-remaining-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_remaining_cutover_paths',pr=468,old_sha=old,sha=sha,url=x['html_url'],validation='121 helper/token/enrichment/import tests; 34 CI/evidence/escrow tests; 3 inventory regressions; shell syntax and inventory/diff checks pass. All network operations mocked; no provider or production writes.')
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [
 ('PRRT_kwDOQPdUrM6myA5L','The bundled importer now defaults to https://api.tdfrecords.net while preserving BASE_URL overrides, reviewed entity types and stable idempotency keys. The curl-stub regression verifies all 31 default requests, override routing, retry identity and fail-fast behavior. The login-helper production example is also corrected. No parties were created.'),
 ('PRRT_kwDOQPdUrM6myA5Q','Updated all identified active payment callback URLs and helper output to the canonical API. The guides point to the current guarded runtime procedure and preserve webhook IDs/signing secrets and existing provider/environment gates; obsolete deployment instructions are removed or explicitly marked historical. The helper no longer claims unperformed payment tests passed and fails on an unhealthy/unreachable health probe, covered by a mocked regression in the existing repo-quality suite. No payment, webhook or deployment was performed.'),
 ('PRRT_kwDOQPdUrM6myA5W','Both printed callback commands now retain FACEBOOK/META app credential fallbacks and INSTAGRAM_VERIFY_TOKEN/IG_VERIFY_TOKEN fallback without interpolating secret values. A local curl-stub regression verifies canonical and alias-only configurations expand the correct app URL, access token and verifier. Existing Facebook primary/alias/fallback and redaction tests still pass.')
]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2));body=f'Fixed in {sha}. '+msg+' All 121 helper/token/enrichment/import tests, 34 CI/evidence regressions, three inventory tests and the current inventory check pass.'
 r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=r['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 assert gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread']['isResolved']
 current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(current,indent=2));ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=r['url']);print('Verified resolved',tid,flush=True)
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
body=x['body']+'\nRemaining-helper follow-up `'+sha+'`: retargets the guarded bundled importer and payment callback instructions, honors diagnostic credential aliases, and removes unearned success claims from the read-only Stripe readiness helper. Its healthy/unhealthy/unreachable regression runs in the existing repo-quality suite. Validation: 121 helper/token/enrichment/import tests, 34 CI/evidence/escrow tests, three inventory tests, regenerated document hashes and shell syntax checks all pass. Eleven concrete review concerns are fixed; actual login/upload validation and independent current-head review/CI remain pending. No payment, provider write, runtime credential change or deployment occurred.\n' 
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body;(P/'cutover-remaining-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=y['html_url'])
print('Verified operator follow-up',sha)
