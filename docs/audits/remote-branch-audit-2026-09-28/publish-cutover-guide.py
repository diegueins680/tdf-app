import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='7f1d2d74877c5e603097e44cc643834e1932a69e';branch='codex/hetzner-cutover-evidence-20260928'
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
(P/'cutover-guide-before.json').write_text(json.dumps(x,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/468')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'cutover-guide-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_retired_deployment_guide_fix',pr=468,old_sha=old,sha=sha,url=x['html_url'],validation='Documentation-only change and generated fingerprint; specification inventory, three Python and nine Node gate tests pass; prior 122 helper tests unchanged. No production writes.')
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [
 ('PRRT_kwDOQPdUrM6m0Rxd','Retired DEPLOYMENT_GUIDE.md as a production procedure with an explicit top-level notice and links to the current Hetzner runbook and outstanding validation. Historical provider/secret/migration/rollback commands are explicitly non-operative, and retained Trader/shared Fly resources are protected. Both Cloudflare and Vercel frontend examples now use the canonical API. Regenerated the single guide fingerprint; inventory validation, three Python and nine Node gate regressions pass.')
]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2));body=f'Fixed in {sha}. '+msg+' All 122 helper/token/enrichment/import tests and the current specification inventory check pass.'
 r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=r['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 assert gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread']['isResolved']
 current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(current,indent=2));ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=r['url']);print('Verified resolved',tid,flush=True)
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
body='The web-first cutover left scheduled/manual tools and repair instructions pointing at the retired Fly backend. Retarget enrichment, bundled artist import, course publishing and payment/social callback guidance to the canonical API. Recognize canonical API assets as managed without admitting lookalike hosts. Retire the old deployment guide as a production procedure, link the current guarded runbook and correct both frontend API examples. Make messaging monitoring read-only while retaining invalid/expiry failure notifications, support credential aliases without printing values, and make readiness diagnostics report observed results rather than unperformed payment tests.\n\nPreserve the recorded restore, callback migration, backups, retained Trader/shared Fly resources and YouTube activation evidence. Cutover validation remains incomplete: actual Google login and authenticated upload gates are still unperformed. Production catalog inventory and the installed mail monitor also require an approved current read-only connection/secret source; those findings remain open. Automatic Hetzner credential rotation remains pending. No infrastructure credentials, provider callbacks, payments or deployment were changed by these repairs.\n\nValidation: final helper/token/enrichment/import suite passes 122 tests; CI/evidence/escrow tests pass 34 cases; specification regressions pass three cases. Course and callback commands are tested with mocked requests; shell syntax and regenerated specification hashes pass. The managed-host regression accepts canonical/legacy hosts and rejects lookalikes. The source commits and detailed results are recorded in the audit report. Thirteen concrete review findings are fixed; current-head hosted checks and independent review are required. Datadog still needs its external retired endpoint corrected without weakening assertions.\n'
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body;(P/'cutover-guide-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=y['html_url'])
print('Verified operator follow-up',sha)
