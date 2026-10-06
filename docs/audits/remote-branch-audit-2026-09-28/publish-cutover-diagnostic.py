import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'cutover';old='5486b49144f9b0d5576cef5f2e59d927c66e334f';branch='codex/hetzner-cutover-evidence-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(q):return api('graphql','-f','query='+q)['data']
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
assert 'ℹ pass 115' in (P/'cutover-diagnostic-tests.log').read_text() and 'ℹ fail 0' in (P/'cutover-diagnostic-tests.log').read_text()
subprocess.run(['python3','scripts/specification-inventory.py','--check'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True).strip()
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'cutover-diagnostic-before.json').write_text(json.dumps(x,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(repo+'/pulls/468')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha;(P/'cutover-diagnostic-after.json').write_text(json.dumps(x,indent=2));ledger(action='pushed_cutover_diagnostic_fixes',pr=468,old_sha=old,sha=sha,url=x['html_url'],validation='115 tests including stale/inactive/current callback states and backend-supported Facebook token expansion; inventory check and diff check pass. No provider/network writes or deployment.')
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [
 ('PRRT_kwDOQPdUrM6mxj_I','The diagnostic now requires active=true and the exact current callback URL for each subscription. Missing, inactive or stale callbacks trigger the corresponding repair command and summary issue. Regression cases exercise both channels with stale URLs, inactive current URLs and active current URLs, without provider calls.'),
 ('PRRT_kwDOQPdUrM6mxj_P','The Facebook repair command now expands the configured FACEBOOK_MESSAGING_TOKEN, its supported FACEBOOK_PAGE_ACCESS_TOKEN alias, or INSTAGRAM_VERIFY_TOKEN fallback. These match the existing Config/ServerExtra verification contract. The command prints environment references rather than secret values. A local curl-stub regression executes all three expansions and verifies the accepted token is supplied without network writes.')
]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2));body=f'Fixed in {sha}. '+msg+' All 115 messaging/enrichment regressions and the unchanged specification inventory check pass.'
 r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=r['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 assert gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread']['isResolved']
 current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(current,indent=2));ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=r['url']);print('Verified resolved',tid,flush=True)
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
body=x['body']+'\nWebhook-health follow-up `'+sha+'`: existing subscriptions must be active at the canonical callback before the diagnostic reports them healthy. Facebook repair uses the configured backend-supported token/alias/fallback without exposing values. All 115 messaging/enrichment tests pass, including both callback-state checks and all three token expansions through a local curl stub; the specification inventory remains current. Eight concrete review concerns are fixed; actual login/upload validation and current-head independent review/CI remain pending. No provider writes or deployment.\n' 
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body;(P/'cutover-diagnostic-description-after.json').write_text(json.dumps(y,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=y['html_url'])
print('Verified operator follow-up',sha)
