import collect,json,subprocess,datetime
P=collect.ROOT; repo=collect.REPO; sha='7def35976f62dd153127d25217e22d3f0d19f3aa'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(q):return api('graphql','-f','query='+q)['data']
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:468){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest']
 assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main'
 assert not x['reviewThreads']['pageInfo']['hasNextPage']; return x
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
items=[
 ('PRRT_kwDOQPdUrM6mvT5q','Retargeted the scheduled/manual artist enrichment API base to https://api.tdfrecords.net. Existing scope, publication policy, authentication and concurrency are unchanged. All messaging/enrichment regressions (111 tests) and CI contract tests (25) passed. No production enrichment run or deployment was manually triggered.',True),
 ('PRRT_kwDOQPdUrM6mv4sP','The hourly and manual workflow now invokes the existing read-only --check path, rejects refresh requests, and contains no Fly setup or credentials. Missing, invalid, expired and soon-expiring tokens still fail and notify. Existing exhaustive validity/expiry tests and workflow guard tests pass. A reviewed Hetzner secret-store integration remains pending; the runbook explicitly requires operator rotation and synchronization of GitHub check credentials and does not mistake those for live-service verification. No credentials were changed.',True),
 ('PRRT_kwDOQPdUrM6mv4sV','Updated the course publisher default and production documentation to https://api.tdfrecords.net. Three mocked-request checks passed: production default, explicit course-base override precedence, and legacy VITE base override. Bearer authentication and request payload are unchanged; no course was published.',True),
 ('PRRT_kwDOQPdUrM6mvT6B','Corrected both documents to label cutover validation incomplete and distinguish the observed traffic/writer transition from the unperformed Google interactive-login and authenticated-upload gates. No gate waiver is claimed. This thread remains open until an authorized operator performs and records those end-to-end checks; the audit has not deployed or performed production writes.',False)]
for tid,msg,resolve in items:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid)
 assert not t['isResolved'] and len(t['comments']['nodes'])==1,'Concurrent discussion: preserve'
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2))
 body=f'Follow-up {sha}: '+msg
 reply=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id url body}}}')['addPullRequestReviewThreadReply']['comment'];assert reply['body']==body
 ledger(action='replied_to_review',pr=468,sha=sha,thread=tid,url=reply['url'])
 current=state();t=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)
 assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==reply['id']
 if resolve:
  r=gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread'];assert r['isResolved']
  current=state();assert next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)['isResolved']
  ledger(action='resolved_verified_review',pr=468,sha=sha,thread=tid,url=reply['url'])
 (P/(tid+'-after.json')).write_text(json.dumps(current,indent=2))
 print(tid,'resolved' if resolve else 'left open',flush=True)
before=api(repo+'/pulls/468');assert before['state']=='open' and before['head']['sha']==sha
body='''After the web-first move to Hetzner, scheduled enrichment and course publishing still targeted the retired Fly API, and hourly messaging maintenance could update Fly secrets. Point enrichment and course publishing at `https://api.tdfrecords.net`; make scheduled/manual messaging validation read-only while preserving invalid/expiring-token failures and notifications. Automatic rotation into the current secret store remains pending and is documented.

Preserve the verified restore, Cloudflare transition, callback migration, backup restore, retained Trader/shared Fly resources, and official YouTube ingestion evidence. Explicitly label cutover validation incomplete until Google interactive login and authenticated uploads are verified; no waiver or deployment is performed by this follow-up.

Validation of follow-up `'''+sha+'''`: 111 messaging/enrichment tests and 25 CI contract tests pass; mocked course requests verify the production default and both override paths; both workflow YAML documents parse and `git diff --check` passes. Three concrete endpoint/retired-writer review concerns are fixed. The authentication/upload review remains open, and current-head CI plus independent review are required. The external Datadog probe still targets retired Fly and must be corrected without weakening its assertions.
'''
(P/'cutover-followup-body-before.json').write_text(json.dumps(before,indent=2))
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
after=api(repo+'/pulls/468');assert after['body']==body and after['head']['sha']==sha
(P/'cutover-followup-body-after.json').write_text(json.dumps(after,indent=2));ledger(action='updated_validation_description',pr=468,sha=sha,url=after['html_url'])
