import collect,subprocess,json,datetime
P=collect.ROOT;repo=collect.REPO;sha='697bd58b5ea1c8134be490cce588b65e9a08dad8'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(query):return api('graphql','-f','query='+query)['data']
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:463){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest']
 assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='fix/event-confirmed-end-20260920'
 assert not x['reviewThreads']['pageInfo']['hasNextPage']
 return x
def ledger(**entry):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**entry})+'\n')
comments={
 'PRRT_kwDOQPdUrM6mht0D':f'Verified on {sha}: introduction commit 441dfcc968559f829e84c89751f008657566138c is in the current release ancestry. All 115 registered introduction commits pass git merge-base --is-ancestor, and all 82 production-release tests pass. The earlier review snapshot is superseded by the existing normal commits; no ancestry guard or manifest entry was weakened. Native-stack automatic rebasing has not been invoked.',
 'PRRT_kwDOQPdUrM6mht0E':f'Fixed in {sha}. The pilot-status endpoint and PostgreSQL import preflight now call the same canonical-identity count used by the database capacity guard. The real PostgreSQL regression exercises mixed candidates/imports, linked-event deduplication, duplicate source references and suppression (20 → 20 → 19 → 20), with fixture changes rolled back. Mixed-writer races, authority/revocation and rollback/reapplication still pass; all 28 focused examples and the actual handler/dependency compilation pass. The existing response field is retained for compatibility.'}
for tid,body in comments.items():
 before=state();thread=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid)
 assert not thread['isResolved'] and len(thread['comments']['nodes'])==1,'Concurrent review activity: preserve thread'
 (P/(tid+'-before.json')).write_text(json.dumps(before,indent=2))
 reply=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id url body}}}')['addPullRequestReviewThreadReply']['comment'];assert reply['body']==body
 ledger(action='replied_to_review',pr=463,sha=sha,thread=tid,url=reply['url'])
 current=state();thread=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)
 assert len(thread['comments']['nodes'])==2 and thread['comments']['nodes'][-1]['id']==reply['id'] and not thread['isResolved']
 resolved=gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread'];assert resolved['isResolved']
 verified=state();assert next(t for t in verified['reviewThreads']['nodes'] if t['id']==tid)['isResolved']
 (P/(tid+'-after.json')).write_text(json.dumps(verified,indent=2))
 ledger(action='resolved_verified_review',pr=463,sha=sha,thread=tid,url=reply['url'])
 print('Verified resolved',tid)
before=api(repo+'/pulls/463');record=json.load(open(P/'pr463-fix-after.json'));assert before['head']['sha']==sha and before['state']=='open' and before['body']==record['body']
body=before['body']+f'\n\nReview repair at `{sha}`: the status endpoint and import preflight share the database canonical-identity capacity count. The existing response field remains compatible. The real PostgreSQL mixed/deduplicated/suppressed count regression, concurrent writer/authority/revocation/rollback tests, 28 focused Hspec examples, actual handler compilation, 82 release tests and all 115 migration-ancestor checks pass. Both original review concerns were addressed and resolved with evidence. Current-head hosted validation and any required renewed approval remain pending. Native stack466 must use an integration approach that preserves shared history and production introduction ancestry; no automatic rebase or deployment was performed.\n'
api(repo+'/pulls/463','--method','PATCH','-f','body='+body)
after=api(repo+'/pulls/463');assert after['body']==body and after['head']['sha']==sha
(P/'pr463-fix-after.json').write_text(json.dumps(after,indent=2))
ledger(action='updated_validation_description',pr=463,sha=sha,url=after['html_url'])
