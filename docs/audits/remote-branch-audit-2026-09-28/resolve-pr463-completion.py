import collect,subprocess,json,datetime
P=collect.ROOT;repo=collect.REPO;record=json.load(open(P/'pr463-completion-after.json'));sha=record['head']['sha'];tid='PRRT_kwDOQPdUrM6mtRZC'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def gql(query):return api('graphql','-f','query='+query)['data']
def state():
 x=gql('query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:463){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}')['repository']['pullRequest']
 assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='fix/event-confirmed-end-20260920'
 assert not x['reviewThreads']['pageInfo']['hasNextPage']
 return x
def ledger(**entry):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**entry})+'\n')
before=state();thread=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid)
assert not thread['isResolved'] and len(thread['comments']['nodes'])==1,'Concurrent review activity: preserve thread'
(P/(tid+'-before.json')).write_text(json.dumps(before,indent=2))
body=f'Fixed in {sha}. Per-event persistence errors now propagate (rescuing that change from PR464 commit 5068a0d with contributor attribution), preventing reconciliation and false success. Completion rechecks and locks the source before absence reconciliation, including empty responses; reconciliation, run completion and source success commit atomically. No network fetch occurs under the lock. Real PostgreSQL tests verify disabled empty-feed rejection with no success writes, forced final-write failure rolls back run completion, and enabled completion commits both timestamps. The existing mixed-capacity/concurrent writer/authority/revocation/rollback tests and 28 discovery examples pass; the actual Cron dependency graph compiles. No production constraint or validation was weakened.'
reply=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id url body}}}')['addPullRequestReviewThreadReply']['comment'];assert reply['body']==body
ledger(action='replied_to_review',pr=463,sha=sha,thread=tid,url=reply['url'])
current=state();thread=next(t for t in current['reviewThreads']['nodes'] if t['id']==tid)
assert not thread['isResolved'] and len(thread['comments']['nodes'])==2 and thread['comments']['nodes'][-1]['id']==reply['id']
resolved=gql('mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}')['resolveReviewThread']['thread'];assert resolved['isResolved']
verified=state();assert next(t for t in verified['reviewThreads']['nodes'] if t['id']==tid)['isResolved']
(P/(tid+'-after.json')).write_text(json.dumps(verified,indent=2))
ledger(action='resolved_verified_review',pr=463,sha=sha,thread=tid,url=reply['url'])
before=api(repo+'/pulls/463');assert before['state']=='open' and before['head']['sha']==sha and before['body']==record['body']
body=before['body']+f'\n\nSource-completion follow-up `{sha}`: propagates per-event failures and locks/rechecks source enablement before atomic absence reconciliation and success updates, including empty feeds. Rescues PR464 failure propagation with attribution. Real PostgreSQL disabled-source/rollback/enabled-completion regressions and the original boundary suite pass; all 28 discovery tests, actual Cron dependency compilation, catalog gate and specification inventory pass. The third review concern was addressed and resolved with evidence. Current-head CI and renewed review still govern merge readiness; native-stack history constraints remain.\n'
api(repo+'/pulls/463','--method','PATCH','-f','body='+body)
after=api(repo+'/pulls/463');assert after['body']==body and after['head']['sha']==sha
(P/'pr463-completion-after.json').write_text(json.dumps(after,indent=2))
ledger(action='updated_validation_description',pr=463,sha=sha,url=after['html_url'])
print('Verified third concern resolved and description updated',sha)
