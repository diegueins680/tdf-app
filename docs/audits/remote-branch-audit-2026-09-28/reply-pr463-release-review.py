import collect,subprocess,json,datetime
P=collect.ROOT;sha='a125a347cacb8d941a515171e2a81d4c92f2c2d2';replacement='cda2c5f7790126f35bd7e09bed35b52b5a0012ef';tid='PRRT_kwDOQPdUrM6mu3Os'
def gql(q):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api','graphql','-f','query='+q],env=collect.ENV,text=True))['data']
for head in [sha,replacement]:subprocess.run(['git','merge-base','--is-ancestor','441dfcc968559f829e84c89751f008657566138c',head],cwd=P/'repo',check=True)
probe=subprocess.run(['/usr/local/bin/gh','api',collect.REPO+'/commits/39120b84ac361cc1498c7b4d32c073bc440eb503'],env=collect.ENV,text=True,capture_output=True)
(P/'pr463-release-review-probe.json').write_text(json.dumps({'exit_code':probe.returncode,'stdout':probe.stdout,'stderr':probe.stderr},indent=2))
q='query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:463){state headRefOid reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}'
x=gql(q)['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and not x['reviewThreads']['pageInfo']['hasNextPage']
t=next(t for t in x['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
body=f'The current published head is {sha}; git merge-base --is-ancestor 441dfcc968559f829e84c89751f008657566138c {sha} exits 0. The same check passes for standalone replacement #469 at {replacement}, and all 116 registry introductions are ancestors there. GitHub currently returns HTTP 422 for the cited 39120b84ac361cc1498c7b4d32c073bc440eb503, so I cannot verify that release object and am leaving this thread open. No migration introduction is being changed to bypass ancestry. The ordinary native-stack merge was rejected; its rebasing API was not invoked. #469 preserves the complete original history through a normal merge into main and requires fresh independent review and CI before integration. This original PR remains open until the replacement is successfully merged and verified.'
r=gql('mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}')['addPullRequestReviewThreadReply']['comment'];assert r['body']==body
after=gql(q)['repository']['pullRequest'];t=next(t for t in after['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and any(c['id']==r['id'] for c in t['comments']['nodes'])
(P/'pr463-release-review-after.json').write_text(json.dumps(after,indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'replied_to_unresolved_release_review','pr':463,'sha':sha,'thread':tid,'url':r['url'],'resolved':False})+'\n')
print('Verified release-review evidence reply; thread intentionally open',r['url'])
