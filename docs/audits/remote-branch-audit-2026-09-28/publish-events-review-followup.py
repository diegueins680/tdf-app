import collect,subprocess,json,datetime,time
P=collect.ROOT;cwd=P/'events';old='23ff8f588bbdefd4d14e0d2a39cc39953d2c114f';branch='audit/event-stack-consolidation-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
files=['docs/catalog-persistence/event-catalog-current-successors.json','docs/catalog-persistence/event-integration-retirements.json','docs/event-operations/gap-matrix.md','docs/event-operations/task-view-contract.md','formal/system/inventory.json','tdf-hq-ui/src/pages/ArtistPublicPage.component.test.tsx','tdf-hq-ui/src/pages/ArtistPublicPage.tsx','tdf-hq-ui/src/pages/EventLogisticsPage.tsx']
assert set(subprocess.check_output(['git','diff','--name-only'],cwd=cwd,text=True).splitlines())==set(files)
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()==old
assert 'Tests:       27 passed' in (P/'events-current-main-review-ui.log').read_text()
assert 'ℹ pass 5' in (P/'events-current-main-catalog-regression-repair.log').read_text()
validation=json.loads((P/'events-review-command-results.json').read_text());assert all(v==0 for v in validation.values())
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'events-review-before.json').write_text(json.dumps(x,indent=2)+'\n')
subprocess.run(['git','diff','--check'],cwd=cwd,check=True)
subprocess.run(['git','add','--',*files],cwd=cwd,check=True)
subprocess.run(['git','commit','-m','Preserve late follow cache updates and defer disabled task entry'],cwd=cwd,check=True)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==old and x['base']['ref']=='main'
assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==old
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(collect.REPO+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 x=api(collect.REPO+'/pulls/465')
 if x['head']['sha']==sha:break
 time.sleep(2)
assert x['head']['sha']==sha
(P/'events-review-after.json').write_text(json.dumps(x,indent=2)+'\n')
ledger(action='pushed_event_review_and_catalog_repair',pr=465,old_sha=old,sha=sha,url=x['html_url'],validation='27 focused UI regressions; 5 unchanged catalog regressions; zero-warning UI lint/typecheck; catalog gate and regenerated specification inventory. Both historical decision payloads and previous successor IDs preserved. Full current-head hosted validation and independent review remain required.')
print('Verified normal push',sha,flush=True)
def state():
 q='query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:465){state headRefOid baseRefName reviewThreads(first:100){nodes{id isResolved comments(first:100){nodes{id body url}}} pageInfo{hasNextPage}}}}}'
 x=api('graphql','-f','query='+q)['data']['repository']['pullRequest'];assert x['state']=='OPEN' and x['headRefOid']==sha and x['baseRefName']=='main' and not x['reviewThreads']['pageInfo']['hasNextPage'];return x
for tid,msg in [('PRRT_kwDOQPdUrM6nx9wW','Removed the logistics task CTA while the adapter migrations remain outside the production manifest and event.operations.api defaults disabled. Direct routes and isolated enabled adapter tests remain intact. The activation contract and gap matrix now explicitly defer discoverable links until an authenticated backend availability contract and separately reviewed activation. No flag, migration or security control was enabled.'),('PRRT_kwDOQPdUrM6nx9wa','Successful follow and unfollow writes now invalidate the original viewer, artist directory and profile cache before the current-navigation guard. Analytics and resume navigation remain fenced. Two delayed-mutation component regressions verify shared data refreshes on the newly rendered profile and the correct action after returning; all five previous logout/session/unmount/navigation suppression cases remain intact. The focused UI run passes 27 tests.')]:
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==1
 body='Fixed in '+sha+'. '+msg
 q='mutation{addPullRequestReviewThreadReply(input:{pullRequestReviewThreadId:'+json.dumps(tid)+',body:'+json.dumps(body)+'}){comment{id body url}}}'
 r=api('graphql','-f','query='+q)['data']['addPullRequestReviewThreadReply']['comment'];assert r['body']==body;ledger(action='replied_to_review',pr=465,sha=sha,thread=tid,url=r['url'])
 before=state();t=next(t for t in before['reviewThreads']['nodes'] if t['id']==tid);assert not t['isResolved'] and len(t['comments']['nodes'])==2 and t['comments']['nodes'][-1]['id']==r['id']
 q='mutation{resolveReviewThread(input:{threadId:'+json.dumps(tid)+'}){thread{id isResolved}}}'
 assert api('graphql','-f','query='+q)['data']['resolveReviewThread']['thread']['isResolved']
 after=state();assert next(t for t in after['reviewThreads']['nodes'] if t['id']==tid)['isResolved'];(P/(tid+'-after.json')).write_text(json.dumps(after,indent=2)+'\n');ledger(action='resolved_verified_review',pr=465,sha=sha,thread=tid,url=r['url'])
print('Verified both addressed review resolutions',flush=True)
