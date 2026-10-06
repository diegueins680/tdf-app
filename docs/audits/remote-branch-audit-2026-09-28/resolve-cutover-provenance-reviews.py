import collect,json,subprocess,datetime
P=collect.ROOT;head=json.loads((P/'cutover-provenance-push-verified.json').read_text())['head']['sha']
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def check():
 p=api(collect.REPO+'/pulls/468');assert p['state']=='open' and p['head']['sha']==head;return p
messages=[('PRRT_kwDOQPdUrM6otNfL',4175475341,'The inventory now performs fresh DNS/TLS/peer-bound public health and version requests after the SQL query and SSH after-snapshot, then validates both against the same deployment before emitting a report. A regression injects a mid-query origin, health or commit change and requires failure; the no-post-query-check negative control fails that regression.'),('PRRT_kwDOQPdUrM6otRPH',4175499359,'The shared access helper now requires configured POSTGRES_IMAGE to be digest-pinned and match the running database Config.Image or its registry digest before metadata, inventory or credential access. It records configuredDatabaseImage and rejects changes during inventory. Regressions reject missing, mutable, wrong and stale database references across all modes, and accept a matching registry digest distinct from the local image ID. The previous helper fails15 new rejection cases.')]
for thread,comment,detail in messages:
 check();body='Fixed in `'+head+'`. '+detail+' All11 access,6 catalog and6 mail tests plus the strict1162-candidate catalog gate and inventory check pass. Live SSH currently times out; no current production-image/inventory verification is claimed. The independent interactive-login/upload acceptance thread remains open.'
 result=api(collect.REPO+f'/pulls/468/comments/{comment}/replies','--method','POST','-f','body='+body)
 (P/f'cutover-provenance-review-{comment}-reply.json').write_text(json.dumps(result,indent=2))
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'review_reply','pr':468,'sha':head,'url':result['html_url'],'thread':thread})+'\n')
 check();query='mutation{resolveReviewThread(input:{threadId:"'+thread+'"}){thread{id isResolved}}}'
 resolved=api('graphql','-f','query='+query);assert resolved['data']['resolveReviewThread']['thread']['isResolved']
 fresh=api('graphql','-f','query=query{node(id:"'+thread+'"){... on PullRequestReviewThread{id isResolved}}}')
 assert fresh['data']['node']['isResolved']
 (P/f'cutover-provenance-review-{comment}-resolved.json').write_text(json.dumps(fresh,indent=2))
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'resolved_review_thread','pr':468,'sha':head,'thread':thread,'evidence':f'cutover-provenance-review-{comment}-resolved.json'})+'\n')
 print('Verified addressed thread',thread,flush=True)
