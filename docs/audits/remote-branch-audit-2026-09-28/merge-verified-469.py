import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;head='cda2c5f7790126f35bd7e09bed35b52b5a0012ef';base='7e7106b36e7ac427711c2f64af650527d619f9ce'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
def verify(draft):
 pr=api(repo+'/pulls/469');assert pr['state']=='open' and pr['head']['sha']==head and pr['base']['ref']=='main' and pr['base']['sha']==base and pr['draft']==draft and pr['mergeable'] and not pr.get('stack')
 assert api(repo+'/git/ref/heads/main')['object']['sha']==base
 settings=api(repo);assert settings['allow_merge_commit'] and not settings['delete_branch_on_merge']
 checks=api(repo+'/commits/'+head+'/check-runs?per_page=100');assert checks['total_count']==len(checks['check_runs']) and all(x['head_sha']==head and x['status']=='completed' and x['conclusion'] in ['success','skipped'] for x in checks['check_runs'])
 status=api(repo+'/commits/'+head+'/status');assert status['state']=='success'
 q='query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:469){id headRefOid baseRefName reviewDecision reviewThreads(first:100){nodes{id isResolved} pageInfo{hasNextPage}} reviews(last:100){nodes{author{login} state commit{oid}} pageInfo{hasPreviousPage}}}}}'
 data=api('graphql','-f','query='+q)['data']['repository']['pullRequest'];assert data['headRefOid']==head and data['reviewDecision']=='APPROVED' and not data['reviewThreads']['pageInfo']['hasNextPage'] and all(t['isResolved'] for t in data['reviewThreads']['nodes']);assert any(r['state']=='APPROVED' and r['commit']['oid']==head and r['author']['login']!='diegueins680' for r in data['reviews']['nodes'])
 (P/('merge469-safety-'+str(draft)+'.json')).write_text(json.dumps({'pr':pr,'checks':checks,'status':status,'reviews':data},indent=2));return pr,data
pr,data=verify(True)
r=api('graphql','-f','query='+'mutation{markPullRequestReadyForReview(input:{pullRequestId:'+json.dumps(data['id'])+'}){pullRequest{number isDraft headRefOid}}}')['data']['markPullRequestReadyForReview']['pullRequest'];assert not r['isDraft'] and r['headRefOid']==head;ledger(action='marked_ready_after_verified_checks',pr=469,sha=head,url=pr['html_url'])
pr,data=verify(False)
r=api(repo+'/pulls/469/merge','--method','PUT','-f','merge_method=merge','-f','sha='+head);(P/'merge469-response.json').write_text(json.dumps(r,indent=2));assert r['merged'] is True
result=api(repo+'/pulls/469');assert result['merged'] and result['state']=='closed' and result['head']['sha']==head and result['merge_commit_sha']==r['sha'];(P/'merge469-verified.json').write_text(json.dumps(result,indent=2));ledger(action='merged_pull_request',pr=469,sha=head,base_sha=base,merge_commit=r['sha'],url=result['html_url'],strategy='merge',branch_deleted=False)
assert api(repo+'/git/ref/heads/audit/event-discovery-integration-20260928')['object']['sha']==head
print('Verified merged PR469',r['sha'])
