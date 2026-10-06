import collect,subprocess,json,datetime
P=collect.ROOT;repo=collect.REPO;sha='648f2d6dc69080d4438ae2a25309497012ded2d1';base='cc244b1f86603055997b51379b297baebfd3e7ce'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
before=api(repo+'/pulls/460');assert before['state']=='open' and before['head']['sha']==sha and before['base']['ref']=='main' and before['base']['sha']==base and not before['draft'] and before['mergeable']
assert api(repo+'/branches/main')['commit']['sha']==base
protection=api(repo+'/branches/main/protection');assert protection['required_pull_request_reviews']['required_approving_review_count']==1 and protection['required_conversation_resolution']['enabled']
rules=api(repo+'/rulesets/9478019');assert rules['enforcement']=='active' and not rules['bypass_actors']
reviews=api(repo+'/pulls/460/reviews?per_page=100','--paginate','--slurp');assert any(r['state']=='APPROVED' and r['commit_id']==sha and r['user']['login']!='diegueins680' for page in reviews for r in page)
query='query{repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:460){reviewDecision headRefOid baseRefName mergeStateStatus reviewThreads(first:100){nodes{id isResolved} pageInfo{hasNextPage}}}}}'
review=api('graphql','-f','query='+query)['data']['repository']['pullRequest'];assert review['reviewDecision']=='APPROVED' and review['headRefOid']==sha and review['baseRefName']=='main' and review['mergeStateStatus']=='CLEAN'
assert not review['reviewThreads']['pageInfo']['hasNextPage'] and all(t['isResolved'] for t in review['reviewThreads']['nodes'])
checks=api(repo+'/commits/'+sha+'/check-runs?per_page=100','--paginate','--slurp');runs=[c for page in checks for c in page['check_runs']];assert runs and all(c['head_sha']==sha and c['status']=='completed' and c['conclusion']=='success' for c in runs)
status=api(repo+'/commits/'+sha+'/status');assert status['state']=='success' or not status['statuses']
assert '26 examples, 0 failures' in (P/'pr460-discovery-tests.log').read_text()
evidence={'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'pr':before,'protection':protection,'ruleset':rules,'reviews':reviews,'review_threads':review,'checks':checks,'status':status,'local_validation':'26 EventDiscovery examples passed on isolated normal merge with current main, Stack GHC9.10.3 -Wall; source diff limited to nullable end normalization/persistence and tests.'}
(P/'merge-460-before.json').write_text(json.dumps(evidence,indent=2))
subprocess.run(['/usr/local/bin/gh','pr','merge','460','--repo','diegueins680/tdf-app','--merge','--match-head-commit',sha],env=collect.ENV,check=True)
after=api(repo+'/pulls/460');assert after['merged'] and after['state']=='closed' and after['head']['sha']==sha and after['merge_commit_sha']
main=api(repo+'/branches/main');commit=api(repo+'/commits/'+after['merge_commit_sha']);assert any(p['sha']==sha for p in commit['parents'])
(P/'merge-460-after.json').write_text(json.dumps({'pr':after,'main':main,'commit':commit},indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'merged_pr','pr':460,'sha':sha,'merge_commit':after['merge_commit_sha'],'url':after['html_url'],'strategy':'merge','approval':'tdfrecords exact-head approval; GraphQL APPROVED/CLEAN; all checks success; no unresolved threads','tests':'26 focused examples on current-main integration, 0 failures'})+'\n')
print('Verified merged PR460',after['merge_commit_sha'])
