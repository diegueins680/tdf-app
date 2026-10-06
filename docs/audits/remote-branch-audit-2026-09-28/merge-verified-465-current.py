import collect, subprocess, json, datetime, time
P=collect.ROOT
repo=collect.REPO
head='ee58bd13ebef6f56cfdafcd4ab45ac6ecc6bec4b'
base='c4d479c7f3da659368e5bc60464a1a43c99d9b7a'
def api(path,*args):
    return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def pages(path):
    return api(path,'--paginate','--slurp')
while True:
    assert api(repo+'/git/ref/heads/main')['object']['sha']==base
    current=api(repo+'/pulls/465')
    assert current['head']['sha']==head and current['state']=='open'
    waiting=pages(repo+'/commits/'+head+'/check-runs?per_page=100')
    ws=[c for pg in waiting for c in pg['check_runs']]
    failed=[c['name'] for c in ws if c['status']=='completed' and c['conclusion'] not in ('success','skipped')]
    assert not failed,failed
    if ws and all(c['status']=='completed' and c['conclusion'] in ('success','skipped') for c in ws):break
    print('Waiting for exact-head PR465 CI',flush=True)
    time.sleep(30)
while True:
    assert api(repo+'/git/ref/heads/main')['object']['sha']==base
    base_checks=pages(repo+'/commits/'+base+'/check-runs?per_page=100')
    base_runs=pages(repo+'/actions/runs?head_sha='+base+'&per_page=100')
    bs=[c for pg in base_checks for c in pg['check_runs']]+[r for pg in base_runs for r in pg['workflow_runs']]
    assert bs and not any(c['status']=='completed' and c['conclusion'] not in ('success','skipped') for c in bs)
    base_status=api(repo+'/commits/'+base+'/status')
    if all(c['status']=='completed' and c['conclusion'] in ('success','skipped') for c in bs) and base_status['state']=='success':break
    print('Waiting for normal target CI',flush=True);time.sleep(30)
(P/'notification-rescue-main-ci.json').write_text(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'target':base,'checks':base_checks,'runs':base_runs,'status':base_status,'all_passed':True},indent=2))
pr=api(repo+'/pulls/465')
assert pr['state']=='open' and not pr['draft'] and not pr['merged']
assert pr['head']['sha']==head and pr['base']['ref']=='main' and pr['base']['sha']==base
assert pr['mergeable'] and pr['mergeable_state']=='clean' and not pr.get('stack')
settings=api(repo)
assert settings['allow_merge_commit'] and not settings['delete_branch_on_merge']
protection=api(repo+'/branches/main/protection')
rules=api(repo+'/rules/branches/main')
assert all(r['type'] in ('deletion','non_fast_forward') for r in rules),rules
assert protection['required_pull_request_reviews']['required_approving_review_count']==1
assert not protection['required_pull_request_reviews']['require_code_owner_reviews']
assert not protection.get('required_status_checks'),protection
checks=pages(repo+'/commits/'+head+'/check-runs?per_page=100')
all_checks=[c for page in checks for c in page['check_runs']]
assert all_checks and all(c['head_sha']==head and c['status']=='completed' and c['conclusion'] in ('success','skipped') for c in all_checks)
status=api(repo+'/commits/'+head+'/status?per_page=100')
assert status['state']=='success'
query='''query($endCursor:String){repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:465){headRefOid baseRefName reviewDecision mergeStateStatus reviewThreads(first:100,after:$endCursor){nodes{id isResolved} pageInfo{hasNextPage endCursor}}}}}'''
threads=api('graphql','--paginate','--slurp','-f','query='+query)
for page in threads:
    d=page['data']['repository']['pullRequest']
    assert d['headRefOid']==head and d['baseRefName']=='main' and d['reviewDecision']=='APPROVED' and d['mergeStateStatus']=='CLEAN'
    assert all(t['isResolved'] for t in d['reviewThreads']['nodes'])
reviews=pages(repo+'/pulls/465/reviews?per_page=100')
latest={}
for page in reviews:
    for review in page:
        if review['state'] in ('APPROVED','CHANGES_REQUESTED','DISMISSED'):
            latest[review['user']['login']]=review
assert any(r['state']=='APPROVED' and r['commit_id']==head and name!=pr['user']['login'] for name,r in latest.items())
assert not any(r['state']=='CHANGES_REQUESTED' for r in latest.values())
mobile=api('repos/diegueins680/TDF-mobile/pulls/115')
assert mobile['merged'] and mobile['head']['sha']=='ede1f2a0ccf75f3794c7892f291e1751b279d19e'
mobile_checks=pages('repos/diegueins680/TDF-mobile/commits/'+mobile['merge_commit_sha']+'/check-runs?per_page=100')
assert all(c['status']=='completed' and c['conclusion'] in ('success','skipped') for pg in mobile_checks for c in pg['check_runs'])
issues=api('graphql','-f','query='+query.replace('reviewThreads(first:100,after:$endCursor)', 'closingIssuesReferences(first:100){nodes{number} pageInfo{hasNextPage}} reviewThreads(first:100,after:$endCursor)'))
assert not issues['data']['repository']['pullRequest']['closingIssuesReferences']['nodes']
branches=[b for pg in pages(repo+'/branches?per_page=100') for b in pg]
by_name={b['name']:b['commit']['sha'] for b in branches}
source_proofs=[]
for source in json.loads((P/'events-source-closure-candidates.json').read_text()):
    assert by_name[source['branch']]==source['head'],source['branch']
    subprocess.run(['git','merge-base','--is-ancestor',source['head'],head],cwd=P/'events-editorial-repair-20261003',check=True)
    source_proofs.append({'branch':source['branch'],'head':source['head'],'ancestor_of':head})
manifest=json.loads((P/'events-editorial-repair-20261003/scripts/production-migrations.json').read_text())
for migration in manifest['migrations']:
    subprocess.run(['git','merge-base','--is-ancestor',migration['introducedBy'],head],cwd=P/'events-editorial-repair-20261003',check=True)
(P/'events-premerge-source-and-migration-proof.json').write_text(json.dumps({'source_branches':source_proofs,'migration_count':len(manifest['migrations']),'mobile_merge':mobile['merge_commit_sha'],'mobile_checks':mobile_checks,'linked_issues':issues,'result':'PASS'},indent=2))
composition=json.loads((P/'events-notification-merge-validation.json').read_text())
assert composition['local_candidate']==head and composition['base']==base and composition['inventory_check']=='PASS' and composition['document_exactly_matches_reviewed_main']
tree=composition['expected_merge_tree']
assert subprocess.check_output(['git','merge-tree','--write-tree',base,head],cwd=P/'events-editorial-repair-20261003',text=True).splitlines()[0]==tree
assert api(repo+'/git/ref/heads/main')['object']['sha']==base
again=api(repo+'/pulls/465')
assert again['head']['sha']==head and again['base']['sha']==base and again['state']=='open' and again['mergeable_state']=='clean'
(P/'merge465-safety.json').write_text(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'pr':again,'settings':settings,'protection':protection,'rules':rules,'checks':checks,'status':status,'reviews':reviews,'threads':threads,'tested_tree':tree},indent=2))
result=api(repo+'/pulls/465/merge','--method','PUT','-f','merge_method=merge','-f','sha='+head)
(P/'merge465-response.json').write_text(json.dumps(result,indent=2))
assert result['merged'] is True
merged=api(repo+'/pulls/465')
assert merged['merged'] and merged['state']=='closed' and merged['head']['sha']==head and merged['merge_commit_sha']==result['sha']
commit=api(repo+'/git/commits/'+result['sha'])
assert commit['tree']['sha']==tree
assert {p['sha'] for p in commit['parents']}=={base,head}
ref=api(repo+'/git/ref/heads/'+pr['head']['ref'])
assert ref['object']['sha']==head
(P/'merge465-verified.json').write_text(json.dumps({'pr':merged,'commit':commit,'branch':ref,'tree_matches_validated_candidate':True},indent=2))
with (P/'mutations.jsonl').open('a') as out:
    out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'merged_pull_request','pr':465,'sha':head,'base_sha':base,'merge_commit':result['sha'],'url':merged['html_url'],'strategy':'merge','branch_deleted':False,'tree_matches_validated_candidate':True})+'\n')
print('Verified normal merge PR465',result['sha'],'with exact validated composition tree and branch retained')
