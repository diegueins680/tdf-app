import collect, subprocess, json, datetime
P=collect.ROOT
repo=collect.REPO
head='4100a04c0de9496e7bf9cd6d656602dee1d8e277'
base='49f1f0ec087067d17f62d91df1b616eb053eb894'
def api(path,*args):
    return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def pages(path):
    return api(path,'--paginate','--slurp')
pr=api(repo+'/pulls/475')
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
query='''query($endCursor:String){repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:475){headRefOid baseRefName reviewDecision mergeStateStatus reviewThreads(first:100,after:$endCursor){nodes{id isResolved} pageInfo{hasNextPage endCursor}}}}}'''
threads=api('graphql','--paginate','--slurp','-f','query='+query)
for page in threads:
    d=page['data']['repository']['pullRequest']
    assert d['headRefOid']==head and d['baseRefName']=='main' and d['reviewDecision']=='APPROVED' and d['mergeStateStatus']=='CLEAN'
    assert all(t['isResolved'] for t in d['reviewThreads']['nodes'])
reviews=pages(repo+'/pulls/475/reviews?per_page=100')
latest={}
for page in reviews:
    for review in page:
        if review['state'] in ('APPROVED','CHANGES_REQUESTED','DISMISSED'):
            latest[review['user']['login']]=review
assert any(r['state']=='APPROVED' and r['commit_id']==head and name!=pr['user']['login'] for name,r in latest.items())
assert not any(r['state']=='CHANGES_REQUESTED' for r in latest.values())
ancestry=json.loads((P/'editorial-review-ancestry-verified.json').read_text())
assert ancestry['head']==head and ancestry['result']=='PASS' and ancestry['count']==160
tree=api(repo+'/git/commits/'+head)['tree']['sha']
assert api(repo+'/git/ref/heads/main')['object']['sha']==base
again=api(repo+'/pulls/475')
assert again['head']['sha']==head and again['base']['sha']==base and again['state']=='open' and again['mergeable_state']=='clean'
(P/'merge475-safety.json').write_text(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'pr':again,'settings':settings,'protection':protection,'rules':rules,'checks':checks,'status':status,'reviews':reviews,'threads':threads,'tested_tree':tree},indent=2))
result=api(repo+'/pulls/475/merge','--method','PUT','-f','merge_method=merge','-f','sha='+head)
(P/'merge475-response.json').write_text(json.dumps(result,indent=2))
assert result['merged'] is True
merged=api(repo+'/pulls/475')
assert merged['merged'] and merged['state']=='closed' and merged['head']['sha']==head and merged['merge_commit_sha']==result['sha']
commit=api(repo+'/git/commits/'+result['sha'])
assert commit['tree']['sha']==tree
assert {p['sha'] for p in commit['parents']}=={base,head}
ref=api(repo+'/git/ref/heads/'+pr['head']['ref'])
assert ref['object']['sha']==head
(P/'merge475-verified.json').write_text(json.dumps({'pr':merged,'commit':commit,'branch':ref,'tree_matches_tested_head':True},indent=2))
with (P/'mutations.jsonl').open('a') as out:
    out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'merged_pull_request','pr':475,'sha':head,'base_sha':base,'merge_commit':result['sha'],'url':merged['html_url'],'strategy':'merge','branch_deleted':False,'tree_matches_tested_head':True})+'\n')
print('Verified normal merge PR475',result['sha'],'with exact tested tree and branch retained')
