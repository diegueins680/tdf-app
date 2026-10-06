import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO
target='f8925e339447cb593f7cce5d86403d1396aaf5a5'
source='fdfcd7e4a78273c0e010f5515c061ee4086d26b7'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def pages(path):return api(path,'--paginate','--slurp')
def log(**kw):
    with (P/'mutations.jsonl').open('a') as out:out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
def target_checks():
    assert api(repo+'/git/ref/heads/main')['object']['sha']==target
    runs=pages(repo+'/actions/runs?head_sha='+target+'&per_page=100')
    checks=pages(repo+'/commits/'+target+'/check-runs?per_page=100')
    status=api(repo+'/commits/'+target+'/status?per_page=100')
    rs=[r for page in runs for r in page['workflow_runs']]
    cs=[c for page in checks for c in page['check_runs']]
    assert rs and cs
    failures=[x['name'] for x in rs+cs if x['status']=='completed' and x['conclusion'] not in ('success','skipped')]
    assert not failures,failures
    green=all(x['status']=='completed' and x['conclusion'] in ('success','skipped') for x in rs+cs) and status['state']=='success'
    evidence={'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'target':target,'runs':runs,'checks':checks,'status':status,'all_passed':green}
    (P/'editorial-closure-main-ci-latest.json').write_text(json.dumps(evidence,indent=2))
    if green:(P/'editorial-closure-main-ci.json').write_text(json.dumps(evidence,indent=2))
    return green
while not target_checks():
    print('Waiting for normal post-merge CI on',target,flush=True)
    time.sleep(30)
replacement=api(repo+'/pulls/475')
assert replacement['merged'] and replacement['merge_commit_sha']==target
pr=api(repo+'/pulls/464')
assert pr['state']=='open' and pr['head']['sha']==source and not pr['merged']
assert api(repo+'/git/ref/heads/'+pr['head']['ref'])['object']['sha']==source
comparison=api(repo+'/compare/'+source+'...'+target)
assert comparison['behind_by']==0 and comparison['merge_base_commit']['sha']==source
discussion=pages(repo+'/issues/464/comments?per_page=100')
(P/'close-464-integrated-proof.json').write_text(json.dumps({'pr':pr,'replacement':replacement,'compare':comparison,'discussion':discussion},indent=2))
body='Classification: ALREADY_MERGED through approved replacement #475. Original head `'+source+'` is an ancestor of main `'+target+'`; the normal merge retains contributor history and the exact fully tested replacement tree. The replacement fixes the private/public metadata boundary, durable editorial overrides, recurring reconciliation and API/directory visibility, including the forward-only migration. Current-head approval, 3,553 backend examples, compiled PostgreSQL/runtime/schema tests and the 160-migration rehearsal passed; normal post-merge main CI also passed. All useful source work was retained, including field ownership and source-failure behavior. Closing this superseded PR without rewriting its native stack. The branch remains intact. Recovery head: `'+source+'`.'
assert target_checks()
fresh=api(repo+'/pulls/464');assert fresh['state']=='open' and fresh['head']['sha']==source and fresh['base']['ref']==pr['base']['ref']
comment=api(repo+'/issues/464/comments','--method','POST','-f','body='+body)
verified_comment=api(repo+'/issues/comments/'+str(comment['id']));assert verified_comment['body']==body
log(action='commented_integrated_closure_evidence',pr=464,sha=source,url=comment['html_url'],replacement=475)
assert target_checks()
fresh=api(repo+'/pulls/464');assert fresh['state']=='open' and fresh['head']['sha']==source and fresh['base']['ref']==pr['base']['ref']
api(repo+'/pulls/464','--method','PATCH','-f','state=closed')
after=api(repo+'/pulls/464')
assert after['state']=='closed' and not after['merged'] and after['head']['sha']==source
assert api(repo+'/git/ref/heads/'+pr['head']['ref'])['object']['sha']==source
(P/'close-464-integrated-after.json').write_text(json.dumps(after,indent=2))
log(action='closed_unmerged',pr=464,sha=source,url=after['html_url'],comment=comment['html_url'],replacement=475,branch_retained=True)
print('Verified source PR464 closed after passing target CI; branch retained',flush=True)
