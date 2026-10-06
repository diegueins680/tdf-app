import concurrent.futures, datetime, json, os, pathlib, subprocess, urllib.parse
ROOT=pathlib.Path(__file__).parent
REPO='repos/diegueins680/tdf-app'
ENV={k:v for k,v in os.environ.items() if k not in ('GH_TOKEN','GITHUB_TOKEN','GITHUB_PAT')}
def api(path, dest, paginate=False, graphql=None):
    file=ROOT/dest
    if file.exists() and file.stat().st_size: return
    cmd=['/usr/local/bin/gh','api',path]
    if paginate: cmd+=['--paginate','--slurp']
    if graphql: cmd+=['-f','query='+graphql]
    r=subprocess.run(cmd,env=ENV,text=True,capture_output=True)
    file.parent.mkdir(parents=True,exist_ok=True)
    if r.returncode:
        file.with_suffix('.error.txt').write_text(r.stderr+'\n'+r.stdout)
        print('ERROR',dest,r.stderr[:180],flush=True)
    else:
        try: data=json.loads(r.stdout)
        except Exception: data={'raw':r.stdout}
        file.write_text(json.dumps(data,indent=2))
    return r.returncode
def pr_task(pr):
    n=pr['number']; pre=f'prs/{n}'
    for suffix in ['', '/reviews', '/comments', '/files']:
        api(f'{REPO}/pulls/{n}{suffix}'+('?'+'per_page=100' if suffix else ''),pre+('/info.json' if not suffix else suffix+'.json'),bool(suffix))
    api(f'{REPO}/issues/{n}/comments?per_page=100',pre+'/discussion.json',True)
    query='''query($endCursor:String){repository(owner:"diegueins680",name:"tdf-app"){pullRequest(number:NUMBER){number reviewDecision mergeStateStatus mergeable headRefOid baseRefName isDraft closingIssuesReferences(first:100){nodes{number title body state url} pageInfo{hasNextPage endCursor}} reviewThreads(first:100,after:$endCursor){nodes{id isResolved isOutdated path line comments(first:100){nodes{author{login} body url createdAt} pageInfo{hasNextPage endCursor}}} pageInfo{hasNextPage endCursor}}}}}'''.replace('NUMBER',str(n))
    api('graphql',pre+'/threads.json',True,query)
    print('PR',n,'captured',flush=True)
def branch_task(b):
    sha=b['commit']['sha']; name=b['name']; q=urllib.parse.quote(name,safe='')
    api(f'{REPO}/commits/{sha}/check-runs?per_page=100',f'checks/{sha}.json',True)
    api(f'{REPO}/commits/{sha}/status?per_page=100',f'status/{sha}.json',True)
    api(f'{REPO}/rules/branches/{q}',f'branch-rules/{q}.json')
    if b['protected']: api(f'{REPO}/branches/{q}/protection',f'protection/{q}.json')
def main():
    branches=sum(json.loads((ROOT/'branches-pages.json').read_text()),[])
    prs=sum(json.loads((ROOT/'pulls-pages.json').read_text()),[])
    names={b['name'] for b in branches if b['name']!='main'}
    relevant=[p for p in prs if p['state']=='open' or p['head']['ref'] in names]
    endpoints=[('environments?per_page=100','environments.json'),('deployments?per_page=100','deployments.json'),('releases?per_page=100','releases.json'),('actions/workflows?per_page=100','workflows.json'),('issues?state=all&per_page=100','issues.json'),('hooks?per_page=100','hooks.json')]
    with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
        futures=[pool.submit(api,REPO+'/'+path,dest,True) for path,dest in endpoints]
        futures += [pool.submit(pr_task,p) for p in relevant]
        futures += [pool.submit(branch_task,b) for b in branches]
        for f in concurrent.futures.as_completed(futures): f.result()
    (ROOT/'details-finished.txt').write_text(datetime.datetime.now(datetime.timezone.utc).isoformat())
    print('Complete',len(relevant),'PRs',len(branches),'branches',flush=True)
if __name__=='__main__': main()
