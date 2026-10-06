import collect,datetime,json,concurrent.futures,sys
root=collect.ROOT;collect.ROOT=root/sys.argv[1];collect.ROOT.mkdir(exist_ok=True)
start=datetime.datetime.now(datetime.timezone.utc).isoformat();collect.ROOT.joinpath('started.txt').write_text(start)
for endpoint,dest in [('branches?per_page=100','branches.json'),('pulls?state=all&per_page=100','pulls.json'),('branches/main/protection','main-protection.json'),('rulesets?per_page=100','rulesets.json')]:collect.api(collect.REPO+'/'+endpoint,dest,'?' in endpoint)
prs=sum(json.loads((collect.ROOT/'pulls.json').read_text()),[])
tracked_event_prs={pr['number'] for source in json.loads((root/'events-source-closure-candidates.json').read_text()) for pr in source['prs']}
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
 fs=[pool.submit(collect.pr_task,p) for p in prs if p['state']=='open' or p['number'] in tracked_event_prs or p['number']>=462 or p['number'] in [390,391,394,397,402,409,415,460,462,463,467,469,471,472]]
 for f in concurrent.futures.as_completed(fs):f.result()
branches=sum(json.loads((collect.ROOT/'branches.json').read_text()),[])
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
 fs=[pool.submit(collect.branch_task,b) for b in branches]
 for f in concurrent.futures.as_completed(fs):f.result()
# Retry only missing branch evidence after transient API errors; cached successful reads are retained.
for b in branches:collect.branch_task(b)
for n in [460,462,463,464,465]:
 pr=next(x for x in prs if x['number']==n)
 collect.api(collect.REPO+'/commits/'+pr['head']['sha']+'/check-runs?per_page=100',f'checks/{pr["head"]["sha"]}.json',True)
main_branch=next(b for page in json.load(open(collect.ROOT/'branches.json')) for b in page if b['name']=='main')
collect.api(collect.REPO+'/commits/'+main_branch['commit']['sha']+'/check-runs?per_page=100','main-checks.json',True)
collect.api(collect.REPO+'/commits/'+main_branch['commit']['sha']+'/status?per_page=100','main-status.json',True)
collect.api('repos/diegueins680/TDF-mobile/pulls/115','mobile-pr115.json')
collect.api('repos/diegueins680/TDF-mobile/pulls/115/reviews?per_page=100','mobile-reviews.json',True)
mobile_head=json.load(open(collect.ROOT/'mobile-pr115.json'))['head']['sha']
collect.api('repos/diegueins680/TDF-mobile/commits/'+mobile_head+'/check-runs?per_page=100','mobile-checks.json',True)
collect.ROOT.joinpath('completed.txt').write_text(datetime.datetime.now(datetime.timezone.utc).isoformat())
print('Snapshot',collect.ROOT,len(prs),'PRs',flush=True)
