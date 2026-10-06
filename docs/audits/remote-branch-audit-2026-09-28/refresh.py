import collect,datetime,json,concurrent.futures
root=collect.ROOT;collect.ROOT=root/'refresh';collect.ROOT.mkdir(exist_ok=True)
collect.api(collect.REPO+'/branches?per_page=100','branches.json',True)
collect.api(collect.REPO+'/pulls?state=all&per_page=100','pulls.json',True)
prs=sum(json.loads((collect.ROOT/'pulls.json').read_text()),[])
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
 fs=[pool.submit(collect.pr_task,p) for p in prs if p['state']=='open' or p['number']==462]
 for f in concurrent.futures.as_completed(fs):f.result()
collect.ROOT.joinpath('timestamp.txt').write_text(datetime.datetime.now(datetime.timezone.utc).isoformat())
print('Refreshed',len(prs),'PR records',flush=True)
