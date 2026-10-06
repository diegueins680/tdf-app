import concurrent.futures,json,pathlib,subprocess,urllib.parse
from collect import api,ROOT,REPO
branches=sum(json.loads((ROOT/'branches-pages.json').read_text()),[])
deployments=sum(json.loads((ROOT/'deployments.json').read_text()),[])
tasks=[]
for b in branches:
    matches=[d for d in deployments if d['sha']==b['commit']['sha'] or d['ref'] in (b['name'],'refs/heads/'+b['name'])]
    (ROOT/'deployment-matches').mkdir(exist_ok=True)
    (ROOT/'deployment-matches'/(urllib.parse.quote(b['name'],safe='')+'.json')).write_text(json.dumps(matches,indent=2))
    # Latest deployment of each branch head in each environment; retain all historical records separately.
    seen=set()
    for d in matches:
        if d['environment'] in seen:continue
        seen.add(d['environment']);tasks.append((f'{REPO}/deployments/{d["id"]}/statuses?per_page=100',f'deployment-status/{d["id"]}.json',True))
for n in [128,130]:tasks.append((f'{REPO}/issues/{n}/comments?per_page=100',f'issues/{n}-comments.json',True))
with concurrent.futures.ThreadPoolExecutor(max_workers=5) as pool:
    for f in concurrent.futures.as_completed([pool.submit(api,*t) for t in tasks]):f.result()
print('Operational status collections',len(tasks),flush=True)
