import collect,json,subprocess,re
P=collect.ROOT;x=json.load(open(sorted(P.glob('candidate-poll-*.json'))[-1]));p=next(p for p in x['candidates'] if p['pr']==465);c=next(c for c in p['checks'] if c['name']=='hardcoded-list-audit');print(c['url']);job=re.search(r'/job/(\d+)',c['url']).group(1)
r=subprocess.run(['/usr/local/bin/gh','run','view','--repo','diegueins680/tdf-app','--job',job,'--log'],env=collect.ENV,capture_output=True,text=True);(P/'events-current-main-hosted-catalog-failure.log').write_text(r.stdout+r.stderr);assert r.returncode==0,r.stderr
print(r.stdout[-14000:])
