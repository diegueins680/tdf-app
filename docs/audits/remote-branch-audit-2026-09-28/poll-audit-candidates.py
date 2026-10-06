import collect,json,subprocess,datetime,concurrent.futures
P=collect.ROOT
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def read(spec):
 repo,n=spec;x=api('repos/'+repo+'/pulls/'+str(n));sha=x['head']['sha'];pages=api('repos/'+repo+'/commits/'+sha+'/check-runs?per_page=100','--paginate','--slurp');reviews=api('repos/'+repo+'/pulls/'+str(n)+'/reviews?per_page=100','--paginate','--slurp')
 return {'repo':repo,'pr':n,'head':sha,'state':x['state'],'merged':x['merged'],'mergeable':x['mergeable'],'mergeable_state':x['mergeable_state'],'base':x['base']['sha'],'checks':[{'name':c['name'],'status':c['status'],'conclusion':c['conclusion'],'url':c['html_url']} for pg in pages for c in pg['check_runs']],'reviews':[{'state':r['state'],'sha':r['commit_id'],'by':r['user']['login']} for pg in reviews for r in pg]}
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as ex:items=list(ex.map(read,[('diegueins680/tdf-app',465),('diegueins680/tdf-app',468),('diegueins680/TDF-mobile',115),('diegueins680/tdf-app',475)]))
x={'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'candidates':items};stamp=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%SZ');(P/('candidate-poll-'+stamp+'.json')).write_text(json.dumps(x,indent=2)+'\n')
for p in items:
 print(p['repo'],p['pr'],p['head'],p['state'],p['mergeable_state'],'reviews',p['reviews'])
 print('checks',[(c['name'],c['conclusion'] or c['status']) for c in p['checks']])
