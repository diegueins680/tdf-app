import collect,subprocess,json,datetime
p=collect.ROOT;repo='repos/diegueins680/TDF-mobile';sha='2ec145f17d76938ef7a8c042c949e2ad64222c41'
def api(path,*a):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*a],env=collect.ENV,text=True))
i=api(repo+'/pulls/115');assert i['head']['sha']==sha and i['state']=='open' and i['draft'] and i['base']['ref']=='main'
checks=api(repo+'/commits/'+sha+'/check-runs?per_page=100')['check_runs'];assert checks and all(c['status']=='completed' and c['conclusion']=='success' for c in checks)
subprocess.run(['/usr/local/bin/gh','pr','ready','115','--repo','diegueins680/TDF-mobile'],env=collect.ENV,check=True)
a=api(repo+'/pulls/115');assert not a['draft'] and a['head']['sha']==sha
(p/'mobile-repair-pr.json').write_text(json.dumps(a,indent=2))
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile','action':'marked_ready_for_review','pr':115,'sha':sha,'url':a['html_url'],'validation':'85 suites/510 tests, typecheck, lint, release:check, exact-head validate and synthetic checks passed; not merged and no review bypass.'})+'\n')
print('Verified mobile115 ready for independent review; not merged.')
