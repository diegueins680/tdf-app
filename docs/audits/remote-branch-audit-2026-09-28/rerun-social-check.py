import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;sha='cda2c5f7790126f35bd7e09bed35b52b5a0012ef';jobid=108997922873;runid=36442970971
def api(path):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path],env=collect.ENV,text=True))
pr=api(repo+'/pulls/469');assert pr['state']=='open' and pr['head']['sha']==sha and pr['base']['ref']=='main'
run=api(repo+'/actions/runs/'+str(runid));job=api(repo+'/actions/jobs/'+str(jobid))
assert run['head_sha']==sha and job['head_sha']==sha and job['name']=='social-client' and job['conclusion']=='failure' and run['run_attempt']==1
assert 'PASS: local component journey' in (P/'discovery-social-browser-recheck.log').read_text()
(P/'discovery-social-rerun-before.json').write_text(json.dumps({'run':run,'job':job},indent=2))
subprocess.run(['/usr/local/bin/gh','api',repo+'/actions/jobs/'+str(jobid)+'/rerun','--method','POST'],env=collect.ENV,check=True)
for _ in range(10):
 after=api(repo+'/actions/runs/'+str(runid))
 if after['run_attempt']==2:break
 time.sleep(2)
assert after['run_attempt']==2 and after['head_sha']==sha
(P/'discovery-social-rerun-after.json').write_text(json.dumps(after,indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'reran_failed_test_job_once','pr':469,'sha':sha,'run':runid,'job':jobid,'attempt':2,'url':after['html_url'],'reason':'Unchanged local synthetic browser assertions and accessibility recheck passed; only social-client test job rerun. No deployment, timeout/assertion change or Datadog bypass.'})+'\n')
print('Verified unchanged test job rerun attempt2',after['html_url'])
