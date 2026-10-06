import collect,subprocess,json,re,datetime
P=collect.ROOT
job=json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+'/actions/jobs/111332783593'],env=collect.ENV,text=True))
assert job['head_sha']=='b07f67c3ebe2022d5f70e94578944b6401616696'
assert job['status']=='completed' and job['conclusion']=='success'
assert all(s['status']=='completed' and s['conclusion'] in ('success','skipped') for s in job['steps'])
log=subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+'/actions/jobs/111332783593/logs'],env=collect.ENV,text=True)
assert '3589 examples, 0 failures' in log
assert '1141 examples, 0 failures' in log
assert '110 examples, 0 failures' in log
(P/'events-main-backend-b07f67c3.log').write_text(log)
(P/'events-main-backend-b07f67c3-verified.json').write_text(json.dumps({'verified_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'job':job,'verified_example_counts':[3589,1141,110],'all_prescribed_steps_passed':True},indent=2)+'\n')
print('Verified main backend: 3589 Hspec, 1141 social HTTP, 110 event HTTP examples; all prescribed stages passed')
