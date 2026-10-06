import collect,subprocess,json,datetime,time
p=collect.ROOT;cwd=p/'events'
def api(path):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path],env=collect.ENV,text=True))
i=api(collect.REPO+'/pulls/465');old='a28e1858341bbf87b4c6080144bf38e9aedfc4d2'
assert i['head']['sha']==old and i['state']=='open' and i['base']['ref']=='main'
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();assert sha!=old
subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
subprocess.run(['git','push','origin','HEAD:refs/heads/audit/event-stack-consolidation-20260928'],cwd=cwd,env=collect.ENV,check=True)
assert api(collect.REPO+'/git/ref/heads/audit/event-stack-consolidation-20260928')['object']['sha']==sha
for attempt in range(6):
 a=api(collect.REPO+'/pulls/465')
 if a['head']['sha']==sha:break
 time.sleep(2)
assert a['head']['sha']==sha and a['state']=='open' and a['base']['ref']=='main'
(p/'events-repair-pr.json').write_text(json.dumps(a,indent=2))
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'pushed_repair_followup','pr':465,'old_sha':old,'sha':sha,'url':a['html_url']})+'\n')
print('Verified normal follow-up push',sha)
