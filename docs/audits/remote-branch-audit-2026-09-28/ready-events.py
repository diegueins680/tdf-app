import collect,subprocess,json,datetime
P=collect.ROOT;sha='afd86265f90378000cb3b5723f3406ced9793bbc';repo=collect.REPO
def api(path,*a):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*a],env=collect.ENV,text=True))
def record(action,info):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':action,'pr':465,'sha':sha,'url':info['html_url']})+'\n')
i=api(repo+'/pulls/465');assert i['state']=='open' and i['draft'] and i['head']['sha']==sha and i['base']['ref']=='main'
c=json.loads(subprocess.check_output(['/usr/local/bin/gh','pr','checks','465','--repo','diegueins680/tdf-app','--json','name,state,link'],env=collect.ENV,text=True));assert c and all(x['state']=='SUCCESS' for x in c)
(P/'events-hosted-complete.json').write_text(json.dumps(c,indent=2))
subprocess.run(['/usr/local/bin/gh','pr','ready','465','--repo','diegueins680/tdf-app'],env=collect.ENV,check=True)
a=api(repo+'/pulls/465');assert not a['draft'] and a['head']['sha']==sha
record('marked_ready_for_review',a)
before=api(repo+'/pulls/465');assert before['head']['sha']==sha and before['state']=='open' and before['body']==i['body']
old='Current-head hosted checks are running after the final HTTP/browser repairs. Keep this draft until complete; independent approval, resolved blocking discussions and applicable checks remain required.'
assert old in before['body']
body=before['body'].replace(old,'All 31 current-head hosted checks now pass, including the full backend and runtime job. This PR is ready for integration review. Independent approval, resolved blocking discussions and applicable checks must be rechecked before merge; mobile companion PR115 still awaits independent review and integration.')
api(repo+'/pulls/465','--method','PATCH','-f','body='+body)
after=api(repo+'/pulls/465');assert after['body']==body and after['head']['sha']==sha and not after['draft']
(P/'events-repair-pr.json').write_text(json.dumps(after,indent=2));record('updated_validation_description',after)
print('Verified PR465 ready, all current-head checks passed; mobile review dependency remains.')
