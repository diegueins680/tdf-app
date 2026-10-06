import collect,json,subprocess,datetime
p=collect.ROOT
def api(path,*args):
 return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
i=api(collect.REPO+'/pulls/460');m=api(collect.REPO+'/branches/main')
assert i['state']=='open' and not i['merged']
assert i['head']['sha']=='648f2d6dc69080d4438ae2a25309497012ded2d1'
assert i['base']['ref']=='fix/event-discovery-schedule-20260918'
assert m['commit']['sha']=='cc244b1f86603055997b51379b297baebfd3e7ce'
parent=api(collect.REPO+'/pulls/456');assert parent['merged'] and parent['base']['ref']=='main'
(p/'retarget-460-before.json').write_text(json.dumps(i,indent=2))
api(collect.REPO+'/pulls/460','--method','PATCH','-f','base=main')
after=api(collect.REPO+'/pulls/460');assert after['base']['ref']=='main' and after['head']['sha']==i['head']['sha'] and after['state']=='open'
(p/'retarget-460-after.json').write_text(json.dumps(after,indent=2))
with open(p/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'retargeted','pr':460,'sha':i['head']['sha'],'old_base':i['base']['ref'],'base':'main','url':after['html_url'],'reason':'Original target PR456 already merged; conflict-free against current main; retain required approval and all checks.'})+'\n')
print('Verified PR460 now targets main; head unchanged; left open awaiting review/checks.')
