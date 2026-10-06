import collect,subprocess,json,datetime,time
P=collect.ROOT;W=P/'events-editorial-repair-20261003';v=json.loads((P/'events-notification-merge-validation.json').read_text());old=v['reviewed_remote_head'];head=v['local_candidate'];base=v['base']
def api(path):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+path],env=collect.ENV,text=True))
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=W,text=True).strip()==head
assert not subprocess.check_output(['git','status','--porcelain','--untracked-files=no'],cwd=W,text=True)
p=api('/pulls/465');assert p['state']=='open' and p['head']['sha']==old and p['base']['ref']=='main'
assert api('/git/ref/heads/main')['object']['sha']==base
assert api('/git/ref/heads/'+p['head']['ref'])['object']['sha']==old
(P/'events-notification-before-push.json').write_text(json.dumps(p,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+p['head']['ref']],cwd=W,env=collect.ENV,check=True)
assert api('/git/ref/heads/'+p['head']['ref'])['object']['sha']==head
for _ in range(10):
 after=api('/pulls/465')
 if after['head']['sha']==head:break
 time.sleep(1)
assert after['head']['sha']==head and after['state']=='open'
(P/'events-notification-push-verified.json').write_text(json.dumps(after,indent=2))
v['local_only_not_pushed']=False;v['published_at']=datetime.datetime.now(datetime.timezone.utc).isoformat();(P/'events-notification-merge-validation.json').write_text(json.dumps(v,indent=2))
with (P/'mutations.jsonl').open('a') as out:out.write(json.dumps({'time':v['published_at'],'action':'pushed_repair_followup','pr':465,'old_sha':old,'sha':head,'url':after['html_url'],'evidence':'events-notification-push-verified.json','reason':'normal current-main reconciliation; exact reviewed documentation and generated hash only'})+'\n')
print('Verified normal current-main reconciliation',head)
