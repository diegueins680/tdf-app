import collect,subprocess,json,datetime,time
P=collect.ROOT;W=P/'events-editorial-repair-20261003';old='34ca75d3d92d2a6def22e63ddd47baa96edca878';base='f8925e339447cb593f7cce5d86403d1396aaf5a5'
def api(path):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+path],env=collect.ENV,text=True))
def run(*a):return subprocess.check_output(list(a),cwd=W,env=collect.ENV,text=True).strip()
assert json.loads((P/'task-completion-native-fixed.json').read_text())['exit_code']==0
assert 'Task completion PASS:' in (P/'task-completion-native-fixed.log').read_text()
assert run('git','rev-parse','HEAD')==old
pr=api('/pulls/465');assert pr['head']['sha']==old and pr['state']=='open' and pr['base']['sha']==base
assert api('/git/ref/heads/main')['object']['sha']==base
assert run('git','diff','--name-only')=='scripts/test-event-task-completion-migration.sh'
(P/'completion-poll-before-commit.json').write_text(json.dumps(pr,indent=2))
run('git','diff','--check');run('git','add','scripts/test-event-task-completion-migration.sh');print(run('git','commit','-m','test(events): allow the full permission expiry window'),flush=True)
head=run('git','rev-parse','HEAD');pr=api('/pulls/465');assert pr['head']['sha']==old and pr['state']=='open' and pr['base']['sha']==base
assert api('/git/ref/heads/'+pr['head']['ref'])['object']['sha']==old
run('git','push','origin','HEAD:refs/heads/'+pr['head']['ref'])
assert api('/git/ref/heads/'+pr['head']['ref'])['object']['sha']==head
for _ in range(10):
 after=api('/pulls/465')
 if after['head']['sha']==head:break
 time.sleep(1)
assert after['head']['sha']==head and after['state']=='open'
(P/'completion-poll-push-verified.json').write_text(json.dumps(after,indent=2))
with (P/'mutations.jsonl').open('a') as out:out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'pushed_repair_followup','pr':465,'old_sha':old,'sha':head,'url':after['html_url'],'evidence':'completion-poll-push-verified.json'})+'\n')
print('VERIFIED',head)
