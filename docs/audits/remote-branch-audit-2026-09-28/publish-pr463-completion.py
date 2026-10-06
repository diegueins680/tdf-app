import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'ingestion';old='697bd58b5ea1c8134be490cce588b65e9a08dad8'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**entry):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**entry})+'\n')
assert (P/'pr463-cron-verified-exit.txt').read_text().strip()=='0'
assert '28 examples, 0 failures' in (P/'pr463-discovery-tests-3.log').read_text()
assert 'Source completion passed disabled empty-feed rejection, atomic rollback and enabled completion.' in (P/'pr463-source-completion-db-3.log').read_text()
assert 'rollback/reapply passed.' in (P/'pr463-source-completion-db-3.log').read_text()
before=api(repo+'/pulls/463');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='fix/event-confirmed-end-20260920'
assert api(repo+'/git/ref/heads/codex/event-ingestion-boundaries-20260927')['object']['sha']==old
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();assert sha!=old
subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
(P/'pr463-completion-before.json').write_text(json.dumps(before,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/codex/event-ingestion-boundaries-20260927'],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/codex/event-ingestion-boundaries-20260927')['object']['sha']==sha
for attempt in range(10):
 after=api(repo+'/pulls/463')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['state']=='open' and after['head']['sha']==sha and after['base']['ref']==before['base']['ref']
(P/'pr463-completion-after.json').write_text(json.dumps(after,indent=2))
ledger(action='pushed_source_completion_fix',pr=463,old_sha=old,sha=sha,url=after['html_url'],rescued_from='PR464 commit 5068a0d2d5e1c4888e57cf5b56f0365cb0a718d6; contributor attribution retained',validation='28 focused examples; actual PostgreSQL disabled empty-feed rejection, atomic rollback and enabled completion; mixed count/race/authority/revocation/rollback regression; full Cron dependency typecheck; catalog and specification gates. No rewrite.')
print('Verified normal PR463 source-completion push',sha)
