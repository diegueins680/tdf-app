import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO;cwd=P/'ingestion';old='6ba185a697b3b6b6983ebb48e0ab6454f716cf23'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**entry):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**entry})+'\n')
before=api(repo+'/pulls/463');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='fix/event-confirmed-end-20260920'
assert api(repo+'/git/ref/heads/codex/event-ingestion-boundaries-20260927')['object']['sha']==old
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert '28 examples, 0 failures' in (P/'pr463-discovery-tests.log').read_text()
assert 'Pilot status count passed' in (P/'pr463-shared-count-db.log').read_text()
assert 'rollback/reapply passed.' in (P/'pr463-shared-count-db.log').read_text()
assert '[24 of 24] Compiling TDF.Server.EventResearch' in (P/'pr463-handler-typecheck.log').read_text()
assert 'ℹ pass 82' in (P/'pr463-release-tests.log').read_text()
(P/'pr463-fix-before.json').write_text(json.dumps(before,indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/codex/event-ingestion-boundaries-20260927'],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/codex/event-ingestion-boundaries-20260927')['object']['sha']==sha
for attempt in range(6):
 after=api(repo+'/pulls/463')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['state']=='open' and after['head']['sha']==sha and after['base']['ref']==before['base']['ref']
(P/'pr463-fix-after.json').write_text(json.dumps(after,indent=2))
ledger(action='pushed_review_fix',pr=463,old_sha=old,sha=sha,url=after['html_url'],validation='28 focused examples, real PostgreSQL capacity/deduplication/suppression/concurrency/rollback, actual handler/dependencies compiled, 82 release tests and all 115 introduction ancestors verified; no history rewrite.')
print('Verified normal PR463 repair push',sha)
