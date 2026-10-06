import collect,subprocess,json,datetime,time,urllib.parse
P=collect.ROOT;cwd=P/'discovery';repo=collect.REPO;branch='audit/event-discovery-integration-20260928'
def api(path,*a):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*a],env=collect.ENV,text=True))
def ledger(**entry):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**entry})+'\n')
sources={460:'648f2d6dc69080d4438ae2a25309497012ded2d1',463:'a125a347cacb8d941a515171e2a81d4c92f2c2d2'}
before={str(n):api(repo+'/pulls/'+str(n)) for n in sources}
for n,sha in sources.items():assert before[str(n)]['state']=='open' and before[str(n)]['head']['sha']==sha
main=api(repo+'/git/ref/heads/main')['object']['sha'];assert main=='7e7106b36e7ac427711c2f64af650527d619f9ce'
branches=api(repo+'/git/matching-refs/heads/'+urllib.parse.quote(branch,safe=''));assert not branches
assert not api(repo+'/pulls?state=all&head=diegueins680:'+branch)
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()
for ancestor in [main,*sources.values()]:subprocess.run(['git','merge-base','--is-ancestor',ancestor,sha],cwd=cwd,check=True)
assert '28 examples, 0 failures' in (P/'discovery-focused-tests.log').read_text()
assert 'rollback/reapply passed.' in (P/'discovery-boundary-db.log').read_text()
assert 'ℹ pass 82' in (P/'discovery-release-tests.log').read_text()
(P/'discovery-publish-before.json').write_text(json.dumps({'main':main,'sources':before},indent=2))
subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
ledger(action='pushed_standalone_discovery_integration',sha=sha,branch=branch,sources=sources,base_sha=main)
# Recheck source/main state before creating the draft; a normal branch push is recoverable.
assert api(repo+'/git/ref/heads/main')['object']['sha']==main
for n,expected in sources.items():assert api(repo+'/pulls/'+str(n))['head']['sha']==expected
assert not api(repo+'/pulls?state=open&head=diegueins680:'+branch)
body='''Preserve unconfirmed event end times and enforce shared pilot capacity and separate publication authority, integrating #460 and repaired #463 into current main. Source disablement now prevents absence reconciliation or false success, including empty feeds; completion commits atomically in the importer’s source → pilot → event lock order.

GitHub rejected an ordinary merge of #460 because it belongs to native stack466, whose partial-merge API rebases descendants. This standalone normal merge preserves every source commit and the production migration introduction ancestry. It requires its own current-head review and main-branch CI. The original stack and concurrent #464 remain unchanged; no original PR is superseded until this replacement merges.

Conflict resolution retains current records-ingestion checks alongside event-boundary checks, preserves all 115 existing migration entries and appends the original shared-boundary migration as entry116. Both release-schema gates remain. The catalog decision and specification inventory reflect that exact combined registry. Contributor attribution, including PR464’s rescued persistence-failure propagation, remains in the source history.

Executed validation: 28 discovery examples; real PostgreSQL shared-capacity/deduplication/suppression/concurrency/authority/revocation/rollback and disabled-source/atomic-completion regressions; 82 production-release tests; all 116 migration introductions verified as ancestors; catalog and specification gates. Source repair’s actual Cron dependency graph also compiles. The full integrated backend build, suite, production-schema rehearsal and current-head hosted CI remain pending, so this PR is draft. No deployment or feature activation is requested.
'''
pr=api(repo+'/pulls','--method','POST','-f','title=Integrate event discovery and shared pilot repairs while preserving history','-f','head='+branch,'-f','base=main','-f','body='+body,'-F','draft=true')
verified=api(repo+'/pulls/'+str(pr['number']));assert verified['state']=='open' and verified['draft'] and verified['head']['sha']==sha and verified['base']['ref']=='main'
(P/'discovery-repair-pr.json').write_text(json.dumps(verified,indent=2))
ledger(action='created_draft_replacement',pr=verified['number'],sha=sha,url=verified['html_url'],sources=[460,463],native_stack=verified.get('stack'))
print('Verified draft replacement',verified['html_url'],'stack',verified.get('stack'))
