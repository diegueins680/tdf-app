import collect,json,subprocess,time,datetime
P=collect.ROOT;cwd=P/'events/tdf-mobile';repo='repos/diegueins680/TDF-mobile';old='2ec145f17d76938ef7a8c042c949e2ad64222c41';branch='audit/event-operations-client-20260928'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def ledger(**kw):
 with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'repository':'TDF-mobile',**kw})+'\n')
assert all(r['exit_code']==0 for r in json.load(open(P/'mobile-cutover-validation.json')))
assert len(json.load(open(P/'mobile-cutover-config.json')))==5
sha=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip();subprocess.run(['git','merge-base','--is-ancestor',old,sha],cwd=cwd,check=True)
assert not subprocess.check_output(['git','diff',old,sha,'--','src/api/generated/types.ts'],cwd=cwd,text=True)
before=api(repo+'/pulls/115');assert before['state']=='open' and before['head']['sha']==old and before['base']['ref']=='main' and before['base']['sha']=='2a0e5a99535d9ef199a3e3464a660192f882f72b'
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==old
(P/'mobile-cutover-before.json').write_text(json.dumps(before,indent=2));subprocess.run(['git','push','origin','HEAD:refs/heads/'+branch],cwd=cwd,env=collect.ENV,check=True)
assert api(repo+'/git/ref/heads/'+branch)['object']['sha']==sha
for _ in range(10):
 after=api(repo+'/pulls/115')
 if after['head']['sha']==sha:break
 time.sleep(2)
assert after['head']['sha']==sha and after['state']=='open';(P/'mobile-cutover-after.json').write_text(json.dumps(after,indent=2));ledger(action='pushed_mobile_canonical_api_fix',pr=115,old_sha=old,sha=sha,url=after['html_url'],validation='Full mobile test suite, release asset/lint/typecheck/config validation, Python release-contract tests and five resolved Expo profile/override cases pass. Android/iOS artifact checks retain signing/identity constraints and require the canonical API. No native build/publication/deployment.')
before=api(repo+'/pulls/115');assert before['state']=='open' and before['head']['sha']==sha
body='''The event operations client needs the canonical scoped task/revision/RACI/completion API types, and preview/production builds must use the current backend after the web-first cutover. Regenerate the client from the parent repository contract and retarget release API/upload configuration to `https://api.tdfrecords.net`. Preserve development localhost defaults, explicit overrides, app identities, signing checks and existing dependencies. Android/iOS artifact verification requires the new embedded API; no check is disabled.

Companion: https://github.com/diegueins680/tdf-app/pull/465, which will pin this exact normal commit. Generated types are unchanged by the hosting follow-up, and all original history remains.

Validation at `'''+sha+'''`: full Jest suite passes (85 suites / 510 tests); release assets, lint, typecheck and public Expo configuration pass; Python release-contract tests pass; actual Expo configuration passes production fallback, injected preview and production profiles, development localhost and an explicit isolated override. The original dependency findings remain unchanged. No native build, store submission, deployment or physical login/upload validation was performed. Existing installed binaries require a separately reviewed release; physical authentication/upload checks remain release gates. Independent current-head review and hosted checks are pending.
'''
api(repo+'/pulls/115','--method','PATCH','-f','title=Align event client contracts and release API with the current backend','-f','body='+body)
after=api(repo+'/pulls/115');assert after['head']['sha']==sha and after['body']==body;(P/'mobile-cutover-description-after.json').write_text(json.dumps(after,indent=2));ledger(action='updated_validation_description',pr=115,sha=sha,url=after['html_url']);print('Verified mobile current API push',sha)
