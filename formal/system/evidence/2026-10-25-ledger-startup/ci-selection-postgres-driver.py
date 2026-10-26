from pathlib import Path
import subprocess,os,json,datetime,hashlib
root=Path('/private/tmp/tdf-ledger-startup-namespace-20261025');out=Path('/private/tmp/tdf-ledger-startup-scope-qualified-results-20261025');out.mkdir(mode=0o700)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']}
env['PATH']='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-release-verification-venv/bin:'+env['PATH'];env['PGPASSFILE']=str(out/'no-pgpass');env['CI']='false'
port='55486';db='tdf_ledger_startup_scope_20261025_test';url='postgresql://macpro@127.0.0.1:'+port+'/'+db
for k in ['TDF_MUSIC_REHEARSAL_DATABASE_URL','TDF_LEDGER_POSTING_DATABASE_URL']:env[k]=url
cluster=out/'cluster';sock=out/'socket';sock.mkdir(mode=0o700)
report={'sourceAtStart':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'workingDiffSha256':hashlib.sha256(subprocess.check_output(['git','diff','HEAD'],cwd=root)).hexdigest(),'manifestEntries':220,'productionOperations':False,'steps':[],'cleanup':{}}
report['sourceSha256']={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [root/'scripts/production-migrations.json', root/'scripts/test-ledger-posting.sh', root/'scripts/__tests__/ledger-posting-namespace.py', root/'scripts/__tests__/ledger-posting-upgrade.py', root/'scripts/__tests__/ledger-posting-snapshot.py', *[root/e['path'] for e in json.loads((root/'scripts/production-migrations.json').read_text())['migrations']]]}
report['driverSha256']=hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
report['versions']={'python':subprocess.check_output(['python3','--version'],env=env,text=True).strip(),'node':subprocess.check_output(['node','--version'],env=env,text=True).strip(),'psql':subprocess.check_output(['psql','--version'],env=env,text=True).strip()}
def save():(out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(label,args,input=None,expected=None):
 r=subprocess.run(args,input=input,cwd=root,env=env,text=True,capture_output=True,timeout=240)
 (out/(label+'.log')).write_text(r.stdout+r.stderr)
 report['steps'].append({'name':label,'exitCode':r.returncode,'expectedFailure':expected});save()
 assert (r.returncode!=0 and expected in r.stderr) if expected else r.returncode==0,(label,r.stderr[-2500:])
 print(label,'EXPECTED REJECTION' if expected else 'PASS',flush=True);return r.stdout.strip()
psql=['psql','-XqAt','-v','ON_ERROR_STOP=1','-d',url]
started=created=False
try:
 assert subprocess.run(['lsof','-tiTCP:'+port,'-sTCP:LISTEN'],capture_output=True).returncode!=0
 run('init',['initdb','-D',str(cluster),'--username=macpro','--encoding=UTF8','--locale=C','--auth=trust'])
 run('start',['pg_ctl','-D',str(cluster),'-l',str(out/'postgres.log'),'-o','-p '+port+' -h 127.0.0.1 -k '+str(sock),'start']);started=True
 run('create',['createdb','-h','127.0.0.1','-p',port,'-U','macpro','--template=template0',db]);created=True
 run('full220-apply-replay',['node','scripts/test-music-integration-rehearsal.mjs'])
 assert run('manifest-cardinality',psql,input='SELECT count(*) FROM public.tdf_schema_migration;')=='220'
 report['sourceSha256'].update({str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [root/'scripts/lib/production-release.mjs',root/'scripts/__tests__/ledger-startup-namespace.mjs',root/'scripts/test-provider-retry-runtime.sh',root/'scripts/ci-change-scope.mjs',root/'scripts/__tests__/ci-change-scope.test.mjs',root/'.github/workflows/ci.yml']});save()
 run('path-selector-regressions',['node','--test','scripts/__tests__/ci-change-scope.test.mjs'])
 scope=run('actual-ci-selection',['node','scripts/ci-change-scope.mjs','--base','f9ee2199273471a12cfb95da95bc5503fe51ac79','--head','HEAD'])
 assert json.loads(scope)['scopes']['backend'] is True
 run('repaired-startup-verifier-with-controls',['node','scripts/__tests__/ledger-startup-namespace.mjs'])
 # Execute the same regression against a frozen original module in an external
 # mirror. This proves failure is the deliberately removed guard, not execution.
 control=out/'original-module-control';(control/'scripts/lib').mkdir(parents=True);(control/'scripts/__tests__').mkdir()
 old=subprocess.check_output(['git','show','f9ee2199273471a12cfb95da95bc5503fe51ac79:scripts/lib/production-release.mjs'],cwd=root)
 (control/'scripts/lib/production-release.mjs').write_bytes(old)
 for path in ['scripts/lib/disposable-postgres-url.mjs','scripts/__tests__/ledger-startup-namespace.mjs']:(control/path).write_bytes((root/path).read_bytes())
 report['originalVerifier']={'revision':'f9ee2199273471a12cfb95da95bc5503fe51ac79','sha256':hashlib.sha256(old).hexdigest()};save()
 run('original-verifier-regression-rejects',['node',str(control/'scripts/__tests__/ledger-startup-namespace.mjs')],expected='startup accepted ALTER FUNCTION public.commerce_validate_ledger_posting() RESET search_path;')
 run('guards-after-original-control',['python3','scripts/__tests__/ledger-posting-namespace.py','--negative-controls'])
 run('repaired-startup-final-restoration',['node','scripts/__tests__/ledger-startup-namespace.mjs'])
 assert all(hashlib.sha256((root/p).read_bytes()).hexdigest()==h for p,h in report['sourceSha256'].items())
 report['qualification']='Full220 startup verifier rejects ten namespace/invoker/signature mutations, accepts two extra GUC cases, restores exact functions and receipts; frozen original verifier fails the new regression';save()

finally:
 try:
  if created:run('drop',['dropdb','-h','127.0.0.1','-p',port,'-U','macpro',db]);report['cleanup']['ownedDatabaseRemoved']=True
 finally:
  if started:run('stop',['pg_ctl','-D',str(cluster),'-m','fast','-w','stop']);report['cleanup']['ownedClusterStopped']=True
  report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
