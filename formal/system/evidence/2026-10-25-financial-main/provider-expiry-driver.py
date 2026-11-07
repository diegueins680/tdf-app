from pathlib import Path
import subprocess,os,json,datetime,hashlib
root=Path('/private/tmp/tdf-ledger-startup-namespace-20261025');out=Path('/private/tmp/tdf-ledger-provider-expiry-namespace-results-20261025');out.mkdir(mode=0o700)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']}
env['PATH']='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-release-verification-venv/bin:'+env['PATH'];env['PGPASSFILE']=str(out/'no-pgpass');env['CI']='false'
port='55487';db='tdf_provider_retry_test';url='postgresql://macpro@127.0.0.1:'+port+'/'+db
env['STACK_ROOT']='/Users/macpro/.stack';env['TDF_PROVIDER_RETRY_DATABASE_URL']=url
for k in ['TDF_MUSIC_REHEARSAL_DATABASE_URL','TDF_LEDGER_POSTING_DATABASE_URL']:env[k]=url
cluster=out/'cluster';sock=out/'socket';sock.mkdir(mode=0o700)
report={'sourceAtStart':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'workingDiffSha256':hashlib.sha256(subprocess.check_output(['git','diff','HEAD'],cwd=root)).hexdigest(),'sourceRegistryEntries':220,'productionOperations':False,'steps':[],'cleanup':{}}
report['sourceSha256']={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [root/'scripts/production-migrations.json', root/'scripts/test-ledger-posting.sh', root/'scripts/__tests__/ledger-posting-namespace.py', root/'scripts/__tests__/ledger-posting-upgrade.py', root/'scripts/__tests__/ledger-posting-snapshot.py', *[root/e['path'] for e in json.loads((root/'scripts/production-migrations.json').read_text())['migrations']]]}
report['driverSha256']=hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
report['versions']={'python':subprocess.check_output(['python3','--version'],env=env,text=True).strip(),'node':subprocess.check_output(['node','--version'],env=env,text=True).strip(),'psql':subprocess.check_output(['psql','--version'],env=env,text=True).strip()}
def save():(out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(label,args,input=None,expected=None):
 r=subprocess.run(args,input=input,cwd=root,env=env,text=True,capture_output=True,timeout=600)
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
 fixture=root/'scripts/test-provider-retry-runtime.sh';source=fixture.read_text();marker='# Match quality-backend.sh optimization settings'
 assert source.count(marker)==1
 prefix=source.split(marker)[0]
 report['fixturePrelude']={'scriptSha256':hashlib.sha256(source.encode()).hexdigest(),'executedPrefixSha256':hashlib.sha256(prefix.encode()).hexdigest(),'scope':'Original setup prefix only; full provider retry Hspec suite is not claimed'};save()
 run('canonical-provider-fixture-prelude',['sh','-c',prefix,str(fixture)])
 settings=run('installed-namespace-settings',psql,input="SELECT count(*) FROM pg_proc WHERE oid IN ('public.commerce_validate_ledger_posting()'::regprocedure,'public.commerce_protect_posted_ledger()'::regprocedure) AND proconfig @> ARRAY['search_path=pg_catalog, public, pg_temp']::text[];")
 assert settings=='2'
 run('actual-store-expiry-with-namespace',['/Users/macpro/.codex/worktrees/874a/tdf-app/tmp/payment-intent-expiry/verification-env/bin/python3','scripts/verify-payment-intent-expiry.py','--output',str(out/'expiry.json')])
 expiry=json.loads((out/'expiry.json').read_text());assert expiry['passed'] and expiry['sourcesUnchanged'] and expiry['revision']==report['sourceAtStart']
 report['qualification']='Canonical provider fixture includes both namespace settings; actual intent store and three clock mutation controls pass with the repaired ledger guards installed';save()

finally:
 try:
  if created:run('drop',['dropdb','-h','127.0.0.1','-p',port,'-U','macpro',db]);report['cleanup']['ownedDatabaseRemoved']=True
 finally:
  if started:run('stop',['pg_ctl','-D',str(cluster),'-m','fast','-w','stop']);report['cleanup']['ownedClusterStopped']=True
  report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
