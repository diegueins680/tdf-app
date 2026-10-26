from pathlib import Path
import subprocess,os,json,datetime,hashlib
root=Path('/private/tmp/tdf-ledger-event-identity-20261025');out=Path('/private/tmp/tdf-ledger-event-identity-differential-v4-results-20261026');out.mkdir(mode=0o700)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']}
env['PATH']='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-release-verification-venv/bin:'+env['PATH'];env['PGPASSFILE']=str(out/'no-pgpass');env['CI']='false'
port='57440';db='tdf_ledger_event_identity_differential_v4_20261026_test';url='postgresql://macpro@127.0.0.1:'+port+'/'+db
for k in ['TDF_MUSIC_REHEARSAL_DATABASE_URL','TDF_LEDGER_POSTING_DATABASE_URL']:env[k]=url
cluster=out/'cluster';sock=out/'socket';sock.mkdir(mode=0o700)
report={'sourceAtStart':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'workingDiffSha256':hashlib.sha256(subprocess.check_output(['git','diff','HEAD'],cwd=root)).hexdigest(),'manifestEntries':220,'productionOperations':False,'steps':[],'cleanup':{}}
report['sourceSha256']={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [root/'scripts/production-migrations.json', root/'scripts/lib/production-release.mjs', root/'scripts/__tests__/ledger-event-identity.mjs', root/'tdf-hq/sql/2026-10-25_ledger_posted_header.sql', root/'scripts/test-ledger-posting.sh', root/'scripts/__tests__/ledger-posting-namespace.py', root/'scripts/__tests__/ledger-posting-upgrade.py', root/'scripts/__tests__/ledger-posting-snapshot.py', *[root/e['path'] for e in json.loads((root/'scripts/production-migrations.json').read_text())['migrations']]]}
report['driverSha256']=hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
report['versions']={'python':subprocess.check_output(['python3','--version'],env=env,text=True).strip(),'node':subprocess.check_output(['node','--version'],env=env,text=True).strip(),'psql':subprocess.check_output(['psql','--version'],env=env,text=True).strip()}
def save():(out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(label,args,input=None,expected=None,cwd=None):
 r=subprocess.run(args,input=input,cwd=cwd or root,env=env,text=True,capture_output=True,timeout=240)
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
 parent=Path('/private/tmp/tdf-ledger-final-main-20261025')
 assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=parent,text=True).strip()=='fa5a34792cee86cdf309a81a362de52032aa0b9c'
 report['baselineRendererSource']='fa5a34792cee86cdf309a81a362de52032aa0b9c'
 run('full220-apply-replay',['node','scripts/test-music-integration-rehearsal.mjs'],cwd=parent)
 assert run('manifest-cardinality',psql,input='SELECT count(*) FROM public.tdf_schema_migration;')=='220'
 report['regressionSha256']=hashlib.sha256((root/'scripts/__tests__/ledger-event-identity.mjs').read_bytes()).hexdigest();save()
 run('posted-identity-regression-exposes-baseline',['node','scripts/__tests__/ledger-event-identity.mjs'],expected='posted ledger header changed: posted/transaction_type')
 run('install-additive-header-guard',psql+['-f','tdf-hq/sql/2026-10-25_ledger_posted_header.sql'])
 run('header-regression-and-guard-controls',['node','scripts/__tests__/ledger-event-identity.mjs','--negative-controls'])
 report['counterexample']='An ordinary non-owner with SELECT/UPDATE on ledger headers and no entry UPDATE/public CREATE changes the event key of a real posted balanced transaction';save()

finally:
 try:
  if created:run('drop',['dropdb','-h','127.0.0.1','-p',port,'-U','macpro',db]);report['cleanup']['ownedDatabaseRemoved']=True
 finally:
  if started:run('stop',['pg_ctl','-D',str(cluster),'-m','fast','-w','stop']);report['cleanup']['ownedClusterStopped']=True
  report['sourceHashesUnchanged']=all(hashlib.sha256((root/p).read_bytes()).hexdigest()==h for p,h in report['sourceSha256'].items())
  report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
