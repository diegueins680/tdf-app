from pathlib import Path
import subprocess,os,json,datetime,hashlib,re
root=Path('/private/tmp/tdf-ledger-protected-main-20261025');out=Path('/private/tmp/tdf-ledger-combined-native-v3-results-20261025');out.mkdir(mode=0o700)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']}
env['PATH']='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-release-verification-venv/bin:'+env['PATH'];env['PGPASSFILE']=str(out/'no-pgpass');env['CI']='false'
port='55489';db='tdf_ledger_combined_native_v3_20261025_test';url='postgresql://127.0.0.1/'+db
env['PGPORT']=port;env['PGUSER']='macpro'
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
 assert run('actual-owned-target-before-schema',psql,input="SELECT host(inet_server_addr()) || '|' || inet_server_port()::text || '|' || current_database();")=='127.0.0.1|'+port+'|'+db
 report['explicitFixtureRouting']={'host':'127.0.0.1','port':port,'user':'macpro','environment':['PGPORT','PGUSER'],'reason':'Existing Music suites allow only uncredentialed loopback URLs; routing is assigned by the owned driver, not inherited, and validated before schema writes'};save()
 run('full220-apply-replay',['node','scripts/test-music-integration-rehearsal.mjs'])
 assert run('manifest-cardinality',psql,input='SELECT count(*) FROM public.tdf_schema_migration;')=='220'
 binary=root/'tdf-hq/.stack-work/dist/x86_64-osx/ghc-9.10.3/build/tdf-hq-test/tdf-hq-test'
 assert binary.is_file()
 report['testBinarySha256']=hashlib.sha256(binary.read_bytes()).hexdigest()
 sources=subprocess.check_output(['git','ls-files','tdf-hq'],cwd=root,text=True).splitlines()
 report['backendSourceSha256']={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in sources if p.endswith(('.hs','.cabal','.yaml','.lock'))}
 env.update({'COMMERCE_CHECKOUT_ENV':'sandbox','COMMERCE_LOOKUP_TOKEN_SECRET':'synthetic-ticket-payment-gate-lookup-secret-000000','COMMERCE_EVENT_ENCRYPTION_KEY':'0'*64,'COMMERCE_BANK_TRANSFER_INSTRUCTIONS':'Synthetic test account; no transfer is ever made'})
 for key in ['TDF_TICKET_PAYMENT_GATE_TEST_DB','TDF_MUSIC_PURCHASE_DATABASE_URL','TDF_MUSIC_PUBLICATION_DATABASE_URL','TDF_RAG_INDEX_DATABASE_URL','TDF_TERMS_HISTORY_DATABASE_URL','TDF_CHECKOUT_AMOUNT_DATABASE_URL']:env[key]=url
 for group in ['ledger-posting-postgresql','ticket-payment-gate-postgresql','music-purchase-postgresql','music-publication-postgresql','rag-index-privacy-postgresql']:
  result=run('compiled-'+group,[str(binary),'--match='+group,'--seed=20261025','--fail-on=empty'])
  summary=re.search(r'(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?',result)
  assert summary and int(summary[1])>0 and int(summary[2])==0 and int(summary[3] or 0)==0,(group,result[-2000:])
  report['steps'][-1]['summary']=summary[0];save()
 run('exact-ledger-arithmetic',psql+['-f','tdf-hq/test/integration/ledger_posting_balance.sql'])
 run('terms-history',['sh','scripts/test-checkout-terms-history.sh'])
 run('checkout-snapshot-and-money',['node','scripts/__tests__/checkout-amount-postgres.mjs'])
 run('namespace-after-compiled-writers',['python3','scripts/__tests__/ledger-posting-namespace.py','--negative-controls'])
 assert all(hashlib.sha256((root/p).read_bytes()).hexdigest()==h for p,h in report['sourceSha256'].items())
 assert all(hashlib.sha256((root/p).read_bytes()).hexdigest()==h for p,h in report['backendSourceSha256'].items())
 report['qualification']='Compiled ledger/ticket receipt and uncertainty/purchase/publication/privacy groups pass on full220 schema with no empty or pending cases; exact arithmetic, terms history, checkout snapshots and namespace controls also pass';save()

finally:
 try:
  if created:run('drop',['dropdb','-h','127.0.0.1','-p',port,'-U','macpro',db]);report['cleanup']['ownedDatabaseRemoved']=True
 finally:
  if started:run('stop',['pg_ctl','-D',str(cluster),'-m','fast','-w','stop']);report['cleanup']['ownedClusterStopped']=True
  report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
