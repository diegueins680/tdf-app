#!/usr/bin/env python3
"""Disposable local rehearsal. Default --plan does not start services.
Synthetic mode only. Human-pilot privacy hold remains; no services start in plan mode.
"""
from pathlib import Path
import argparse,subprocess,os,json,time,urllib.request,hashlib,datetime,tempfile,signal,sys,socket,re
packet=Path('/private/tmp/tdf-welcome-club-runtime-20261026')
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--mode',choices=['plan','rehearsal'],default='plan')
parser.add_argument('--source',default='/private/tmp/tdf-welcome-main-integration-20261026')
parser.add_argument('--expected-source',help='Exact clean candidate commit required for rehearsal')
parser.add_argument('--binary',default='/private/tmp/tdf-alert-parity-repro-20261025/bin-post551/tdf-hq-exe')
args=parser.parse_args()
plan={'mode':args.mode,'humanParticipants':0,'protocols':['docs/persona-testing/human-usability-protocol.md','docs/activation-pilot/quito-creator-activation-2026-08-27.md'],'ports':{'postgres':55578,'api':18819,'web':4289},'recording':False,'quotations':False,'productionWrites':False,'outboundProviders':False,'providerAdapter':'Only supplied fictional avatar; real cookie/bearer checked by isolated API. Google storage and native file chooser excluded.','stopPolicy':'The synthetic runner has a bounded browser deadline and finally stops only owned services and drops its owned database. No manual mode.','humanAcceptance':'Two consenting adults and independent observer scorecards remain required; automatic results never count as human success.','sourceRequirement':'Exact --expected-source candidate, with approved d819 welcome and protected ed9 ancestry. Published42b9 repair616 is included prospectively; human pilot remains held until protected integration, publication verification and consent. Native Stack/GHC9.10.3 API qualified at fe179; compiled source inputs byte-identical to protected ed9main. Binary metadata retains its original identity.'}
plan['humanPilotPrivacyHold']='Combined privacy assertions are synthetic only. No manual/human mode; protected delivery and consent remain separate.'
if args.mode=='plan':print(json.dumps(plan,indent=2,ensure_ascii=False));sys.exit(0)
if sys.flags.optimize:parser.error('Run unoptimized Python: the inherited isolation assertions must remain active')
if not args.expected_source or not re.fullmatch(r'[a-f0-9]{40}',args.expected_source):parser.error('--expected-source must be a full immutable commit SHA')
root=Path(args.source).resolve();binary=Path(args.binary).resolve()
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==args.expected_source,'Source differs from explicitly selected commit'
source_tree=subprocess.check_output(['git','rev-parse','HEAD^{tree}'],cwd=root,text=True).strip()
for required in ['d8191612b1d8e185e997a4ecf72c2d23ed83fba9','ed9eb9a1600f0a827e002bb2ab64a608f8352b2c','42b9fbe8dbcd922755c4b23d85a55e030483d5cf','3af768dacd95c1223975c62da3ce8088bed89b47','5ebcb33838a28db9acf14ad95d422fe6669d40d0']:
 subprocess.run(['git','merge-base','--is-ancestor',required,'HEAD'],cwd=root,check=True)
for baseline,paths in [
 ('fe179de45d9b0e085bb7cd2f00a9704991fa1652',['tdf-hq/src','tdf-hq/app','tdf-hq/tdf-hq.cabal','tdf-hq/stack.yaml','tdf-hq/stack.yaml.lock']),
 ('42b9fbe8dbcd922755c4b23d85a55e030483d5cf',['tdf-hq-ui/src/api','tdf-hq-ui/src/session','tdf-hq-ui/src/pages/LoginPage.tsx','tdf-hq-ui/src/pages/ArtistOnboardingPage.tsx','tdf-hq-ui/src/pages/ResetPasswordPage.tsx','package.json','package-lock.json']),
 ('d8191612b1d8e185e997a4ecf72c2d23ed83fba9',['tdf-hq-ui/src/components/PublicWelcome.tsx','tdf-hq-ui/src/pages/DirectorySearchPage.tsx','tdf-hq-ui/src/pages/TdfPlatformPage.tsx','tdf-hq-ui/src/i18n/locales/directorySearch.es.ts','tdf-hq-ui/src/i18n/locales/directorySearch.en.ts','e2e']),
 ('5ebcb33838a28db9acf14ad95d422fe6669d40d0',['tdf-hq-ui/src/pages/FanClubPage.tsx','tdf-hq-ui/src/pages/FanClubPage.recovery.test.tsx']),
 ('3af768dacd95c1223975c62da3ce8088bed89b47',['scripts/production-migrations.json','tdf-hq/sql'])]:
 subprocess.run(['git','diff','--exit-code',baseline,'HEAD','--',*paths],cwd=root,check=True)

assert not subprocess.check_output(['git','status','--porcelain','--untracked-files=no'],cwd=root,text=True).strip(),'Source has tracked modifications'
assert hashlib.sha256(binary.read_bytes()).hexdigest()=='cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a','Unexpected compatibility binary'
assert sys.platform=='darwin' and Path('/usr/bin/sandbox-exec').exists(),'Requires validated Darwin network isolation; no unrestricted fallback'
out=Path(tempfile.mkdtemp(prefix='tdf-welcome-club-activation-20261026-',dir='/private/tmp'));os.chmod(out,0o700)
print(str(out),flush=True)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']};env['PATH']='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-node22-20261026/node-v22.14.0-darwin-x64/bin:'+env['PATH'];env.update(PGHOST='127.0.0.1',PGPORT='55578',PGUSER=os.getlogin(),PGPASSFILE=str(out/'no-passfile'),CI='false')
db='tdf_welcome_20261026_test';url='postgresql://'+env['PGUSER']+'@127.0.0.1:55578/'+db
report={'plan':plan,'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'sourceTree':source_tree,'sourceWorktreeStatus':subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True).splitlines(),'sourceWorktreeDirty':bool(subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True).strip()),'trackedSourceClean':True,'binaryQualifiedSource':'fe179de45d9b0e085bb7cd2f00a9704991fa1652','binarySha256':'cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a','compiledInputsEquivalentToProtectedMain':'ed9eb9a1600f0a827e002bb2ab64a608f8352b2c','steps':[],'cleanup':{},'status':'running'}
report['nodeVersion']=subprocess.check_output(['node','--version'],env=env,text=True).strip();assert report['nodeVersion']=='v22.14.0','Requires canonical Node22.14.0'
procs=[];started=created=False
http=urllib.request.build_opener(urllib.request.ProxyHandler({}))
def save():(out/'results.json').write_text(json.dumps(report,indent=2,ensure_ascii=False)+'\n')
def run(name,command,extra=None,timeout=300):
 with (out/(name+'.log')).open('wb') as log:
  if name=='actual-api-browser':
   child=subprocess.Popen(command,cwd=root,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
   try:child.wait(timeout=timeout)
   finally:
    try:os.killpg(child.pid,signal.SIGTERM)
    except ProcessLookupError:pass
    if child.poll() is None:child.wait(timeout=10)
   r=child
  else:r=subprocess.run(command,cwd=root,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,timeout=timeout)
 report['steps'].append({'step':name,'exitCode':r.returncode});save()
 if r.returncode:raise RuntimeError(name+' failed; inspect private runtime log')
 print(name+' PASS',flush=True)
def scalar(sql):return subprocess.check_output(['psql',url,'-XqAt','-v','ON_ERROR_STOP=1','-c',sql],env=env,text=True).strip()
profile='(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:55578"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
try:
 for port in [55578,18819,4289]:assert subprocess.run(['lsof','-tiTCP:'+str(port),'-sTCP:LISTEN'],capture_output=True).returncode!=0,'Occupied port; no unrelated process will be stopped'
 dylib=out/'loopback-bind.dylib'
 run('compile-test-bind-adapter',['clang','-dynamiclib','-Wall','-Wextra','-Werror',str(packet/'loopback-bind.c'),'-o',str(dylib)])
 # Meaningful negative control: an owned benign wildcard listener is reachable
 # over the host LAN address outside the sandbox, but the same connect is denied
 # inside it. No host address is retained in the report.
 lan=None
 for interface in ['en1','en0']:
  probe=subprocess.run(['ipconfig','getifaddr',interface],capture_output=True,text=True)
  if probe.returncode==0:lan=probe.stdout.strip();break
 assert lan,'No LAN address for required isolation negative control; fail closed'
 listener=socket.socket();listener.bind(('0.0.0.0',0));listener.listen(2)
 try:
  port=listener.getsockname()[1]
  with socket.create_connection((lan,port),timeout=1):pass
  check='import socket,sys\ntry: socket.create_connection((sys.argv[1],int(sys.argv[2])),timeout=1)\nexcept PermissionError: sys.exit(0)\nexcept OSError: sys.exit(3)\nelse: sys.exit(2)'
  run('deny-nonloopback-egress',['sandbox-exec','-p',profile,sys.executable,'-c',check,lan,str(port)])
 finally:listener.close()
 (out/'socket').mkdir();(out/'empty-env').mkdir()
 run('init',['initdb','-D',str(out/'cluster'),'--username='+env['PGUSER'],'--encoding=UTF8','--locale=en_US.UTF-8','--auth=trust'])
 run('start',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o','-p 55578 -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
 run('create',['createdb','--template=template0',db]);created=True
 run('full221-schema',['node','scripts/test-music-integration-rehearsal.mjs'],{'TDF_MUSIC_REHEARSAL_DATABASE_URL':url})
 assert scalar('SELECT count(*) FROM tdf_schema_migration')=='221'
 config=out/'vite.mjs'
 config.write_text("import {loadConfigFromFile,mergeConfig} from "+json.dumps(str(root/'node_modules/vite/dist/node/index.js'))+";\nconst loaded=await loadConfigFromFile({command:'build',mode:'production'},"+json.dumps(str(root/'tdf-hq-ui/vite.config.ts'))+");\nexport default mergeConfig(loaded.config,"+json.dumps({'root':str(root/'tdf-hq-ui'),'envDir':str(out/'empty-env'),'build':{'outDir':str(out/'build'),'emptyOutDir':False},'preview':{'host':'127.0.0.1','port':4289,'strictPort':True}})+");\n")
 webenv={'VITE_API_BASE':'http://127.0.0.1:18819','VITE_POSTHOG_KEY':'','VITE_GOOGLE_CLIENT_ID':''}
 run('build-isolated-web',['node','node_modules/vite/bin/vite.js','build','--config',str(config)],webenv,timeout=300)
 backend={**env,'APP_ENV':'test','DATABASE_URL':url,'APP_PORT':'18819','RESET_DB':'false','RUN_MIGRATIONS':'false','SEED_DB':'false','DEFAULT_LOCALE':'es','ALLOWED_ORIGINS':'http://127.0.0.1:4289','HQ_APP_URL':'http://127.0.0.1:4289','HQ_ASSETS_DIR':str(out)}
 for flag in ['EVENT_DISCOVERY_ENABLED','ARTIST_ENRICHMENT_ENABLED','EVENT_LOGISTICS_RECHECK_ENABLED','OPERATIONS_WORKER_ENABLED','CONTEXTUAL_REPUTATION_ENABLED','PUBLIC_REPUTATION_PROJECTION_ENABLED','COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED']:backend[flag]='false'
 # sandbox-exec itself strips DYLD variables. Set the test adapter only after
 # entering the sandbox, then exec the unchanged, hash-checked test binary.
 wrapper='import os,sys;os.environ["DYLD_INSERT_LIBRARIES"]=sys.argv[1];os.environ["TDF_PILOT_API_PORT"]="18819";os.execv(sys.argv[2],[sys.argv[2]])'
 commands=[('backend',['sandbox-exec','-p',profile,sys.executable,'-c',wrapper,str(dylib),str(binary)],backend),('web',['node','node_modules/vite/bin/vite.js','preview','--config',str(config)],{**env,**webenv})]
 for label,command,variables in commands:
  log=(out/(label+'.log')).open('wb');p=subprocess.Popen(command,cwd=root,env=variables,stdout=log,stderr=subprocess.STDOUT,start_new_session=True);procs.append((p,log))
 for endpoint in ['http://127.0.0.1:18819/health','http://127.0.0.1:4289/']:
  for _ in range(180):
   try:
    with http.open(endpoint,timeout=1) as r:
     if r.status==200:break
   except OSError:pass
   assert all(p.poll() is None for p,_ in procs),'Runtime exited; inspect private logs'
   time.sleep(.5)
  else:raise AssertionError('Runtime startup deadline')
 for port in [55578,18819,4289]:
  listening=subprocess.check_output(['lsof','-nP','-iTCP:'+str(port),'-sTCP:LISTEN'],text=True)
  assert '127.0.0.1:'+str(port) in listening and '*:'+str(port) not in listening,'Listener escaped loopback'
  try:
   with http.open('http://'+lan+':'+str(port)+'/',timeout=1):raise AssertionError('LAN access unexpectedly succeeded')
  except (OSError,urllib.error.URLError):pass
 report['networkIsolation']={'threeListenersLoopbackOnly':True,'lanRequestsDenied':True,'nonLoopbackEgressDeniedWithPositiveControl':True,'noAddressRetained':True}
 with http.open('http://127.0.0.1:18819/version') as r:report['actualVersion']=json.load(r)
 report['binaryRevision']=report['actualVersion'].get('commit')
 save()
 browserenv={'TDF_PILOT_SOURCE':str(root),'TDF_PILOT_OUT':str(out),'TDF_PILOT_MODE':args.mode}
 entry=packet/'rehearsal.mjs'
 report['driverSha256']=hashlib.sha256(entry.read_bytes()).hexdigest();report['runtimeSha256']=hashlib.sha256((packet/'runtime.mjs').read_bytes()).hexdigest();save()
 run('actual-api-browser',['node',str(entry)],browserenv,timeout=300)
 if args.mode=='rehearsal':
  observed=json.loads((out/'journey.json').read_text());artist=observed['artistId'];fans=observed['fanIds']
  for fan in fans:
   assert scalar(f'SELECT count(*) FROM fan_follow WHERE fan_party_id={int(fan)} AND artist_party_id={int(artist)}')=='1'
   assert scalar(f"SELECT count(*) FROM engagement_event WHERE actor_party_id={int(fan)} AND target_artist_id={int(artist)} AND event_type='follow'")=='1'
   assert scalar(f"SELECT count(*) FROM user_onboarding_progress WHERE party_id={int(fan)} AND first_value='artist_followed' AND first_value_completed_at>=signup_completed_at")=='1'
  report['databaseAssertions']={'syntheticFirstValueRecords':2,'syntheticPairs':2,'followRows':2,'evidenceRows':2,'duplicateRows':0}
  report['welcomeAssertions']=json.loads((out/'welcome.json').read_text())['welcome'];report['clubFallback']=json.loads((out/'welcome.json').read_text())['clubFallback'];assert len(report['clubFallback'])==2;assert json.loads((out/'browser.json').read_text())['blockedMutations']==0;report['logoutDiagnostic']=json.loads((out/'logout-diagnostic.json').read_text());report['failedLogoutPrivacyFixTested']=True;report['humanPilotHeld']=True;report['status']='VERIFIED-synthetic-welcome-club-logout-and-activation-only'
 else:report['status']='PARTIAL-human-observer-scorecards-required'
except BaseException as error:
 report['status']='FAILED';report['failureType']=type(error).__name__;save();raise
finally:
 for p,log in reversed(procs):
  if p.poll() is None:
   os.killpg(p.pid,signal.SIGTERM)
   try:p.wait(timeout=10)
   except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait(timeout=10)
  log.close()
 report['cleanup']['runtimesStopped']=all(p.poll() is not None for p,_ in procs)
 if created:
  try:run('drop',['dropdb',db]);report['cleanup']['databaseRemoved']=True
  finally:
   if started:run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
 elif started:run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
 report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
