#!/usr/bin/env python3
"""Disposable follow response-loss rehearsal. Default plan creates no resources."""
from pathlib import Path
import argparse,subprocess,os,json,time,urllib.request,hashlib,datetime,tempfile,signal,sys,socket
packet=Path(__file__).resolve().parent
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--mode',choices=['plan','rehearsal'],default='plan')
parser.add_argument('--source',required=True)
parser.add_argument('--expected-source',required=True)
parser.add_argument('--binary',required=True)
args=parser.parse_args()
plan={'mode':args.mode,'humanParticipants':0,'productionWrites':False,'providersUsed':False,'fixtureSetup':'One fictional artist and six fictional fans; follow/unfollow committed-response loss, failed confirmation reads, pre-commit failures and cross-context return. No club provisioning.','ports':{'postgres':55534,'api':18714,'web':4257},'scope':'Optimized candidate web and qualified native fe179 API with disposable221-schema PostgreSQL. Successful POST/DELETE responses are discarded after commit; separate pre-commit aborts and confirmation503s exercise failures. Artist/auth/club backend unchanged; receipt backend and real providers excluded.'}
if args.mode=='plan':print(json.dumps(plan,indent=2,ensure_ascii=False));sys.exit(0)
assert args.mode=='rehearsal','This isolated verification harness has no human mode'
root=Path(args.source).resolve();binary=Path(args.binary).resolve()
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==args.expected_source,'Source differs from explicitly selected revision'
subprocess.run(['git','diff','--exit-code','fe179de45d9b0e085bb7cd2f00a9704991fa1652',args.expected_source,'--','tdf-hq'],cwd=root,check=True,stdout=subprocess.DEVNULL)
assert not subprocess.check_output(['git','status','--porcelain','--untracked-files=no'],cwd=root,text=True).strip(),'Source has tracked modifications'
assert hashlib.sha256(binary.read_bytes()).hexdigest()=='cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a','Unexpected compatibility binary'
assert sys.platform=='darwin' and Path('/usr/bin/sandbox-exec').exists(),'Requires validated Darwin network isolation; no unrestricted fallback'
assert subprocess.check_output(['node','--version'],text=True).strip()=='v22.14.0','Use pinned Node22.14.0 before creating resources'
out=Path(tempfile.mkdtemp(prefix='tdf-follow-response-runtime-20261026-',dir='/private/tmp'));os.chmod(out,0o700)
print(str(out),flush=True)
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']};env['PATH']='/usr/local/opt/postgresql@16/bin:'+env['PATH'];env.update(PGHOST='127.0.0.1',PGPORT='55534',PGUSER=os.getlogin(),PGPASSFILE=str(out/'no-passfile'),CI='false')
db='tdf_follow_response_20261026_test';url='postgresql://'+env['PGUSER']+'@127.0.0.1:55534/'+db
report={'plan':plan,'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'sourceTree':subprocess.check_output(['git','rev-parse','HEAD^{tree}'],cwd=root,text=True).strip(),'binaryRevision':'fe179de45d9b0e085bb7cd2f00a9704991fa1652','steps':[],'cleanup':{},'status':'running'}
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
profile='(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:55534"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
try:
 for port in [55534,18714,4257]:assert subprocess.run(['lsof','-tiTCP:'+str(port),'-sTCP:LISTEN'],capture_output=True).returncode!=0,'Occupied port; no unrelated process will be stopped'
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
 run('start',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o','-p 55534 -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
 run('create',['createdb','--template=template0',db]);created=True
 run('full221-schema',['node','scripts/test-music-integration-rehearsal.mjs'],{'TDF_MUSIC_REHEARSAL_DATABASE_URL':url})
 assert scalar('SELECT count(*) FROM tdf_schema_migration')=='221'
 config=out/'vite.mjs'
 config.write_text("import {loadConfigFromFile,mergeConfig} from "+json.dumps(str(root/'node_modules/vite/dist/node/index.js'))+";\nconst loaded=await loadConfigFromFile({command:'build',mode:'production'},"+json.dumps(str(root/'tdf-hq-ui/vite.config.ts'))+");\nexport default mergeConfig(loaded.config,"+json.dumps({'root':str(root/'tdf-hq-ui'),'envDir':str(out/'empty-env'),'build':{'outDir':str(out/'build'),'emptyOutDir':False},'preview':{'host':'127.0.0.1','port':4257,'strictPort':True}})+");\n")
 webenv={'VITE_API_BASE':'http://127.0.0.1:18714','VITE_POSTHOG_KEY':'','VITE_GOOGLE_CLIENT_ID':''}
 run('build-isolated-web',['node','node_modules/vite/bin/vite.js','build','--config',str(config)],webenv,timeout=300)
 backend={**env,'APP_ENV':'test','DATABASE_URL':url,'APP_PORT':'18714','RESET_DB':'false','RUN_MIGRATIONS':'false','SEED_DB':'false','DEFAULT_LOCALE':'es','ALLOWED_ORIGINS':'http://127.0.0.1:4257','HQ_APP_URL':'http://127.0.0.1:4257','HQ_ASSETS_DIR':str(out)}
 for flag in ['EVENT_DISCOVERY_ENABLED','ARTIST_ENRICHMENT_ENABLED','EVENT_LOGISTICS_RECHECK_ENABLED','OPERATIONS_WORKER_ENABLED','CONTEXTUAL_REPUTATION_ENABLED','PUBLIC_REPUTATION_PROJECTION_ENABLED','COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED']:backend[flag]='false'
 # sandbox-exec itself strips DYLD variables. Set the test adapter only after
 # entering the sandbox, then exec the unchanged, hash-checked test binary.
 wrapper='import os,sys;os.environ["DYLD_INSERT_LIBRARIES"]=sys.argv[1];os.environ["TDF_PILOT_API_PORT"]="18714";os.execv(sys.argv[2],[sys.argv[2]])'
 commands=[('backend',['sandbox-exec','-p',profile,sys.executable,'-c',wrapper,str(dylib),str(binary)],backend),('web',['node','node_modules/vite/bin/vite.js','preview','--config',str(config)],{**env,**webenv})]
 for label,command,variables in commands:
  log=(out/(label+'.log')).open('wb');p=subprocess.Popen(command,cwd=root,env=variables,stdout=log,stderr=subprocess.STDOUT,start_new_session=True);procs.append((p,log))
 for endpoint in ['http://127.0.0.1:18714/health','http://127.0.0.1:4257/']:
  for _ in range(180):
   try:
    with http.open(endpoint,timeout=1) as r:
     if r.status==200:break
   except OSError:pass
   assert all(p.poll() is None for p,_ in procs),'Runtime exited; inspect private logs'
   time.sleep(.5)
  else:raise AssertionError('Runtime startup deadline')
 for port in [55534,18714,4257]:
  listening=subprocess.check_output(['lsof','-nP','-iTCP:'+str(port),'-sTCP:LISTEN'],text=True)
  assert '127.0.0.1:'+str(port) in listening and '*:'+str(port) not in listening,'Listener escaped loopback'
  try:
   with http.open('http://'+lan+':'+str(port)+'/',timeout=1):raise AssertionError('LAN access unexpectedly succeeded')
  except (OSError,urllib.error.URLError):pass
 report['networkIsolation']={'threeListenersLoopbackOnly':True,'lanRequestsDenied':True,'nonLoopbackEgressDeniedWithPositiveControl':True,'noAddressRetained':True}
 with http.open('http://127.0.0.1:18714/version') as r:report['actualVersion']=json.load(r)
 assert report['actualVersion']['commit']==report['binaryRevision'],'Unexpected native binary identity'
 save()
 browserenv={'TDF_PILOT_SOURCE':str(root),'TDF_PILOT_OUT':str(out),'TDF_PILOT_MODE':args.mode}
 entry=packet/'browser.mjs'
 run('actual-api-browser',['node',str(entry)],browserenv,timeout=300)
 observed=json.loads((out/'follow-response.json').read_text());assert observed['status']=='VERIFIED-isolated-only';artist=int(observed['artistId']);fans=','.join(str(int(x)) for x in observed['fanIds'])
 assert len(observed['fanIds'])==6
 assert scalar(f'SELECT count(*) FROM fan_follow WHERE fan_party_id IN ({fans}) AND artist_party_id={artist}')=='6'
 assert scalar(f"SELECT count(*) FROM engagement_event WHERE actor_party_id IN ({fans}) AND target_artist_id={artist} AND event_type='follow'")=='8'
 assert scalar(f"SELECT count(*) FROM engagement_event WHERE actor_party_id IN ({fans}) AND target_artist_id={artist} AND event_type='unfollow'")=='2'
 assert scalar(f"SELECT count(*) FROM user_onboarding_progress WHERE party_id IN ({fans}) AND first_value='artist_followed' AND first_value_completed_at IS NOT NULL")=='6'
 assert scalar(f'SELECT count(*) FROM (SELECT fan_party_id,artist_party_id FROM fan_follow GROUP BY 1,2 HAVING count(*)>1) duplicates')=='0'
 assert scalar(f'SELECT count(*) FROM fan_club WHERE artist_party_id={artist}')=='0'
 assert scalar(f'SELECT count(*) FROM artist_profile WHERE artist_party_id={artist}')=='1'
 report['databaseAssertions']={'syntheticPairs':6,'followRows':6,'followEvents':8,'explicitUnfollowEvents':2,'serverFirstValues':6,'clubRows':0,'artistProfiles':1,'duplicateRows':0}
 report['status']='VERIFIED-isolated-follow-response-recovery-only'

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
