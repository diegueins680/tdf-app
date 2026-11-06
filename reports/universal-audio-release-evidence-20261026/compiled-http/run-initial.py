#!/usr/bin/env python3
"""Exact-candidate local HTTP query/binding qualification, synthetic fixtures only."""
from pathlib import Path
import subprocess,os,json,time,urllib.request,urllib.error,urllib.parse,http.cookiejar,hashlib,datetime,tempfile,signal,sys,socket,secrets
root=Path('/private/tmp/tdf-audio-http-qualification-20261026'); packet=Path('/private/tmp/tdf-audio-http-evidence-20261026')
expected='5f45677588bd8494226e00fd51d81c134b84bdd6'
binary=root/'tdf-hq/.stack-work/dist/x86_64-osx/ghc-9.10.3/build/tdf-hq-exe/tdf-hq-exe'
assert not sys.flags.optimize,'Assertions must remain enabled'
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()==expected
assert not subprocess.check_output(['git','status','--porcelain','--untracked-files=no'],cwd=root,text=True).strip()
before=json.loads((packet/'source-before.json').read_text())
for p,h in before['sha256'].items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==h,p
assert binary.is_file() and (packet/'stack-build.exit').read_text().strip()=='0','Fresh canonical build must finish successfully'
assert Path('/usr/bin/sandbox-exec').exists() and sys.platform=='darwin'
assert not Path('/app/COMMIT').exists(),'Runtime commit-file override forbidden'
out=Path(tempfile.mkdtemp(prefix='tdf-audio-http-5f-',dir='/private/tmp'));os.chmod(out,0o700);print(out,flush=True)
env={k:v for k,v in os.environ.items() if k in ['HOME','USER','LOGNAME','SHELL','TZ','TMPDIR']}
env.update(PATH='/usr/local/opt/postgresql@16/bin:/private/tmp/tdf-node22-20261026/node-v22.14.0-darwin-x64/bin:/usr/local/bin:/usr/bin:/bin',LANG='en_US.UTF-8',LC_CTYPE='en_US.UTF-8',PGHOST='127.0.0.1',PGPORT='55581',PGUSER=__import__('getpass').getuser(),PGPASSFILE=str(out/'no-passfile'),CI='false')
db='tdf_audio_http_5f_'+secrets.token_hex(4)+'_test';url='postgresql://'+env['PGUSER']+'@127.0.0.1:55581/'+db
api='http://127.0.0.1:18831';procs=[];started=created=False
report={'source':expected,'sourceTree':before['tree'],'binarySha256':hashlib.sha256(binary.read_bytes()).hexdigest(),'sourceSha256':before['sha256'],'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'status':'RUNNING','steps':[],'assertions':[],'cleanup':{},'scope':'Real Stack-compiled HTTP/catalog SQL bindings, session and signing handlers with owned PostgreSQL222 and synthetic publishable asset metadata. No actual object delivery, browser playback, upload processing, production environment or performance SLO claim.'}
http=urllib.request.build_opener(urllib.request.ProxyHandler({}));jar=http.cookiejar.CookieJar();session=urllib.request.build_opener(urllib.request.ProxyHandler({}),urllib.request.HTTPCookieProcessor(jar))
def save():(out/'results.json').write_text(json.dumps(report,indent=2,ensure_ascii=False)+'\n')
def run(label,args,extra=None,timeout=300):
 with (out/(label+'.log')).open('wb') as log:r=subprocess.run(args,cwd=root,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,timeout=timeout)
 report['steps'].append({'label':label,'exitCode':r.returncode});save();assert r.returncode==0,label+' failed; inspect retained log'
 print(label+' PASS',flush=True)
def sql(text):return subprocess.check_output(['psql',url,'-XqAt','-v','ON_ERROR_STOP=1'],input=text,cwd=root,env=env,text=True).strip()
def check(condition,label):assert condition,label;report['assertions'].append(label);save();print(label+' PASS',flush=True)
def request(path,method='GET',payload=None,country='EC',auth=False,expected_status=200):
 assert path.startswith('/') and not path.startswith('//')
 assert method=='GET' or path in ['/signup','/login','/session/logout'],'Only owned synthetic account writes permitted'
 headers={}
 if country is not None:headers['CF-IPCountry']=country
 if payload is not None:headers['Content-Type']='application/json'
 req=urllib.request.Request(api+path,data=None if payload is None else json.dumps(payload).encode(),headers=headers,method=method)
 started=time.monotonic()
 try:r=(session if auth else http).open(req,timeout=10)
 except urllib.error.HTTPError as e:r=e
 body=r.read().decode();assert r.status==expected_status,(method,path,r.status,body[:300])
 value=json.loads(body) if body and 'json' in r.headers.get('Content-Type','') else body
 report['steps'].append({'label':method+' '+path.split('?')[0],'status':r.status,'durationMs':round((time.monotonic()-started)*1000,2)})
 return value,r.headers
def catalog(q='',country='EC',auth=False,**params):return request('/music/releases?'+urllib.parse.urlencode({'q':q,**params}),country=country,auth=auth)[0]
def ids(rows):return [r['id'] for r in rows]
profile='(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:55581"))(deny network-inbound)(allow network-inbound (local ip "localhost:*"))'
try:
 for port in [55581,18831]:assert subprocess.run(['lsof','-tiTCP:'+str(port),'-sTCP:LISTEN'],capture_output=True).returncode!=0,'Occupied port'
 run('node-version',['node','--version']);report['nodeVersion']=subprocess.check_output(['node','--version'],env=env,text=True).strip();assert report['nodeVersion']=='v22.14.0'
 report['postgresVersion']=subprocess.check_output(['pg_ctl','--version'],env=env,text=True).strip()
 dylib=out/'loopback-bind.dylib';adapter=root/'reports/welcome-club-integration-evidence-20261026/loopback-bind.c'
 report['bindAdapterSha256']=hashlib.sha256(adapter.read_bytes()).hexdigest()
 run('compile-bind-adapter',['clang','-dynamiclib','-Wall','-Wextra','-Werror',str(adapter),'-o',str(dylib)])
 lan=None
 for interface in ['en1','en0']:
  p=subprocess.run(['ipconfig','getifaddr',interface],capture_output=True,text=True)
  if p.returncode==0:lan=p.stdout.strip();break
 assert lan,'Required LAN isolation negative-control address unavailable'
 listener=socket.socket();listener.bind(('0.0.0.0',0));listener.listen(2)
 try:
  port=listener.getsockname()[1]
  with socket.create_connection((lan,port),timeout=1):pass
  probe='import socket,sys\ntry:socket.create_connection((sys.argv[1],int(sys.argv[2])),timeout=1)\nexcept PermissionError:sys.exit(0)\nexcept OSError:sys.exit(3)\nelse:sys.exit(2)'
  run('egress-denied-positive-control',['sandbox-exec','-p',profile,sys.executable,'-c',probe,lan,str(port)])
 finally:listener.close()
 (out/'socket').mkdir()
 run('initdb',['initdb','-D',str(out/'cluster'),'--username='+env['PGUSER'],'--encoding=UTF8','--locale=en_US.UTF-8','--auth=trust'])
 run('start-pg',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o','-p 55581 -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
 run('create-owned-db',['createdb','--template=template0',db]);created=True
 run('full222-production-schema',['node','scripts/test-music-integration-rehearsal.mjs'],{'TDF_MUSIC_REHEARSAL_DATABASE_URL':url})
 check(sql('SELECT count(*) FROM tdf_schema_migration')=='222','Full222 ledger verified')
 run('generate-authoritative-fixture',['node',str(packet/'fixture.mjs')])
 sql((packet/'fixture.sql').read_text())
 def make(slug,title='Canción sólo de pista',territory='Worldwide',publish=True):
  assert all("'" not in x for x in [slug,title,territory])
  release,version=sql("SELECT release_id,version_id FROM music_search_publishable('"+slug+"','"+title+"',ARRAY['"+territory+"'])").split('|')
  if publish:
   sql("UPDATE music_release_version SET state='scheduled',scheduled_by=created_by,release_at_utc=clock_timestamp()+interval '1 millisecond',release_timezone='UTC' WHERE id='"+version+"'; SELECT pg_sleep(0.01)")
   sql('SELECT music_publish_due(100)')
  return {'id':release,'version':version}
 visible=make('visible');another=make('another');private=make('private',publish=False);us=make('territory',territory='US');withdrawn=make('withdrawn')
 sql("UPDATE music_release_version SET state='suspended' WHERE id='"+withdrawn['version']+"'; UPDATE music_release_version SET state='withdrawn' WHERE id='"+withdrawn['version']+"'")
 mixed=make('mixed','Invisible authorized-track sentinel','US');unicode=make('unicode','夜の波')
 sql("UPDATE revenue_feature_flag SET enabled=TRUE,reason='Owned HTTP synthetic test' WHERE environment='sandbox' AND flag_key='music_releases.public'")
 backend={**env,'APP_ENV':'test','DATABASE_URL':url,'APP_PORT':'18831','RESET_DB':'false','RUN_MIGRATIONS':'false','SEED_DB':'false','ALLOWED_ORIGINS':'http://127.0.0.1:4187','ALLOW_ALL_ORIGINS':'false','CORS_DISABLE_DEFAULTS':'true','HQ_ASSETS_DIR':str(root/'tdf-hq/assets'),'MUSIC_TRUST_CF_IPCOUNTRY':'true','MUSIC_S3_ENDPOINT':'https://127.0.0.1:19031','MUSIC_S3_REGION':'us-east-1','MUSIC_S3_ACCESS_KEY_ID':'synthetic-http-access','MUSIC_S3_SECRET_ACCESS_KEY':'synthetic-http-secret-for-test-only','COMMERCE_CHECKOUT_ENV':'sandbox'}
 for flag in ['EVENT_DISCOVERY_ENABLED','ARTIST_ENRICHMENT_ENABLED','EVENT_LOGISTICS_RECHECK_ENABLED','OPERATIONS_WORKER_ENABLED','CONTEXTUAL_REPUTATION_ENABLED','PUBLIC_REPUTATION_PROJECTION_ENABLED','COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED','TAX_INVOICE_WORKER_ENABLED','TICKET_CONFIRMATION_EMAIL_ENABLED']:backend[flag]='false'
 wrapper='import os,sys;os.environ["DYLD_INSERT_LIBRARIES"]=sys.argv[1];os.environ["TDF_PILOT_API_PORT"]="18831";os.execv(sys.argv[2],[sys.argv[2],"+RTS","-N2","-RTS"])'
 log=(out/'backend.log').open('wb');p=subprocess.Popen(['sandbox-exec','-p',profile,sys.executable,'-c',wrapper,str(dylib),str(binary)],cwd=root,env=backend,stdout=log,stderr=subprocess.STDOUT,start_new_session=True);procs.append((p,log))
 for _ in range(180):
  try:
   if request('/health',country=None)[0].get('status')=='ok':break
  except (OSError,AssertionError,AttributeError):pass
  assert p.poll() is None,'Backend exited; retained backend.log'
  time.sleep(.5)
 else:raise AssertionError('Startup deadline')
 for port in [55581,18831]:
  listening=subprocess.check_output(['lsof','-nP','-iTCP:'+str(port),'-sTCP:LISTEN'],text=True)
  assert '127.0.0.1:'+str(port) in listening and '*:'+str(port) not in listening
  try:socket.create_connection((lan,port),timeout=1)
  except OSError:pass
  else:raise AssertionError('Listener accessible via LAN')
 report['networkIsolation']={'listenersLoopback':True,'lanDenied':True,'nonLoopbackEgressDeniedPositiveControl':True,'credentialsInherited':False}
 version,_=request('/version',country=None);check(version['commit']==expected,'Fresh compiled API version exact5f');report['actualVersion']=version
 baseline=ids(catalog('Canción'));check(set(baseline)=={visible['id'],another['id']} and len(baseline)==2,'Track-title search yields each published context once')
 for q in ['cancion','CANCION','  canción   sólo ','sólo de'] :check(ids(catalog(q))==baseline,'Accent/case/partial query '+q)
 check(ids(catalog('Spec visible'))==[visible['id']],'Existing release-title search')
 check(len(catalog('Spec Artist'))==4,'Existing display-artist search')
 check([ids(catalog('Canción',limit=1,offset=i))[0] for i in [0,1]]==baseline,'Actual HTTP limit offset preserves stable order')
 artist=catalog('Spec visible')[0]['artistPartyId']
 check(ids(catalog('Canción',artistPartyId=artist))==baseline and catalog('Canción',artistPartyId=-1)==[],'Actual artist binding positive and negative')
 check(ids(catalog('Canción',limit=0,offset=-1))==baseline[:1],'Actual server pagination clamps')
 request('/music/releases?limit=invalid',expected_status=400);check(True,'Invalid HTTP pagination rejected')
 check(us['id'] in ids(catalog('Canción',country='US')),'US authorized positive control')
 check(us['id'] not in ids(catalog('Canción',country=None)),'Unknown territory excludes US-only release')
 for invalid_country in ['XX','T1','invalid'] :check(us['id'] not in ids(catalog('Canción',country=invalid_country)),'Invalid territory fails closed '+invalid_country)
 check(catalog('Invisible authorized-track sentinel')==[] and ids(catalog('Invisible authorized-track sentinel',country='US'))==[mixed['id']],'Mixed release per-track territory preserved')
 check(ids(catalog('Public instrumental companion'))==[mixed['id']],'Mixed release authorized companion positive')
 check(ids(catalog('夜の波'))==[unicode['id']] and catalog('不存在')==[],'Non-Latin matching and empty normalization')
 check(catalog("'; DROP TABLE music_recording; --")==[] and catalog('%_')==[],'Parameter and punctuation safety')
 check(all(x['id'] not in ids(catalog('Canción')) for x in [private,us,withdrawn]),'Private withdrawn territory exclusion')
 password=secrets.token_urlsafe(24);email='listener@audio-http.example.test'
 signup,_=request('/signup','POST',{'firstName':'Synthetic','lastName':'Listener','email':email,'password':password,'termsAccepted':True,'termsVersion':'tdf-account-terms-v1'},auth=True,country=None)
 check(bool(signup.get('token')) and len(jar)>0,'Real signup issues session cookie')
 current,_=request('/session',auth=True,country=None);check(current and current['partyId']==signup['partyId'],'Real cookie session authenticated')
 check(ids(catalog('Canción',auth=True))==baseline,'Authenticated listener retains public isolation')
 stream=sql("SELECT id FROM music_asset WHERE release_version_id='"+visible['version']+"' AND asset_role='stream_audio' ORDER BY object_key LIMIT 1")
 access,headers=request('/music/assets/'+stream+'/access',auth=True)
 signed=urllib.parse.urlparse(access['url']);query=urllib.parse.parse_qs(signed.query)
 check(signed.hostname=='127.0.0.1' and signed.port==19031 and query.get('X-Amz-Expires')==['300'] and bool(query.get('X-Amz-Signature')),'Real authorized source signing bounded300seconds')
 report['signing']={'host':'loopback-test-only','expirySeconds':300,'urlNotFetched':True,'cacheControl':headers.get('Cache-Control'),'deliveryBytesVerified':False}
 for item in [private,us,withdrawn]:
  asset=sql("SELECT id FROM music_asset WHERE release_version_id='"+item['version']+"' AND asset_role='stream_audio' LIMIT 1")
  request('/music/assets/'+asset+'/access',auth=True,expected_status=404)
 check(True,'Actual private withdrawn territory signing denied')
 master=sql("SELECT id FROM music_asset WHERE release_version_id='"+visible['version']+"' AND asset_role='master_audio' LIMIT 1")
 request('/music/assets/'+master+'/access',auth=True,expected_status=404);check(True,'Original master not publicly signed')
 request('/session/logout','POST',auth=True,expected_status=200,country=None)
 check(request('/session',auth=True,country=None)[0] is None,'Real session logout invalidation')
 login,_=request('/login','POST',{'username':email,'password':password},auth=True,country=None);check(bool(login.get('token')),'Real fresh login succeeds')
 sql("UPDATE music_release_version SET state='suspended' WHERE id='"+visible['version']+"'; UPDATE music_release_version SET state='withdrawn' WHERE id='"+visible['version']+"'")
 check(visible['id'] not in ids(catalog('Canción')),'Withdrawal removed release from actual HTTP search')
 request('/music/assets/'+stream+'/access',auth=True,expected_status=404);check(True,'Withdrawal immediately prevents fresh signed source access')
 for name,h in before['sha256'].items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==h
 check(hashlib.sha256(binary.read_bytes()).hexdigest()==report['binarySha256'],'Binary unchanged during HTTP qualification')
 report['status']='PASS-real-compiled-http-synthetic-metadata-only';save()
except BaseException as e:report['status']='FAILED';report['failureType']=type(e).__name__;report['failure']=str(e)[:1000];save();raise
finally:
 for p,log in reversed(procs):
  if p.poll() is None:
   os.killpg(p.pid,signal.SIGTERM)
   try:p.wait(timeout=10)
   except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait(timeout=10)
  log.close()
 report['cleanup']['backendStopped']=all(p.poll() is not None for p,_ in procs)
 if created:
  try:run('drop-owned-db',['dropdb',db]);report['cleanup']['databaseRemoved']=True
  finally:
   if started:run('stop-owned-pg',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
 elif started:run('stop-owned-pg',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
 report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save();(packet/'latest-result-path.txt').write_text(str(out)+'\n')
