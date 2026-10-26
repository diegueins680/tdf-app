#!/usr/bin/env python3
"""Actual API and durable media checks; only an owned disposable PostgreSQL DB."""
import argparse, base64, concurrent.futures, hashlib, io, json, os, pathlib, signal, subprocess, time, urllib.error, urllib.request, uuid, wave
p=argparse.ArgumentParser();p.add_argument('--binary',required=True);p.add_argument('--output',required=True);p.add_argument('--source-moderation',action='store_true');p.add_argument('--browser',action='store_true');a=p.parse_args()
assert not a.browser or a.source_moderation,'The integrated browser lane requires source moderation'
root=pathlib.Path(__file__).resolve().parent.parent
binary=pathlib.Path(a.binary).resolve();assert binary.is_file()
out=pathlib.Path(a.output).resolve();out.mkdir(mode=0o700,parents=False,exist_ok=False)
ci=os.environ.get('GITHUB_ACTIONS')=='true'
native=os.environ.get('TDF_TEST_NATIVE_POSTGRES')=='1'
assert not any(os.environ.get(k) for k in ('PGHOSTADDR','PGSERVICE','PGSERVICEFILE'))
host='postgres' if ci and not native else '127.0.0.1';port='5432' if ci and not native else os.environ.get('TDF_TEST_NATIVE_POSTGRES_PORT','55486')
assert port.isdigit() and 0<int(port)<65536
role='postgres' if ci and not native else os.environ.get('TDF_TEST_NATIVE_POSTGRES_USER','postgres' if ci else 'macpro')
assert role.replace('_','').isalnum()
env={k:v for k,v in os.environ.items() if k in ('HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR')}
if ci or (native and 'TDF_TEST_POSTGRES_PASSWORD' in os.environ):
 fixture_password=os.environ.get('TDF_TEST_POSTGRES_PASSWORD')
 assert fixture_password,'An explicit nonempty fixture database password is required'
 env['PGPASSWORD']=fixture_password
env['TDF_TEST_BUNDLED_BROWSER']='1' if ci else '0'
db='tdf_event_memories_http_test';routing=['-h',host,'-p',port,'-U',role]
api_port='18886';origin='http://127.0.0.1:'+api_port
opener=urllib.request.build_opener(urllib.request.ProxyHandler({}))
runtime_paths=['tdf-hq/src/TDF/API/SocialEventsAPI.hs','tdf-hq/src/TDF/DTO/SocialEventsDTO.hs',
 'tdf-hq/src/TDF/Server/SocialEventsHandlers.hs','tdf-hq/src/TDF/Interactions/Notifications.hs',
 'tdf-hq/sql/2026-10-25_event_memory_invitations.sql','scripts/test-event-memories-http.py']
if a.source_moderation:runtime_paths+=['tdf-hq/src/TDF/Interactions/API.hs','tdf-hq/src/TDF/Interactions/Server.hs','tdf-hq/sql/2026-10-25_event_memory_source_moderation.sql','tdf-hq/sql/2026-10-25_event_memory_source_authorization.sql','tdf-hq/sql/2026-10-25_event_memory_source_permission_locks.sql']
if a.browser:runtime_paths+=['scripts/test-event-memory-source-browser.mjs','tdf-hq-ui/src/components/events/EventMemoryModeration.tsx','tdf-hq-ui/src/api/eventMemoryModeration.ts','tdf-hq-ui/src/pages/SocialEventDetailPage.tsx']
if a.browser:runtime_paths+=['tdf-hq-ui/src/api/socialEvents.ts','tdf-hq-ui/src/api/client.ts','tdf-hq-ui/src/api/eventMemoryInvitations.ts']
if a.browser:runtime_paths+=['tdf-hq-ui/src/components/PublicBranding.tsx','tdf-hq-ui/src/features/interactions/InteractionPanel.tsx']
if a.browser:runtime_paths+=['scripts/test-event-memory-signup-browser.mjs','tdf-hq-ui/src/pages/LoginPage.tsx','tdf-hq-ui/src/api/auth.ts','tdf-hq-ui/src/api/session.ts','tdf-hq-ui/src/utils/loginRouting.ts','tdf-hq-ui/src/utils/roles.ts','tdf-hq-ui/src/utils/signupFields.ts','tdf-hq/src/TDF/ServerAuth.hs','tdf-hq/src/TDF/DTO.hs']
if a.browser:runtime_paths+=['scripts/test-event-raci-browser.sh','.github/workflows/event-raci-browser.yml']
if a.browser:runtime_paths+=['tdf-hq-ui/src/routes/EventCommunityEntry.tsx','tdf-hq-ui/src/routes/publicRoutes.tsx','tdf-hq-ui/src/routes/protectedRoutes.tsx','tdf-hq-ui/src/pages/DirectoryPublicDetailPage.tsx','tdf-hq-ui/src/routes/AppRoutes.tsx','tdf-hq-ui/src/routes/AppShell.tsx']
if a.browser:runtime_paths+=['tdf-hq-ui/src/session/useSessionOwner.ts','tdf-hq-ui/src/session/useOwnedSessionDraft.ts','tdf-hq-ui/src/session/authEvents.ts','tdf-hq-ui/src/session/SessionContext.tsx','tdf-hq-ui/src/components/events/EventMemoryInvitations.tsx','tdf-hq-ui/src/features/fans/useOwnedProfileDraft.ts']
# Bind the actual complete schema, including protected-main successors, rather
# than assuming that a successful source-specific request proves migration scope.
manifest=json.loads((root/'scripts/production-migrations.json').read_text())
expected_migration_ids=sorted(entry['id'] for entry in manifest['migrations'])
assert expected_migration_ids and len(expected_migration_ids)==len(set(expected_migration_ids))
runtime_paths+=['scripts/production-migrations.json','scripts/render-production-migration-batch.mjs','scripts/lib/production-release.mjs','scripts/lib/migration-contract.mjs']
runtime_paths+=[entry['path'] for entry in manifest['migrations']]
runtime_paths=list(dict.fromkeys(runtime_paths))
def runtime_sources():return {path:hashlib.sha256((root/path).read_bytes()).hexdigest() for path in runtime_paths}
report={'source':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),
 'sourceWorktreeDirty':bool(subprocess.check_output(['git','status','--porcelain'],cwd=root,text=True).strip()),
 'runtimeSources':runtime_sources(),'binarySha256':hashlib.sha256(binary.read_bytes()).hexdigest(),'requests':[],'assertions':[],'externalProviderCalls':0}
owned=False;server=None;log=None

def run(args,stdin=None):
 r=subprocess.run(args,cwd=root,env=env,input=stdin,capture_output=True,timeout=180)
 if r.returncode:raise AssertionError('Owned fixture command failed: '+r.stderr.decode()[-1000:])
 return r.stdout

def sql(command):return run(['psql',*routing,'-d',db,'-XqAt','-v','ON_ERROR_STOP=1','-c',command]).decode().strip()
def apply(path):run(['psql',*routing,'-d',db,'-Xq','-v','ON_ERROR_STOP=1','-f',str(path)])
def record(label):report['assertions'].append(label)
def request(path,token=None,body=None,method='GET',expected=200,raw=None,content_type=None):
 headers={}
 if token:headers['Authorization']='Bearer '+token
 if body is not None:raw=json.dumps(body).encode();content_type='application/json'
 if content_type:headers['Content-Type']=content_type
 req=urllib.request.Request(origin+path,data=raw,headers=headers,method=method)
 try:r=opener.open(req,timeout=20)
 except urllib.error.HTTPError as error:r=error
 data=r.read();report['requests'].append({'method':method,'path':path,'status':r.status})
 assert r.status in expected if isinstance(expected,tuple) else r.status==expected,(method,path,r.status,data[:300])
 result=json.loads(data) if 'json' in r.headers.get('Content-Type','') and data else data
 return result,r.headers

def start():
 global server,log
 assets=out/'assets';assets.mkdir(exist_ok=True)
 variables={**env,'APP_ENV':'test','DATABASE_URL':f'postgresql://{role}@{host}:{port}/{db}','APP_PORT':api_port,
  'RESET_DB':'false','RUN_MIGRATIONS':'false','AUTO_APPLY_PRODUCTION_MIGRATIONS':'false','SEED_DB':'false','DEFAULT_LOCALE':'es',
  'ALLOWED_ORIGINS':'http://127.0.0.1:5197,http://127.0.0.1:5206','HQ_APP_URL':'http://127.0.0.1:5206' if a.browser else 'http://127.0.0.1:5197','HQ_ASSETS_DIR':str(assets),
  'HQ_ASSETS_BASE_URL':'https://assets.persona.test/assets/serve','EVENT_DISCOVERY_ENABLED':'false','ARTIST_ENRICHMENT_ENABLED':'false',
  'EVENT_LOGISTICS_RECHECK_ENABLED':'false','OPERATIONS_WORKER_ENABLED':'false'}
 log=(out/('backend-'+str(len(report['requests']))+'.log')).open('wb')
 server=subprocess.Popen([str(binary)],cwd=root,env=variables,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
 for _ in range(120):
  assert server.poll() is None,'Candidate API exited during startup'
  try:
   with opener.open(origin+'/health',timeout=1) as r:
    if r.status==200:return
  except OSError:pass
  time.sleep(.5)
 raise AssertionError('API startup deadline')

def stop():
 global server,log
 if server and server.poll() is None:
  os.killpg(server.pid,signal.SIGTERM)
  try:server.wait(timeout=10)
  except subprocess.TimeoutExpired:os.killpg(server.pid,signal.SIGKILL);server.wait(timeout=10)
 if log:log.close()

def upload(event,token,name,mime,data,expected=200):
 boundary='tdf-synthetic-media-boundary'
 raw=(f'--{boundary}\r\nContent-Disposition: form-data; name="file"; filename="{name}"\r\nContent-Type: {mime}\r\n\r\n'.encode()+data+f'\r\n--{boundary}--\r\n'.encode())
 return request(f'/social-events/events/{event}/moments/media',token,method='POST',expected=expected,raw=raw,content_type='multipart/form-data; boundary='+boundary)[0]

def source_checks(owner,owner_token,participant,participant_token,source_moment):
 ident=source_moment['emId'];event=919143001;moment_path=f'/social-events/events/{event}/moments/{ident}'
 def summary(token):return request(f'/interactions/targets/event_moment/{ident}',token)[0]
 def command(token,payload,key=None,expected=200):
  return request(f'/interactions/targets/{target}/commands',token,body={'requestKey':key or str(uuid.uuid4()),'command':payload},method='POST',expected=expected)[0]
 def action(op,token,reason='Synthetic source review'):
  current=summary(token);return {'operation':op,'reason':reason,'expectedVersion':current['version'],'expectedSourceRevision':current['sourceModeration']['sourceRevision']}
 current=summary(participant_token);target=current['id'];controls=current['sourceModeration']
 request(f'/interactions/reports?sourceEvent={event}',expected=401)
 request('/interactions/reports?sourceEvent=0919143001',owner_token,expected=400)
 request('/interactions/reports?sourceEvent=919143002',participant_token,expected=404)
 ordinary,headers=request(f'/interactions/reports?sourceEvent={event}',participant_token)
 assert ordinary=={'supported':True,'canReview':False,'items':[],'nextCursor':None} and 'no-store' in headers.get('Cache-Control','')
 post={'operation':'source.report','reason':'SOURCE_SYNTHETIC_PRIVACY_CONCERN','expectedSourceRevision':controls['sourceRevision']};key=str(uuid.uuid4())
 command(None,post,expected=401);command(owner_token,post,expected=403)
 assert command(participant_token,post,key)['reported'] is True
 assert command(participant_token,post,key)['reported'] is True
 command(participant_token,post)
 command(participant_token,{**post,'reason':'Different key payload'},key,409)
 assert sql(f"SELECT count(*) FROM interaction_report WHERE target_id='{target}' AND comment_id IS NULL")=='1'
 assert sql(f"SELECT count(*) FROM interaction_audit WHERE target_id='{target}' AND operation='source.report'")=='1'
 # Preserve the default comment inbox, using an actual canonical comment/report.
 discussion=command(participant_token,{'operation':'comment.create','body':'Synthetic discussion for compatibility'})
 command(owner_token,{'operation':'comment.report','commentId':discussion['id'],'reason':'COMMENT_SYNTHETIC_CONCERN'})
 inbox,_=request('/interactions/reports',owner_token)
 assert 'COMMENT_SYNTHETIC_CONCERN' in json.dumps(inbox) and 'SOURCE_SYNTHETIC_PRIVACY_CONCERN' not in json.dumps(inbox)
 review,_=request(f'/interactions/reports?sourceEvent={event}',owner_token)
 assert review['items'][0]['id']==target and review['items'][0]['reasons']==['SOURCE_SYNTHETIC_PRIVACY_CONCERN']
 assert 'reporter' not in json.dumps(review).lower()
 record('Authenticated source reports deduplicate; default comment inbox and reporter privacy remain intact')
 hide=action('source.hide',owner_token)
 command(participant_token,hide,expected=403)
 original=request(f'/social-events/events/{event}/moments?extended',owner_token)[0]
 source=next(m for m in original if m['emId']==ident)
 request(moment_path,owner_token,body={'emeCaption':'Changed after source review','emeExpectedUpdatedAt':source['emUpdatedAt']},method='PUT')
 command(owner_token,hide,expected=409)
 review,_=request(f'/interactions/reports?sourceEvent={event}',owner_token)
 assert review['items'][0]['reportsReferToEarlierContent'] is True
 hide=action('source.hide',owner_token);hide_key=str(uuid.uuid4())
 command(owner_token,hide,hide_key);command(owner_token,hide,hide_key)
 for suffix in ('?extended',''):
  assert ident not in {m['emId'] for m in request(f'/social-events/events/{event}/moments'+suffix,participant_token)[0]}
 request(f'/public/interactions/targets/event_moment/{ident}',expected=404)
 request(f'/interactions/targets/event_moment/{ident}',participant_token,expected=404)
 request(moment_path+'/comments',participant_token,body={'emccBody':'Forbidden hidden comment'},method='POST',expected=404)
 request(moment_path,owner_token,body={'emeCaption':'Cannot bypass hidden state','emeExpectedUpdatedAt':source['emUpdatedAt']},method='PUT',expected=404)
 assert sql(f"SELECT interaction_notification_allowed('{target}',{owner},{participant},'comment')")=='f'
 assert request('/assets/serve/'+image['eiuPath'])[0]==png,'Hiding is not public-asset revocation'
 sql('UPDATE interaction_runtime SET enabled=false WHERE singleton')
 assert ident not in {m['emId'] for m in request(f'/social-events/events/{event}/moments?extended',participant_token)[0]}
 paused,_=request(f'/interactions/reports?sourceEvent={event}',owner_token);assert paused['items'][0]['source']['state']=='hidden'
 restore={'operation':'source.restore','reason':'Synthetic resolved concern','expectedVersion':paused['items'][0]['version'],
  'expectedSourceRevision':paused['items'][0]['source']['sourceRevision']}
 command(owner_token,restore,expected=404)
 sql('UPDATE interaction_runtime SET enabled=true WHERE singleton')
 restore_key=str(uuid.uuid4());command(owner_token,restore,restore_key);command(owner_token,restore,restore_key)
 command(owner_token,hide,hide_key,409)
 assert ident in {m['emId'] for m in request(f'/social-events/events/{event}/moments?extended',participant_token)[0]}
 record('Source hiding gates actual legacy/extended reads, comments, author edits and notification authority; pause cannot bypass it')
 # Reviewed reports reopen for real content changes, never restoration/no-op edits.
 source=next(m for m in request(f'/social-events/events/{event}/moments?extended',owner_token)[0] if m['emId']==ident)
 request(moment_path,owner_token,body={'emeCaption':source['emCaption'],'emeExpectedUpdatedAt':source['emUpdatedAt']},method='PUT')
 post={**post,'expectedSourceRevision':summary(participant_token)['sourceModeration']['sourceRevision']};command(participant_token,post)
 assert request(f'/interactions/reports?sourceEvent={event}',owner_token)[0]['items']==[]
 source=next(m for m in request(f'/social-events/events/{event}/moments?extended',owner_token)[0] if m['emId']==ident)
 request(moment_path,owner_token,body={'emeCaption':'New content after completed review','emeExpectedUpdatedAt':source['emUpdatedAt']},method='PUT')
 post={**post,'expectedSourceRevision':summary(participant_token)['sourceModeration']['sourceRevision']};command(participant_token,post)
 assert len(request(f'/interactions/reports?sourceEvent={event}',owner_token)[0]['items'])==1
 close={**action('source.report.resolve',owner_token),'decision':'dismissed'};close_key=str(uuid.uuid4())
 command(owner_token,close,close_key);command(owner_token,close,close_key)
 assert request(f'/interactions/reports?sourceEvent={event}',owner_token)[0]['items']==[]
 assert summary(participant_token)['sourceModeration']['state']=='visible'
 record('Content-version reports reopen only after real changes and can be closed without hiding; resolution replays are idempotent')
 # Two actual simultaneous requests with distinct keys and the same revisions.
 hide=action('source.hide',owner_token)
 audit_before=int(sql(f"SELECT count(*) FROM interaction_audit WHERE target_id='{target}' AND operation='source.hide'"))
 def competing_hide():
  local=urllib.request.build_opener(urllib.request.ProxyHandler({}))
  req=urllib.request.Request(origin+f'/interactions/targets/{target}/commands',data=json.dumps({'requestKey':str(uuid.uuid4()),'command':hide}).encode(),
   headers={'Content-Type':'application/json','Authorization':'Bearer '+owner_token},method='POST')
  try:r=local.open(req,timeout=20)
  except urllib.error.HTTPError as error:r=error
  r.read();return r.status
 with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:statuses=list(pool.map(lambda _:competing_hide(),range(2)))
 for status in statuses:report['requests'].append({'method':'POST','path':f'/interactions/targets/{target}/commands','status':status})
 assert sorted(statuses)==[200,409],statuses
 assert int(sql(f"SELECT count(*) FROM interaction_audit WHERE target_id='{target}' AND operation='source.hide'"))==audit_before+1
 review=request(f'/interactions/reports?sourceEvent={event}',owner_token)[0]['items'][0]
 restore={'operation':'source.restore','reason':'Synthetic concurrent review resolved','expectedVersion':review['version'],'expectedSourceRevision':review['source']['sourceRevision']}
 command(owner_token,restore)
 record('Competing real HTTP moderation requests serialize to one effect and one revision conflict')
 remove=action('source.remove',owner_token);command(participant_token,remove,expected=403);command(owner_token,remove)
 review=request(f'/interactions/reports?sourceEvent={event}',owner_token)[0]['items'][0]
 command(owner_token,{'operation':'source.restore','reason':'Synthetic platform restoration','expectedVersion':review['version'],'expectedSourceRevision':review['source']['sourceRevision']})
 # Real third identity owns a private parent; platform scope cannot invent membership.
 third=request('/signup',body={'firstName':'Synthetic','lastName':'private-owner','email':'memory.private@persona.test','password':'synthetic-private-password-141','termsAccepted':True,'termsVersion':'tdf-account-terms-v1'},method='POST')[0]
 third_id=third['partyId'];assert isinstance(third_id,int) and third_id>0
 sql(f"INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT {third_id},id,'bootstrap',true,now(),1 FROM security_role WHERE code='customer' ON CONFLICT DO NOTHING")
 sql(f"UPDATE social_event SET organizer_party_id='{third_id}' WHERE id=919143002")
 private=request('/social-events/events/919143002/moments',third['token'],body={'emCreateMediaType':'text','emCreateCaption':'Synthetic private source'},method='POST')[0]
 private_summary=request('/interactions/targets/event_moment/'+private['emId'],third['token'])[0]
 request('/interactions/reports?sourceEvent=919143002',owner_token,expected=404)
 request(f"/interactions/targets/{private_summary['id']}/commands",owner_token,body={'requestKey':str(uuid.uuid4()),'command':{'operation':'source.remove','reason':'Forbidden private scope','expectedVersion':private_summary['version'],'expectedSourceRevision':private_summary['sourceModeration']['sourceRevision']}},method='POST',expected=404)
 remove=action('source.remove',owner_token)
 sql(f"UPDATE party_security_role p SET active=false FROM security_role r WHERE p.role_id=r.id AND p.party_id={owner} AND r.code='admin'")
 command(owner_token,remove,expected=(401,403))
 assert sql(f"SELECT source_moderation_state FROM event_moment WHERE id={ident}")=='visible'
 # SQL NULL is not organizer authority, including a formerly authorized replay.
 sql(f'UPDATE social_event SET organizer_party_id=NULL WHERE id={event}')
 current=summary(participant_token);controls=current['sourceModeration']
 assert all(controls[name] is False for name in ('canHide','canRestore','canRemove'))
 assert request(f'/interactions/reports?sourceEvent={event}',participant_token)[0]=={'supported':True,'canReview':False,'items':[],'nextCursor':None}
 audit_before=int(sql(f"SELECT count(*) FROM interaction_audit WHERE target_id='{target}'"))
 for operation in ('source.hide','source.restore','source.remove','source.report.resolve'):
  denied={'operation':operation,'reason':'Synthetic unassigned authority attempt','expectedVersion':current['version'],'expectedSourceRevision':controls['sourceRevision']}
  if operation=='source.report.resolve':denied['decision']='dismissed'
  command(participant_token,denied,expected=403)
 command(owner_token,hide,hide_key,expected=403)
 assert sql(f"SELECT source_moderation_state FROM event_moment WHERE id={ident}")=='visible'
 assert int(sql(f"SELECT count(*) FROM interaction_audit WHERE target_id='{target}'"))==audit_before
 sql(f"UPDATE social_event SET organizer_party_id='{owner}' WHERE id={event}")
 record('Actual HTTP denies ownerless review/enforcement and former-organizer replay without effects')
 request(moment_path,owner_token,method='DELETE',expected=204)
 assert sql(f"SELECT retired_at IS NOT NULL FROM interaction_target WHERE id='{target}'")=='t'
 record('Actual private-parent denial, live moderator-role revocation and author deletion preserve scoped enforcement')

try:
 run(['createdb',*routing,db]);owned=True
 apply(root/'scripts/__tests__/fixtures/production-schema-20260814.sql')
 apply(root/'scripts/__tests__/fixtures/catalog-production-source-fixture.sql')
 batch=run(['node','scripts/render-production-migration-batch.mjs']);(out/'migrations.sql').write_bytes(batch);apply(out/'migrations.sql')
 installed_migration_ids=json.loads(sql("SELECT coalesce(jsonb_agg(migration_id ORDER BY migration_id),'[]'::jsonb) FROM tdf_schema_migration"))
 assert installed_migration_ids==expected_migration_ids,'The actual schema migration ledger must match the complete canonical manifest'
 report['migrationLedger']={'expectedCount':len(expected_migration_ids),'appliedCount':len(installed_migration_ids),
  'idsSha256':hashlib.sha256(json.dumps(installed_migration_ids,separators=(',',':')).encode()).hexdigest(),
  'batchSha256':hashlib.sha256(batch).hexdigest()}
 record('Actual complete migration ledger matches the canonical manifest; all migration sources and batch are bound')
 if sql("SELECT to_regprocedure('event_memory_dispatch_batch(integer)') IS NULL")=='t':apply(root/'tdf-hq/sql/2026-10-25_event_memory_invitations.sql')
 if a.source_moderation:
  assert sql("SELECT to_regprocedure('event_memory_source_command(bigint,uuid,uuid,jsonb)') IS NOT NULL")=='t','Canonical manifest must install source moderation'
  apply(root/'tdf-hq/sql/2026-10-25_event_memory_source_permission_locks.sql')
 authority_after_install=sql("SELECT jsonb_object_agg(signature,md5(pg_get_functiondef(signature::regprocedure))) FROM (SELECT signature FROM event_memory_moderation_authority UNION SELECT unnest(ARRAY['event_memory_source_context(uuid,bigint)','event_memory_source_reports(bigint,bigint,uuid,integer)','event_memory_source_command(bigint,uuid,uuid,jsonb)'])) definitions") if a.source_moderation else None
 start();report['actualVersion']=request('/version')[0]
 assert request('/session')[0] is None
 record('Actual anonymous session follows the nullable HTTP 200 response contract')
 accounts=[]
 for label in ('owner','participant'):
  account=request('/signup',body={'firstName':'Synthetic','lastName':label,'email':'memory.'+label+'@persona.test','password':'synthetic-memory-password-141','termsAccepted':True,'termsVersion':'tdf-account-terms-v1'},method='POST')[0]
  ident=account['partyId'];assert isinstance(ident,int) and ident>0 and account['token'];accounts.append((ident,account['token']))
 (owner,owner_token),(participant,participant_token)=accounts
 sql(f"INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT {owner},id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin' ON CONFLICT DO NOTHING")
 sql(f"INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT {participant},id,'bootstrap',true,now(),1 FROM security_role WHERE code='customer' ON CONFLICT DO NOTHING")
 sql(f"INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata) SELECT 919143001,'{owner}','HTTP synthetic memories',now()-interval '2 days',now()-interval '1 day',id,'00000000-0000-4000-8000-000000000238','{{\"isPublic\":true}}' FROM event_type WHERE code='concert'")
 sql(f"INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata) SELECT 919143002,'{owner}','Private HTTP fixture',now()-interval '2 days',now()-interval '1 day',id,'00000000-0000-4000-8000-000000000238','{{\"isPublic\":false}}' FROM event_type WHERE code='concert'")
 sql(f"INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata) SELECT 919143003,'{owner}','Suppressed HTTP fixture',now()-interval '2 days',now()-interval '1 day',id,'00000000-0000-4000-8000-000000000238','{{\"isPublic\":true}}' FROM event_type WHERE code='concert'")
 sql("INSERT INTO external_event_ref(provider,external_id,event_id,city,last_seen_at,missing_runs,source_status) VALUES('ticketmaster','memory-suppressed',919143003,'Quito',now(),0,'suppressed')")
 sql('UPDATE interaction_runtime SET enabled=true WHERE singleton')
 request('/social-events/events/919143001/moments?extended',expected=401)
 request('/social-events/events/919143002/moments?extended',participant_token,expected=404)
 request('/social-events/events/919143002/moments',participant_token,body={'emCreateMediaType':'text','emCreateCaption':'Forbidden private write'},method='POST',expected=404)
 request('/social-events/events/919143003/moments',owner_token,body={'emCreateMediaType':'text','emCreateCaption':'Forbidden suppressed write'},method='POST',expected=404)
 png=base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+jQ1sAAAAASUVORK5CYII=')
 image=upload(919143001,owner_token,'memory.png','image/png',png)
 assert '/moments/' in image['eiuPath']
 assert (out/'assets'/image['eiuPath']).read_bytes()==png
 fetched,_=request('/assets/serve/'+image['eiuPath']);assert fetched==png
 record('Public media bytes persist and are recoverable through the actual asset route')
 upload(919143001,owner_token,'forged.png','image/png',b'<html>invalid</html>',400)
 upload(919143002,owner_token,'private.png','image/png',png,403)
 upload(919143099,owner_token,'deleted-parent.png','image/png',png,404)
 upload(919143003,owner_token,'suppressed.png','image/png',png,404)
 assert not (out/'assets/social-events/events/919143003').exists()
 assert not (out/'assets/social-events/events/919143099').exists()
 record('Private and deleted parents cannot create public assets; forged signatures are rejected')
 audio=io.BytesIO()
 with wave.open(audio,'wb') as w:w.setnchannels(1);w.setsampwidth(2);w.setframerate(8000);w.writeframes(b'\0\0'*8000)
 sound=upload(919143001,owner_token,'memory.wav','audio/wav',audio.getvalue())
 video_path=out/'memory.mp4'
 run(['ffmpeg','-hide_banner','-loglevel','error','-f','lavfi','-i','color=c=black:s=16x16:r=1','-t','1','-pix_fmt','yuv420p',str(video_path)])
 video=upload(919143001,owner_token,'memory.mp4','video/mp4',video_path.read_bytes())
 quicktime_path=out/'memory.mov'
 run(['ffmpeg','-hide_banner','-loglevel','error','-i',str(video_path),'-c','copy',str(quicktime_path)])
 quicktime=upload(919143001,owner_token,'memory.mov','video/quicktime',quicktime_path.read_bytes())
 assert (out/'assets'/quicktime['eiuPath']).read_bytes()==quicktime_path.read_bytes()
 record('A real QuickTime MOV container uploads without depending on generic Drive storage')
 created=[]
 for kind,url in [('image',image['eiuPublicUrl']),('audio',sound['eiuPublicUrl']),('video',video['eiuPublicUrl']),('text','')]:
  moment=request('/social-events/events/919143001/moments',owner_token,body={'emCreateCaption':'Synthetic '+kind,'emCreateMediaType':kind,'emCreateMediaUrl':url},method='POST')[0]
  assert moment['emMediaType']==kind and moment['emAuthorPartyId']==str(owner);created.append(moment)
 extended,headers=request('/social-events/events/919143001/moments?extended',participant_token)
 assert len(extended)==4 and 'no-store' in headers.get('Cache-Control','')
 legacy,_=request('/social-events/events/919143001/moments',participant_token);assert {m['emMediaType'] for m in legacy}=={'image','video'}
 record('All four moment types persist; legacy feed remains compatible and authenticated reads are no-store')
 moment=created[-1];mid=moment['emId'];path=f'/social-events/events/919143001/moments/{mid}'
 edit={'emeCaption':'Edited synthetic memory','emeExpectedUpdatedAt':moment['emUpdatedAt']}
 request(path,participant_token,body=edit,method='PUT',expected=404)
 request(path,participant_token,method='DELETE',expected=404)
 updated,_=request(path,owner_token,body=edit,method='PUT');assert updated['emCaption']=='Edited synthetic memory'
 request(path,owner_token,body=edit,method='PUT',expected=409)
 request('/social-events/events/919143002/moments/'+mid,owner_token,method='DELETE',expected=404)
 record('Author edit/delete and cross-event IDOR guards apply; stale edits conflict')
 reaction_id=sql("SELECT id FROM reaction_type WHERE code='fire' AND active LIMIT 1");assert reaction_id
 for _ in range(2):request(f'/social-events/events/919143001/moments/{mid}/reactions',participant_token,body={'emrrReactionTypeId':reaction_id,'emrrActive':True},method='POST')
 request(f'/social-events/events/919143001/moments/{mid}/comments',participant_token,body={'emccBody':'Synthetic conversation'},method='POST')
 record('Existing canonical reactions and comments work for an extended text memory')
 preference,_=request('/social-events/me/memory-invitations',participant_token);assert preference['inApp'] is False
 request('/social-events/me/memory-invitations',participant_token,body=True,method='PUT')
 sql(f"INSERT INTO event_rsvp(event_id,party_id,status) VALUES(919143001,'{participant}','accepted')")
 request('/social-events/events/919143001/memory-invitations',participant_token,expected=404)
 request('/social-events/events/919143001/memory-invitations',owner_token,body=True,method='PUT')
 sql('SELECT event_memory_dispatch_batch(20)')
 assert sql(f'SELECT count(*) FROM event_memory_delivery WHERE event_id=919143001 AND party_id={participant}')=='1'
 sql('SELECT event_memory_dispatch_batch(20)')
 key=sql(f'SELECT id FROM event_memory_delivery WHERE event_id=919143001 AND party_id={participant}')
 request(f'/social-events/events/919143001/memory-invitations/{key}/open',participant_token,method='POST')
 request('/social-events/events/919143001/moments',participant_token,body={'emCreateMediaType':'text','emCreateCaption':'Participant memory'},method='POST')
 sql("UPDATE event_memory_delivery SET delivered_at=now()-interval '8 days' WHERE event_id=919143001")
 sql('SELECT event_memory_dispatch_batch(20)')
 summary,_=request('/social-events/events/919143001/memory-invitations',owner_token)
 assert summary['invitations']==1 and summary['reminders']==0 and summary['published']==1 and summary['opened']==1
 assert not any(k in summary for k in ('recipients','emails','parties'))
 record('Actual organizer API exposes aggregates; one synthetic inbox delivery and publication suppress reminder')
 request(path,owner_token,method='DELETE',expected=204)
 assert all(m['emId']!=mid for m in request('/social-events/events/919143001/moments?extended',owner_token)[0])
 stop();start()
 assert len(request('/social-events/events/919143001/moments?extended',participant_token)[0])==4
 assert request('/assets/serve/'+image['eiuPath'])[0]==png
 record('Source deletion persists; surviving moments and media remain after a real API restart')
 if a.source_moderation:source_checks(owner,owner_token,participant,participant_token,created[0])
 if a.source_moderation:
  refused=subprocess.run(['psql',*routing,'-d',db,'-Xq','-v','ON_ERROR_STOP=1','-f',str(root/'tdf-hq/sql/2026-10-25_event_memory_source_moderation_rollback.sql')],cwd=root,env=env,capture_output=True,timeout=30)
  assert refused.returncode!=0 and b'Retain source moderation authority after evidence' in refused.stderr
  assert sql("SELECT jsonb_object_agg(signature,md5(pg_get_functiondef(signature::regprocedure))) FROM (SELECT signature FROM event_memory_moderation_authority UNION SELECT unnest(ARRAY['event_memory_source_context(uuid,bigint)','event_memory_source_reports(bigint,bigint,uuid,integer)','event_memory_source_command(bigint,uuid,uuid,jsonb)'])) definitions")==authority_after_install
  record('Source rollback refuses after actual committed evidence and retains all installed privacy authorities')
 if a.browser:
  assert sql("SELECT count(*) FROM party WHERE lower(primary_email)='memory.newcomer@persona.test'")=='0','The browser signup identity must not already exist'
  # Explicit synthetic fan grants for the actual product UI, never production.
  sql(f"INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version) SELECT actor,id,'bootstrap',true,now(),1 FROM security_role CROSS JOIN (VALUES({owner}::bigint),({participant}::bigint)) a(actor) WHERE code='fan' ON CONFLICT DO NOTHING")
  browser_config=out/'browser-input.json';browser_config.write_text(json.dumps({'api':origin,'eventId':'919143001','output':str(out),'accounts':[{'email':'memory.owner@persona.test','password':'synthetic-memory-password-141'},{'email':'memory.participant@persona.test','password':'synthetic-memory-password-141'}]}));browser_config.chmod(0o600)
  driver=subprocess.Popen(['node','scripts/test-event-memory-source-browser.mjs',str(browser_config)],cwd=root,env=env,stdout=subprocess.PIPE,stderr=subprocess.PIPE,start_new_session=True)
  try:stdout,stderr=driver.communicate(timeout=240)
  except subprocess.TimeoutExpired:
   os.killpg(driver.pid,signal.SIGTERM)
   try:stdout,stderr=driver.communicate(timeout=10)
   except subprocess.TimeoutExpired:os.killpg(driver.pid,signal.SIGKILL);stdout,stderr=driver.communicate(timeout=10)
   raise AssertionError('Owned integrated browser deadline')
  (out/'browser-driver.log').write_bytes(stdout+stderr)
  assert driver.returncode==0,'Integrated browser failed; see its private evidence/log'
  report['browser']=json.loads((out/'browser-results.json').read_text());assert report['browser']['status']=='PASS'
  record('Actual desktop/phone product UI signs in, publishes, reports, hides and restores against this PostgreSQL/API instance')
  newcomer=report['browser']['newAccount'];actor=newcomer['partyId'];ident=newcomer['momentId'];asset_path=newcomer['assetPath']
  assert isinstance(actor,int) and not isinstance(actor,bool) and actor>0 and str(ident).isdigit()
  assert asset_path.startswith('/assets/serve/') and '..' not in asset_path
  assert sql(f"SELECT count(*) FROM event_moment WHERE id={int(ident)} AND event_id=919143001 AND author_party_id='{actor}'")=='1'
  assert sql("SELECT count(*) FROM party WHERE lower(primary_email)='memory.newcomer@persona.test'")=='1'
  stop();start()
  persisted=request('/social-events/events/919143001/moments?extended',owner_token)[0]
  assert any(str(m['emId'])==str(ident) and str(m['emAuthorPartyId'])==str(actor) for m in persisted)
  data=request(asset_path)[0];assert data==png and hashlib.sha256(data).hexdigest()==newcomer['assetSha256']
  report['newAccountPersistence']={'partyId':actor,'momentId':str(ident),'assetSha256':hashlib.sha256(data).hexdigest(),'survivesRealApiRestart':True,'fixtureRoleGrants':0}
  record('Browser-created account and its first photograph persist in PostgreSQL and durable storage across a real API restart without fixture role grants')
 assert json.loads(sql("SELECT coalesce(jsonb_agg(migration_id ORDER BY migration_id),'[]'::jsonb) FROM tdf_schema_migration"))==expected_migration_ids,'The migration ledger changed during the actual HTTP/browser execution'
 assert report['runtimeSources']==runtime_sources(),'Runtime sources changed during the HTTP execution'
 assert report['binarySha256']==hashlib.sha256(binary.read_bytes()).hexdigest(),'Candidate binary changed during the HTTP execution'
 report['status']='PASS'
finally:
 stop()
 report['runtimesStopped']=server is None or server.poll() is not None
 if owned:
  run(['dropdb',*routing,db]);report['ownedDatabaseRemoved']=True
 report['finishedAt']=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
 (out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps({'status':report['status'],'assertions':len(report['assertions']),'requests':len(report['requests']),'output':str(out)}))
