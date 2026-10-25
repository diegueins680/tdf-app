#!/usr/bin/env python3
"""Actual API and durable media checks; only an owned disposable PostgreSQL DB."""
import argparse, base64, hashlib, io, json, os, pathlib, signal, subprocess, time, urllib.error, urllib.request, wave
p=argparse.ArgumentParser();p.add_argument('--binary',required=True);p.add_argument('--output',required=True);a=p.parse_args()
root=pathlib.Path(__file__).resolve().parent.parent
binary=pathlib.Path(a.binary).resolve();assert binary.is_file()
out=pathlib.Path(a.output).resolve();out.mkdir(mode=0o700,parents=False,exist_ok=False)
ci=os.environ.get('GITHUB_ACTIONS')=='true'
assert not any(os.environ.get(k) for k in ('PGHOSTADDR','PGSERVICE','PGSERVICEFILE'))
host='postgres' if ci else '127.0.0.1';port='5432' if ci else os.environ.get('TDF_TEST_NATIVE_POSTGRES_PORT','55486')
assert port.isdigit() and 0<int(port)<65536
role='postgres' if ci else os.environ.get('TDF_TEST_NATIVE_POSTGRES_USER','macpro')
assert role.replace('_','').isalnum()
env={k:v for k,v in os.environ.items() if k in ('HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR')}
if ci:env['PGPASSWORD']=os.environ['TDF_TEST_POSTGRES_PASSWORD']
db='tdf_event_memories_http_test';routing=['-h',host,'-p',port,'-U',role]
api_port='18886';origin='http://127.0.0.1:'+api_port
opener=urllib.request.build_opener(urllib.request.ProxyHandler({}))
report={'source':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'binarySha256':hashlib.sha256(binary.read_bytes()).hexdigest(),'requests':[],'assertions':[],'externalProviderCalls':0}
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
 assert r.status==expected,(method,path,r.status,data[:300])
 result=json.loads(data) if 'json' in r.headers.get('Content-Type','') and data else data
 return result,r.headers

def start():
 global server,log
 assets=out/'assets';assets.mkdir(exist_ok=True)
 variables={**env,'APP_ENV':'test','DATABASE_URL':f'postgresql://{role}@{host}:{port}/{db}','APP_PORT':api_port,
  'RESET_DB':'false','RUN_MIGRATIONS':'false','AUTO_APPLY_PRODUCTION_MIGRATIONS':'false','SEED_DB':'false','DEFAULT_LOCALE':'es',
  'ALLOWED_ORIGINS':'http://127.0.0.1:5197','HQ_APP_URL':'http://127.0.0.1:5197','HQ_ASSETS_DIR':str(assets),
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

try:
 run(['createdb',*routing,db]);owned=True
 apply(root/'scripts/__tests__/fixtures/production-schema-20260814.sql')
 apply(root/'scripts/__tests__/fixtures/catalog-production-source-fixture.sql')
 batch=run(['node','scripts/render-production-migration-batch.mjs']);(out/'migrations.sql').write_bytes(batch);apply(out/'migrations.sql')
 if sql("SELECT to_regprocedure('event_memory_dispatch_batch(integer)') IS NULL")=='t':apply(root/'tdf-hq/sql/2026-10-25_event_memory_invitations.sql')
 start();report['actualVersion']=request('/version')[0]
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
 report['status']='PASS'
finally:
 stop()
 report['runtimesStopped']=server is None or server.poll() is not None
 if owned:
  run(['dropdb',*routing,db]);report['ownedDatabaseRemoved']=True
 report['finishedAt']=time.strftime('%Y-%m-%dT%H:%M:%SZ',time.gmtime())
 (out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps({'status':report['status'],'assertions':len(report['assertions']),'requests':len(report['requests']),'output':str(out)}))
