import concurrent.futures, contextlib, hashlib, http.client, io, json, tempfile, threading, unittest, urllib.request, urllib.error
from pathlib import Path
from unittest.mock import patch
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from server import Handler, Server, PREFIX, upstream_actor
from domain import Store, Denied, uid
from providers import Providers, Vault
from cryptography.fernet import Fernet

class HttpBoundaryTests(unittest.TestCase):
 def setUp(self):
  self.tmp=tempfile.TemporaryDirectory();self.config={'STATE_DIR':self.tmp.name,'MEDIA_KEY':'test-key','ALLOWED_ORIGINS':['https://www.tdfrecords.net'],'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform'};self.store=Store(Path(self.tmp.name)/'db.sqlite');self.p=Providers(self.store,Vault(Fernet.generate_key()),self.config);self.server=Server(('127.0.0.1',0),self.store,self.p,self.config);self.thread=threading.Thread(target=self.server.serve_forever,daemon=True);self.thread.start();self.base='http://127.0.0.1:'+str(self.server.server_port)+PREFIX
  def auth(headers,config):
   token=headers.get('Authorization')
   if token not in ('Bearer user1','Bearer user2'):
    from domain import Denied
    raise Denied('No autenticado',401)
   return {'id':int(token[-1]),'name':'Usuario '+token[-1],'admin':False}
  self.patch=patch('server.upstream_actor',auth);self.patch.start()
 def tearDown(self):self.patch.stop();self.server.shutdown();self.server.server_close();self.thread.join();self.tmp.cleanup()
 def req(self,path,body=None,method=None,who='user1',origin='https://www.tdfrecords.net',raw=None,headers=None):
  h={'Authorization':'Bearer '+who,'Origin':origin,**(headers or {})};data=json.dumps(body).encode() if body is not None else raw
  if body is not None:h['Content-Type']='application/json'
  r=urllib.request.Request(self.base+path,data=data,method=method,headers=h)
  try:
   with urllib.request.urlopen(r,timeout=10) as result:return result.status,result.read(),dict(result.headers)
  except urllib.error.HTTPError as e:return e.code,e.read(),dict(e.headers)
 def create(self):
  return json.loads(self.req('/campaigns',{'name':'Lanzamiento','objective':'Escuchar música'})[1])['id']
 @contextlib.contextmanager
 def pending_upload(self,identifier,who='user1'):
  reading=threading.Event();original_setup=Handler.setup
  class Reader:
   def __init__(self,handler,stream):self.handler=handler;self.stream=stream
   def __getattr__(self,name):return getattr(self.stream,name)
   def read(self,size=-1):
    if self.handler.command=='PUT' and self.handler.path.endswith('/uploads/'+identifier):reading.set()
    return self.stream.read(size)
  def setup(handler):
   original_setup(handler);handler.rfile=Reader(handler,handler.rfile)
  connection=http.client.HTTPConnection('127.0.0.1',self.server.server_port,timeout=5)
  finished=False
  with patch.object(Handler,'setup',setup):
   try:
    connection.putrequest('PUT',PREFIX+'/uploads/'+identifier)
    for key,value in {'Authorization':'Bearer '+who,'Origin':'https://www.tdfrecords.net','Content-Length':'6','Upload-Offset':'0'}.items():connection.putheader(key,value)
    connection.endheaders();connection.send(b'abc')
    self.assertTrue(reading.wait(2),'Upload did not reach its body read')
    def finish():
     nonlocal finished
     connection.send(b'def');response=connection.getresponse();body=response.read();finished=True
     return response.status,body
    yield finish
   finally:
    if not finished:
     try:connection.send(b'def');connection.getresponse().read()
     except OSError:pass
    connection.close()
 def test_slow_upload_keeps_other_users_and_worker_writes_available(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'slow.mp4','size':6})[1])['id']
  with self.store.db(True) as db:db.execute('INSERT INTO jobs(id,kind,payload,due_at) VALUES(?,"heartbeat","{}",0)',(uid(),))
  pool=concurrent.futures.ThreadPoolExecutor(max_workers=2)
  try:
   with self.pending_upload(aid) as finish:
    session=pool.submit(self.req,'/session',who='user2');claim=pool.submit(self.store.claim)
    self.assertEqual(session.result(timeout=2)[0],200)
    self.assertEqual(claim.result(timeout=2)['kind'],'heartbeat')
    self.assertEqual(self.req('/uploads/'+aid,method='PUT',raw=b'xyz',headers={'Upload-Offset':'0'})[0],409)
    self.assertEqual(self.req('/assets/'+aid,{},method='DELETE')[0],409)
    self.assertEqual(finish()[0],200)
  finally:pool.shutdown(wait=True)
  self.assertEqual(self.req('/assets/'+aid)[1],b'abcdef')
  with self.store.db() as db:
   row=db.execute('SELECT uploaded,status,sha256 FROM assets WHERE id=?',(aid,)).fetchone()
   self.assertEqual(tuple(row),(6,'ready',hashlib.sha256(b'abcdef').hexdigest()))
 def test_upload_rechecks_editor_permission_after_receiving_bytes(self):
  cid=self.create();self.req('/session',who='user2')
  owner={'id':1,'name':'Usuario 1','admin':False}
  self.store.member(owner,cid,{'user_id':2,'role':'editor'})
  aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'revoked.mp4','size':6})[1])['id']
  with self.pending_upload(aid,who='user2') as finish:
   self.store.member(owner,cid,{'user_id':2,'role':'viewer'})
   self.assertEqual(finish()[0],404)
  with self.store.db() as db:
   row=db.execute('SELECT uploaded,reserved,status FROM assets WHERE id=?',(aid,)).fetchone()
   self.assertEqual(tuple(row),(0,6,'uploading'))
  self.assertFalse((Path(self.tmp.name)/'assets'/aid).exists())
 def test_failed_offset_commit_keeps_reservation_and_retry_replaces_uncommitted_tail(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'retry.mp4','size':6})[1])['id']
  with self.store.db(True) as db:db.execute('CREATE TRIGGER reject_upload_commit BEFORE UPDATE OF uploaded ON assets BEGIN SELECT RAISE(ABORT,"synthetic commit failure"); END')
  self.assertEqual(self.req('/uploads/'+aid,method='PUT',raw=b'badbad',headers={'Upload-Offset':'0'})[0],500)
  with self.store.db(True) as db:
   row=db.execute('SELECT uploaded,reserved,status FROM assets WHERE id=?',(aid,)).fetchone()
   self.assertEqual(tuple(row),(0,6,'uploading'));db.execute('DROP TRIGGER reject_upload_commit')
  self.assertEqual(self.req('/uploads/'+aid,method='PUT',raw=b'abc',headers={'Upload-Offset':'0'})[0],200)
  self.assertEqual((Path(self.tmp.name)/'assets'/aid).read_bytes(),b'abc')
  self.assertEqual(self.req('/uploads/'+aid,method='PUT',raw=b'def',headers={'Upload-Offset':'3'})[0],200)
  self.assertEqual(self.req('/assets/'+aid)[1],b'abcdef')
 def test_tiktok_photo_references_protect_each_asset_including_provider_fetch(self):
  cid=self.create();photos=[]
  for name in ('cover.jpg','second.jpg'):
   aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':name,'size':3})[1])['id'];photos.append(aid)
   self.assertEqual(self.req('/uploads/'+aid,method='PUT',raw=b'jpg',headers={'Upload-Offset':'0'})[0],200)
  pid=uid()
  with self.store.db(True) as db:db.execute('INSERT INTO pieces(id,campaign,name,platform,format,copy,destination,due_at,settings,created_at) VALUES(?,?,"Photos","tiktok","photo","","",1,?,1)',(pid,cid,json.dumps({'tiktok':{'images':photos}})))
  for status in ('draft','approved','publishing','processing','published'):
   with self.subTest(status=status):
    with self.store.db(True) as db:db.execute('UPDATE pieces SET status=? WHERE id=?',(status,pid))
    for aid in photos:
     self.assertEqual(self.req('/assets/'+aid,{},method='DELETE')[0],409)
     self.assertEqual(self.req('/assets/'+aid)[1],b'jpg')
  with self.store.db() as db:self.assertEqual(db.execute('SELECT sum(reserved) FROM assets WHERE campaign=?',(cid,)).fetchone()[0],6)
 def test_unauthenticated_session_denied(self):self.assertEqual(self.req('/session',who='bad')[0],401)
 def core_session(self,payload,headers):
  # Exercise the real adapter and HTTP handler; only Core's response is synthetic.
  self.patch.stop();self.config['AUTH_URL']='https://core.persona.test/session'
  def response(request,timeout):
   self.assertEqual(request.full_url,self.config['AUTH_URL']);self.assertEqual(timeout,10)
   for name,value in headers.items():self.assertEqual(request.get_header(name),value)
   return io.BytesIO(json.dumps(payload).encode())
  with patch('server.urllib.request.urlopen',side_effect=response):
   connection=http.client.HTTPConnection('127.0.0.1',self.server.server_port,timeout=5)
   try:
    connection.request('GET',PREFIX+'/session',headers=headers);reply=connection.getresponse()
    return reply.status,json.loads(reply.read())
   finally:connection.close()
 def test_null_core_session_returns_401_without_creating_an_actor(self):
  for headers in ({'Cookie':'tdf_session=synthetic-expired'},{'Authorization':'Bearer synthetic-revoked'}):
   with self.subTest(headers=headers):
    status,body=self.core_session(None,headers)
    self.assertEqual(status,401);self.assertIn('iniciar sesión',body['error'])
  with self.store.db() as d:self.assertEqual(d.execute('SELECT count(*) FROM users').fetchone()[0],0)
 def test_nonobject_core_session_rejects_then_accepts_a_valid_recovered_actor(self):
  headers={'Cookie':'tdf_session=synthetic-only'}
  for payload in ([],['Admin'],'session',0,False):
   with self.subTest(payload=payload):self.assertEqual(self.core_session(payload,headers)[0],401)
  with self.store.db() as d:self.assertEqual(d.execute('SELECT count(*) FROM users').fetchone()[0],0)
  status,body=self.core_session({'partyId':7,'displayName':'Recovered synthetic listener','roles':['Customer']},headers)
  self.assertEqual(status,200);self.assertEqual(body['user'],{'id':7,'name':'Recovered synthetic listener','admin':False})
  with self.store.db() as d:self.assertEqual(d.execute('SELECT count(*) FROM users WHERE id=7').fetchone()[0],1)
 def test_upstream_failure_remains_distinct_from_an_absent_session(self):
  for code,expected in ((401,401),(403,401),(500,503)):
   with self.subTest(code=code),patch('server.urllib.request.urlopen',side_effect=urllib.error.HTTPError('https://core.persona.test/session',code,'synthetic',{},None)):
    with self.assertRaises(Denied) as error:upstream_actor({'Cookie':'synthetic'},self.config)
    self.assertEqual(error.exception.status,expected)
  with patch('server.urllib.request.urlopen',return_value=io.BytesIO(b'not-json')):
   with self.assertRaises(Denied) as error:upstream_actor({'Cookie':'synthetic'},self.config)
   self.assertEqual(error.exception.status,503)
 def test_sandbox_prefix_and_designated_admin_are_enforced_over_http(self):
  state=Path(self.tmp.name)/'tdf-campaign-platform-sandbox';state.mkdir()
  self.config.update(STATE_DIR=str(state),PUBLIC_BASE='https://api.tdfrecords.net/campaign-platform-sandbox',TIKTOK_MODE='sandbox',TIKTOK_CLIENT_AUDITED=False,TIKTOK_SANDBOX_ADMIN_ID=1,TIKTOK_SANDBOX_USERNAME='artista')
  self.store=Store(state/'campaigns.sqlite3');self.server.store=self.store;self.server.providers.store=self.store
  original=self.base;self.base=original+'-sandbox'
  self.assertEqual(self.req('/session')[0],403)
  with patch('server.upstream_actor',return_value={'id':1,'name':'Admin de prueba','admin':True}):
   self.assertEqual(self.req('/session')[0],200)
   self.assertEqual(self.req('/app.js')[0],200)
   self.base=original;self.assertEqual(self.req('/session')[0],404)
 def test_cross_user_record_hidden(self):
  cid=self.create();self.assertEqual(self.req('/campaigns/'+cid,who='user2')[0],404)
 def test_cross_origin_mutation_denied(self):self.assertEqual(self.req('/campaigns',{'name':'Bad','objective':'Bad'},origin='https://evil.example')[0],403)
 def test_cors_has_no_wildcard(self):self.assertNotEqual(self.req('/session',origin='https://evil.example')[2].get('Access-Control-Allow-Origin'),'*')
 def test_resume_offsets_hash_and_private_download(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'clip.mp4','size':6})[1])['id'];path='/uploads/'+aid
  self.assertEqual(self.req(path,method='PUT',raw=b'abc',headers={'Upload-Offset':'0'})[0],200)
  self.assertEqual(self.req(path,method='PUT',raw=b'bad',headers={'Upload-Offset':'0'})[0],409)
  self.assertEqual(json.loads(self.req(path)[1])['offset'],3)
  self.assertEqual(self.req(path,method='PUT',raw=b'def',headers={'Upload-Offset':'3'})[0],200)
  self.assertEqual(self.req('/assets/'+aid)[1],b'abcdef');self.assertEqual(self.req('/assets/'+aid,who='user2')[0],404)
  self.assertEqual(self.req('/assets/'+aid,headers={'Range':'bytes=2-4'})[1],b'cde')
 def test_html_upload_does_not_execute_in_app_origin(self):
  cid=self.create();content=b'<script>fetch("/session")</script>';aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'attack.html','size':len(content)})[1])['id'];self.req('/uploads/'+aid,method='PUT',raw=content,headers={'Upload-Offset':'0'});status,body,h=self.req('/assets/'+aid);self.assertEqual(status,200);self.assertEqual(h['Content-Type'],'application/octet-stream');self.assertTrue(h['Content-Disposition'].startswith('attachment'));self.assertIn('sandbox',h['Content-Security-Policy'])
 def test_traversal_and_unsigned_media_denied(self):
  self.assertEqual(self.req('/../schema.sql')[0],404);self.assertEqual(self.req('/media/'+uid())[0],403)
 def test_unapproved_work_not_scheduled(self):
  cid=self.create();self.assertEqual(self.req('/campaigns/'+cid+'/status',{'status':'active'})[0],409)
 def test_javascript_destination_refused(self):self.assertEqual(self.req('/campaigns',{'name':'X','objective':'X','destination':'javascript:alert(1)'})[0],400)
 def test_backup_controls_require_current_admin_authority(self):
  self.assertEqual(self.req('/backups')[0],403)
  for path in ('/backups/key','/backups/key/challenge','/backups/key/confirm','/backups/enable','/backups/run'):
   self.assertEqual(self.req(path,{})[0],403)
 def test_tiktok_review_endpoints_keep_campaign_authority_and_private_credentials(self):
  cid=self.create();aid=uid()
  with self.store.db(True) as db:db.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',(aid,1,'tiktok','creator','Artista','own',self.p.vault.seal({'access_token':'test-only'}),'"video.publish"',None,0))
  with patch('tiktok.request') as network:
   self.assertEqual(self.req('/campaigns/'+cid+'/tiktok-creator',{'account':aid},who='user2')[0],404)
   self.assertEqual(self.req('/campaigns/'+cid+'/tiktok-consent',{'account':aid},who='user2')[0],404)
   self.assertEqual(self.req('/campaigns/'+cid+'/tiktok-creator',{'account':aid})[0],409)
   self.assertEqual(self.req('/campaigns/'+cid+'/tiktok-creator',{'account':aid},origin='https://evil.example')[0],403)
   network.assert_not_called()
  status,body,headers=self.req('/tiktok.js');self.assertEqual(status,200);self.assertIn('script-src',headers['Content-Security-Policy']);self.assertNotIn(b'test-only',body)
 def test_delete_original_requires_owner_and_explicit_confirmation(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'original.mp4','size':3,'original':True})[1])['id'];self.req('/uploads/'+aid,method='PUT',raw=b'abc',headers={'Upload-Offset':'0'})
  self.assertEqual(self.req('/assets/'+aid,{},method='DELETE',who='user2')[0],404)
  self.assertEqual(self.req('/assets/'+aid,{},method='DELETE')[0],400)
  self.assertEqual(self.req('/assets/'+aid,{'delete_original':True},method='DELETE')[0],204)
  self.assertEqual(self.req('/assets/'+aid)[0],404)
  with self.store.db() as d:self.assertEqual(d.execute('SELECT reserved FROM assets WHERE id=?',(aid,)).fetchone()[0],0)
 def test_video_library_anonymous_read_drafts_hidden_scope_and_revocation(self):
  self.assertEqual(self.req('/videos?artist_id=invalid')[0],400)
  self.assertEqual(self.req('/public/videos?artist_id=invalid',who='bad')[0],400)
  cid=self.create();data={'artist_id':1,'campaign':cid,'title':'Session','source_url':'https://youtu.be/uRNyIMLiqU8'}
  with patch('videos.authorize_artist') as authority:
   status,body,_=self.req('/videos',data);self.assertEqual(status,201);row=json.loads(body);authority.assert_called_once()
   self.assertEqual(json.loads(self.req('/public/videos?artist_id=1',who='bad')[1]),[])
   self.assertEqual(self.req('/videos/'+row['id']+'/publish',{'version':1,'rights_confirmed':True,'technical_reviewed':True})[0],200)
   public=json.loads(self.req('/public/videos?artist_id=1',who='bad')[1]);self.assertEqual(public[0]['id'],row['id']);self.assertNotIn('campaign',public[0]);self.assertNotIn('owner',public[0])
   self.assertEqual(self.req('/videos/'+row['id']+'/publish',{'version':1,'rights_confirmed':True,'technical_reviewed':True},who='user2')[0],404)
   self.assertEqual(self.req('/videos/'+row['id'],{**data,'version':1,'description':'New credits'},method='PUT')[0],200)
   self.assertEqual(json.loads(self.req('/public/videos?artist_id=1',who='bad')[1]),[])
   self.assertEqual(self.req('/videos/'+row['id']+'/publish',{'version':1,'rights_confirmed':True,'technical_reviewed':True})[0],409)
  from domain import Denied
  with patch('videos.authorize_artist',side_effect=Denied('Permission revoked',403)):
   self.assertEqual(self.req('/videos/'+row['id']+'/publish',{'version':2,'rights_confirmed':True,'technical_reviewed':True})[0],403)
 def test_public_video_media_range_and_withdrawal_without_exposing_original(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'video.mp4','size':6})[1])['id'];self.req('/uploads/'+aid,method='PUT',raw=b'abcdef',headers={'Upload-Offset':'0'})
  with patch('videos.authorize_artist'):
   row=json.loads(self.req('/videos',{'artist_id':1,'campaign':cid,'title':'File','asset':aid})[1])
   media='/public/videos/'+row['id']+'/media';self.assertEqual(self.req(media,who='bad')[0],404)
   with patch('videos.probe_asset'):
    self.assertEqual(self.req('/videos/'+row['id']+'/publish',{'version':1,'rights_confirmed':True,'technical_reviewed':True})[0],200)
   self.assertEqual(self.req(media,who='bad',headers={'Range':'bytes=2-4'})[:2],(206,b'cde'))
   self.assertEqual(self.req('/assets/'+aid,who='bad')[0],401)
   self.assertEqual(self.req('/assets/'+aid,{'delete_original':True},method='DELETE')[0],409)
   self.assertEqual(self.req('/videos/'+row['id']+'/unpublish',{'version':1})[0],204)
   self.assertEqual(self.req(media,who='bad')[0],404)
if __name__=='__main__':unittest.main()
