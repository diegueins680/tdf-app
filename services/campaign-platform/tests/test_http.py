import json, tempfile, threading, unittest, urllib.request, urllib.error
from pathlib import Path
from unittest.mock import patch
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from server import Server, PREFIX, upstream_actor
from domain import Store, uid
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
 def test_unauthenticated_session_denied(self):self.assertEqual(self.req('/session',who='bad')[0],401)
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
 def test_delete_original_requires_owner_and_explicit_confirmation(self):
  cid=self.create();aid=json.loads(self.req('/campaigns/'+cid+'/uploads',{'name':'original.mp4','size':3,'original':True})[1])['id'];self.req('/uploads/'+aid,method='PUT',raw=b'abc',headers={'Upload-Offset':'0'})
  self.assertEqual(self.req('/assets/'+aid,{},method='DELETE',who='user2')[0],404)
  self.assertEqual(self.req('/assets/'+aid,{},method='DELETE')[0],400)
  self.assertEqual(self.req('/assets/'+aid,{'delete_original':True},method='DELETE')[0],204)
  self.assertEqual(self.req('/assets/'+aid)[0],404)
  with self.store.db() as d:self.assertEqual(d.execute('SELECT reserved FROM assets WHERE id=?',(aid,)).fetchone()[0],0)
if __name__=='__main__':unittest.main()
