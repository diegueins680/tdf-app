import concurrent.futures
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from domain import Store, Denied
import videos

class ArtistVideosTests(unittest.TestCase):
 def setUp(self):
  self.tmp=tempfile.TemporaryDirectory();self.store=Store(Path(self.tmp.name)/'db.sqlite');self.a={'id':1,'name':'Artist','admin':False};self.b={'id':2,'name':'Other','admin':False};self.admin={'id':9,'name':'TDF','admin':True}
  for a in (self.a,self.b,self.admin):self.store.user(a)
  self.cid=self.store.create(self.a,{'name':'Session','objective':'Watch'})
  self.data={'artist_id':1,'campaign':self.cid,'title':'Session','description':'Confirmed credits','source_url':'https://youtu.be/uRNyIMLiqU8'}
 def tearDown(self):self.tmp.cleanup()
 def publish(self,row,actor=None):videos.publish(self.store,actor or self.a,row['id'],{'version':row['version'],'rights_confirmed':True,'technical_reviewed':True},{'STATE_DIR':self.tmp.name})
 def test_draft_private_then_exact_version_public_and_edit_withdraws(self):
  row=videos.save(self.store,self.a,self.data);self.assertEqual(videos.list_public(self.store,1,'https://example.test'),[]);self.publish(row)
  public=videos.list_public(self.store,1,'https://example.test');self.assertEqual(public[0]['url'],'https://www.youtube.com/watch?v=uRNyIMLiqU8');self.assertNotIn('owner',public[0]);self.assertNotIn('campaign',public[0])
  edited=videos.save(self.store,self.a,{**self.data,'title':'Updated','version':1},row['id']);self.assertEqual(edited['version'],2);self.assertEqual(videos.list_public(self.store,1,''),[])
  with self.assertRaises(Denied):self.publish(row)
  with self.assertRaises(Denied):videos.save(self.store,self.a,{**self.data,'version':1},row['id'])
 def test_duplicate_concurrent_creations_reuse_one_identity(self):
  with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:rows=list(pool.map(lambda _:videos.save(self.store,self.a,self.data),range(4)))
  self.assertEqual(len({r['id'] for r in rows}),1)
  with self.assertRaises(Denied):videos.save(self.store,self.a,{**self.data,'title':'Changed'})
 def test_campaign_scope_never_granted_by_admin_status(self):
  for a in (self.b,self.admin):
   with self.assertRaises(Denied):videos.save(self.store,a,self.data)
 def test_tdf_scope_requires_current_admin_and_campaign_grant(self):
  row=videos.save(self.store,self.a,{**self.data,'scope':'tdf'})
  with self.assertRaises(Denied):self.publish(row)
  self.store.member(self.a,self.cid,{'user_id':9,'role':'reviewer'});self.publish(row,self.admin)
  self.assertEqual(len(videos.list_public(self.store,1,'')),1)
 def test_rights_and_technical_review_cannot_be_implied(self):
  row=videos.save(self.store,self.a,self.data)
  for extra in ({},{'rights_confirmed':True},{'technical_reviewed':True}):
   with self.assertRaises(Denied):videos.publish(self.store,self.a,row['id'],{'version':1,**extra},{})
 def test_artist_authority_rechecked_with_explicit_current_manager_capability(self):
  def core(headers,config,path):
   if path.endswith('/public'):return {'apArtistId':1}
   if '/party-profiles/' in path:return {'id':'canonical-profile'}
   return [{'id':'canonical-profile','kind':'artist','capabilities':{'edit':True}}]
  with patch('videos.core_json',side_effect=core):videos.authorize_artist(self.b,1,{},{});videos.authorize_artist(self.a,1,{},{});videos.authorize_artist(self.admin,1,{},{})
  with patch('videos.core_json',side_effect=[{'apArtistId':1},{'id':'canonical-profile'},[{'id':'canonical-profile','kind':'artist','capabilities':{'edit':False}}]]):
   with self.assertRaises(Denied):videos.authorize_artist(self.b,1,{},{})
  with patch('videos.core_json',side_effect=[{'apArtistId':1},{'id':'canonical-profile'},[{'id':'some-other-profile','kind':'artist','capabilities':{'edit':True}}]]):
   with self.assertRaises(Denied):videos.authorize_artist(self.b,1,{},{})
  with patch('videos.core_json',side_effect=[{'apArtistId':1},{},[{'kind':'artist','capabilities':{'edit':True}}]]):
   with self.assertRaises(Denied):videos.authorize_artist(self.b,1,{},{})
 def test_revoked_role_during_probe_does_not_reuse_admin_approval(self):
  row=videos.save(self.store,self.a,{**self.data,'scope':'tdf'})
  self.store.member(self.a,self.cid,{'user_id':9,'role':'reviewer'})
  with self.assertRaises(Denied):videos.publish(self.store,self.admin,row['id'],{'version':1,'rights_confirmed':True,'technical_reviewed':True},{},recheck=lambda:{**self.admin,'admin':False})
  self.assertEqual(videos.list_public(self.store,1,''),[])
 def test_file_must_be_finished_owned_same_campaign_and_verified(self):
  asset=self.store.reserve_asset(self.a,self.cid,'video.mp4',6)
  data={**self.data,'source_url':'','asset':asset}
  with self.assertRaises(Denied):videos.save(self.store,self.a,data)
  with self.store.db(True) as d:d.execute('UPDATE assets SET status="ready",size=6,uploaded=6 WHERE id=?',(asset,))
  row=videos.save(self.store,self.a,data)
  with self.assertRaises(Denied):videos.public_asset(self.store,row['id'])
  Path(self.tmp.name,'assets').mkdir();Path(self.tmp.name,'assets',asset).write_bytes(b'abcdef')
  probe={'format':{'duration':'12'},'streams':[{'codec_type':'video','codec_name':'h264'},{'codec_type':'audio','codec_name':'aac'}]}
  with patch('videos.subprocess.run',return_value=type('Probe',(),{'stdout':json.dumps(probe)})()):self.publish(row)
  self.assertEqual(videos.public_asset(self.store,row['id'])['id'],asset)
  other=self.store.create(self.a,{'name':'Other','objective':'Watch'})
  with self.assertRaises(Denied):videos.save(self.store,self.a,{**data,'campaign':other})
 def test_provider_identity_and_url_injection(self):
  for url,provider in [('https://youtu.be/uRNyIMLiqU8','youtube'),('https://vimeo.com/123456789/privatehash','vimeo'),('https://www.instagram.com/reel/DfGMOaVDhJ9/','instagram'),('https://www.tiktok.com/@tdf.records.label/video/7703720333777325330','tiktok')]:self.assertTrue(videos.external_source(url)[0].startswith(provider+':'))
  for url in ('javascript:alert(1)','https://youtube.com.evil.test/watch?v=uRNyIMLiqU8','https://user:secret@youtube.com/watch?v=uRNyIMLiqU8','http://youtu.be/uRNyIMLiqU8','https://127.0.0.1/evil.mp4','https://vm.tiktok.com/short','https://youtube.com/playlist?list=x','https://instagram.com/stories/account/123'):
   with self.assertRaises(Denied):videos.external_source(url)

if __name__=='__main__':unittest.main()
