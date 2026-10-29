import concurrent.futures, json, tempfile, time, unittest
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from domain import Store, Denied, uid
from providers import Vault, Providers, ProviderError
from worker import run_one
from cryptography.fernet import Fernet

class CampaignTests(unittest.TestCase):
 def setUp(self):
  self.tmp=tempfile.TemporaryDirectory();self.store=Store(Path(self.tmp.name)/'db.sqlite');self.a={'id':1,'name':'Artista','admin':False};self.b={'id':2,'name':'Otra persona','admin':False};self.admin={'id':3,'name':'TDF','admin':True}
  for a in (self.a,self.b,self.admin):self.store.user(a)
  self.cid=self.store.create(self.a,{'name':'Música','objective':'Escuchar la sesión completa','destination':'https://www.youtube.com/watch?v=abc'})
  self.account=uid()
  with self.store.db(True) as d:d.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',(self.account,1,'instagram','ig-owner','artista','own','encrypted','[]',None,0))
 def tearDown(self):self.tmp.cleanup()
 def piece(self,**extra):
  data={'name':'Fragmento','platform':'instagram','format':'reel','account':self.account,'copy':'Sesión completa','due_at':time.time()-1,'late_seconds':60,**extra};return self.store.piece(self.a,self.cid,data)
 def approve(self,actor=None,**extra):self.store.approve(actor or self.a,self.cid,{'version':self.store.detail(self.a,self.cid)['version'],'rights_confirmed':True,**extra})
 def test_any_registered_user_can_create_without_artist_role(self):self.assertEqual(len(self.store.list(self.a)),1)
 def test_no_cross_user_or_blanket_admin_access(self):
  for actor in (self.b,self.admin):
   self.assertEqual(self.store.list(actor),[])
   with self.assertRaises(Denied):self.store.detail(actor,self.cid)
 def test_role_isolation_and_no_privilege_escalation(self):
  self.store.member(self.a,self.cid,{'user_id':2,'role':'viewer'})
  with self.assertRaises(Denied):self.store.piece(self.b,self.cid,{'platform':'instagram','format':'reel','due_at':time.time()})
  with self.assertRaises(Denied):self.store.member(self.b,self.cid,{'user_id':3,'role':'manager'})
 def test_approval_bound_to_exact_version(self):
  self.piece();self.approve();self.piece(name='Cambio')
  with self.assertRaises(Denied):self.store.status(self.a,self.cid,'active')
 def test_tdf_approval_cannot_be_forged_by_owner(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve(tdf_approve=True);self.store.status(self.a,self.cid,'active')
  with self.store.db() as d:
   p=d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone()
   with self.assertRaises(Denied):self.store.execution_guard(d,p)
 def test_private_message_needs_separate_consent(self):
  pid=self.piece(format='message',recipient='verified-owner');self.approve();self.store.status(self.a,self.cid,'active')
  with self.store.db() as d:
   with self.assertRaises(Denied):self.store.execution_guard(d,d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
 def test_tdf_admin_can_review_only_explicit_submissions(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.assertEqual(self.store.tdf_review(self.admin),[]);self.approve();pending=self.store.tdf_review(self.admin);self.assertEqual(len(pending),1)
  with self.assertRaises(Denied):self.store.tdf_review(self.b)
  with self.assertRaises(Denied):self.store.tdf_review(self.admin,pid,999)
  self.store.tdf_review(self.admin,pid,1);self.store.status(self.a,self.cid,'active')
  with self.store.db() as d:self.assertEqual(self.store.execution_guard(d,d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())['scope'],'tdf')
 def test_other_person_account_cannot_be_selected(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=2 WHERE id=?',(self.account,))
  with self.assertRaises(Denied):self.piece()
 def test_concurrent_claim_is_single(self):
  self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:claimed=list(pool.map(lambda _:self.store.claim(),range(8)))
  self.assertEqual(sum(x is not None for x in claimed),1)
 def test_crash_does_not_repeat_provider_action(self):
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active');self.store.claim()
  with self.store.db(True) as d:d.execute('UPDATE jobs SET claimed_at=?',(time.time()-1000,))
  self.assertIsNone(self.store.claim())
  with self.store.db() as d:self.assertEqual(d.execute('SELECT status FROM pieces WHERE id=?',(pid,)).fetchone()[0],'unknown')
 def test_pause_and_cancel_stop_future_jobs(self):
  self.piece();self.approve();self.store.status(self.a,self.cid,'active');self.store.status(self.a,self.cid,'paused');self.assertIsNone(self.store.claim());self.store.status(self.a,self.cid,'active');self.store.status(self.a,self.cid,'cancelled');self.assertIsNone(self.store.claim())
  with self.assertRaises(Denied):self.store.status(self.a,self.cid,'active')
 def test_quota_reservations_are_atomic(self):
  with self.store.db(True) as d:d.execute('UPDATE users SET storage_limit=100 WHERE id=1')
  def reserve(_):
   try:return self.store.reserve_asset(self.a,self.cid,'master.mp4',60)
   except Denied:return None
  with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:reserved=list(pool.map(reserve,range(4)))
  self.assertEqual(sum(x is not None for x in reserved),1)
 def test_late_or_revoked_jobs_do_not_publish(self):
  pid=self.piece(due_at=time.time()-3600);self.approve();self.store.status(self.a,self.cid,'active')
  class P:
   def publish(*args):raise AssertionError('No publication allowed')
  run_one(self.store,P(),lambda a:'')
  with self.store.db() as d:self.assertEqual(d.execute('SELECT state FROM jobs WHERE piece=?',(pid,)).fetchone()[0],'blocked')
 def test_uncertain_outcome_never_retries(self):
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class P:
   def publish(*args):raise ProviderError('timeout',True)
  run_one(self.store,P(),lambda a:'');self.assertIsNone(self.store.claim())
  with self.store.db() as d:self.assertEqual(d.execute('SELECT status FROM pieces WHERE id=?',(pid,)).fetchone()[0],'unknown')
 def test_encrypted_credentials_not_plaintext(self):
  key=Fernet.generate_key();v=Vault(key);value=v.seal({'access_token':'private-provider-secret'});self.assertNotIn('private-provider-secret',value);self.assertEqual(v.open(value)['access_token'],'private-provider-secret')
 def test_missing_oauth_client_is_explicit_block(self):
  p=Providers(self.store,Vault(Fernet.generate_key()),{'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform'})
  with self.assertRaises(Denied):p.start(self.a,'tiktok','own')
 def test_cannot_self_promote_to_tdf_account(self):
  p=Providers(self.store,Vault(Fernet.generate_key()),{'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform','GOOGLE_CLIENT_ID':'configured'})
  with self.assertRaises(Denied):p.start(self.a,'youtube','tdf')
 def test_plan_queue_is_durable_and_does_not_schedule_publications(self):
  from planning import queue_plan
  aid=self.store.reserve_asset(self.a,self.cid,'master.mp4',100)
  with self.store.db(True) as d:d.execute('UPDATE assets SET status="ready" WHERE id=?',(aid,))
  data={'source':aid,'start_at':'2030-01-01T18:30:00-05:00','facts':{'artist':'Banda'}}
  result=queue_plan(self.store,self.a,self.cid,data)
  with self.store.db() as d:
   self.assertEqual(d.execute('SELECT kind FROM jobs WHERE id=?',(result['job'],)).fetchone()[0],'plan')
   self.assertEqual(d.execute('SELECT count(*) FROM jobs WHERE kind="publish"').fetchone()[0],0)
  with self.assertRaises(Denied):queue_plan(self.store,self.a,self.cid,data)
 def test_plan_rejects_ambiguous_local_time(self):
  from planning import queue_plan
  aid=self.store.reserve_asset(self.a,self.cid,'master.mp4',100)
  with self.store.db(True) as d:d.execute('UPDATE assets SET status="ready" WHERE id=?',(aid,))
  with self.assertRaises(Denied):queue_plan(self.store,self.a,self.cid,{'source':aid,'start_at':'2030-01-01T18:30:00'})
 def test_preparation_is_not_publication(self):
  self.piece()
  with self.store.db() as d:self.assertEqual(d.execute('SELECT count(*) FROM jobs').fetchone()[0],0)
 def test_completed_render_binds_only_its_unapproved_draft(self):
  from media import bind_rendered_clip
  pid=self.piece(name='CL01',settings={'render_job':'render-owned'})
  wrong=self.piece(name='CL01',settings={'render_job':'other'})
  bind_rendered_clip(self.store,{'id':'render-owned','campaign':self.cid},{'label':'CL01'},'finished-asset')
  pieces={p['id']:p for p in self.store.detail(self.a,self.cid)['pieces']}
  self.assertEqual(pieces[pid]['asset'],'finished-asset');self.assertIsNone(pieces[wrong]['asset'])
  self.approve();bind_rendered_clip(self.store,{'id':'render-owned','campaign':self.cid},{'label':'CL01'},'late-asset')
  self.assertEqual(next(p for p in self.store.detail(self.a,self.cid)['pieces'] if p['id']==pid)['asset'],'finished-asset')
 def test_final_provider_guard_rechecks_pause(self):
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active');p=next(p for p in self.store.detail(self.a,self.cid)['pieces'] if p['id']==pid)
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{})
  adapter.effect_guard(p);self.store.status(self.a,self.cid,'paused')
  with self.assertRaises(Denied):adapter.effect_guard(p)
 def test_oauth_state_not_consumed_by_different_session(self):
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform','GOOGLE_CLIENT_ID':'client'})
  from urllib.parse import parse_qs,urlsplit
  state=parse_qs(urlsplit(adapter.start(self.a,'youtube','own')).query)['state'][0]
  with self.assertRaises(Denied):adapter.callback('youtube',state,'code',self.b)
  with self.store.db() as d:self.assertEqual(d.execute('SELECT used FROM oauth_states WHERE state=?',(state,)).fetchone()[0],0)
 def test_youtube_studio_confirmation_binds_video_and_tdf_admin(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET platform="youtube",scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece(platform='youtube',format='video',settings={'youtube_checks_reviewed':True})
  self.assertNotIn('youtube_checks_reviewed',json.loads(self.store.detail(self.a,self.cid)['pieces'][0]['settings']))
  self.approve()
  with self.store.db(True) as d:d.execute('UPDATE pieces SET external_id="private-video",status="blocked" WHERE id=?',(pid,))
  data={'piece':pid,'version':1,'video_id':'private-video','checks_passed':True}
  with self.assertRaises(Denied):self.store.youtube_checks(self.a,self.cid,data)
  self.store.member(self.a,self.cid,{'user_id':3,'role':'reviewer'})
  with self.assertRaises(Denied):self.store.youtube_checks(self.admin,self.cid,{**data,'video_id':'different'})
  self.store.youtube_checks(self.admin,self.cid,data)
  with self.store.db() as d:
   self.assertEqual(d.execute('SELECT kind FROM jobs WHERE piece=?',(pid,)).fetchone()[0],'verify_youtube')
   self.assertEqual(d.execute('SELECT status FROM pieces WHERE id=?',(pid,)).fetchone()[0],'processing')
 def test_analytics_preserves_pending_and_maps_actual_watch_time(self):
  from unittest.mock import patch
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{})
  public={'items':[{'statistics':{'viewCount':'8'},'snippet':{'publishedAt':'2026-10-28T00:00:00Z'}}]}
  report={'columnHeaders':[{'name':'estimatedMinutesWatched'},{'name':'subscribersGained'}],'rows':[[17.5,2]]}
  with patch.object(adapter,'token',return_value='test-only'),patch('providers.request',side_effect=[(public,{}),(report,{}),({'rows':[]},{}),({'rows':[]},{})]):
   result=adapter.metrics({'platform':'youtube','external_id':'video','account':self.account})
  self.assertEqual(result['watch_time'],17.5);self.assertEqual(result['attributed_subscribers'],2);self.assertEqual(result['retention']['status'],'pending');self.assertIsNone(result['thumbnail_ctr'])
if __name__=='__main__':unittest.main()
