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
 def test_worker_block_keeps_owner_approved_shared_piece_in_tdf_review(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class NoProvider:
   def publish(*args):raise AssertionError('TDF review must precede provider publication')
  self.assertEqual([p['id'] for p in self.store.tdf_review(self.admin)],[pid])
  self.assertTrue(run_one(self.store,NoProvider(),lambda a:''))
  with self.store.db() as d:
   before=dict(d.execute('SELECT * FROM jobs WHERE piece=?',(pid,)).fetchone())
   piece=dict(d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
  self.assertEqual((piece['status'],piece['external_id'],piece['approved_version'],piece['tdf_approval_version']),('blocked',None,1,None))
  self.assertEqual(before['state'],'blocked')
  self.assertEqual([p['id'] for p in self.store.tdf_review(self.admin)],[pid])
  self.assertEqual(self.store.tdf_review(self.admin,pid,1),{'approved':True})
  with self.store.db() as d:
   after=dict(d.execute('SELECT * FROM jobs WHERE piece=?',(pid,)).fetchone())
   piece=dict(d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
  self.assertEqual(after,before)
  self.assertEqual((piece['status'],piece['external_id'],piece['version'],piece['approved_version'],piece['tdf_approval_version']),('blocked',None,1,1,1))
  self.assertEqual(self.store.tdf_review(self.admin),[]);self.assertIsNone(self.store.claim())
  # Administrative review alone cannot replay a provider operation. The owner
  # explicitly reapproves/reactivates through the existing durable recovery path.
  self.approve();self.store.status(self.a,self.cid,'active')
  with self.store.db() as d:
   job=d.execute('SELECT * FROM jobs WHERE piece=?',(pid,)).fetchone()
   self.assertEqual((job['id'],job['state']),(before['id'],'queued'))
   self.assertEqual(self.store.execution_guard(d,d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())['scope'],'tdf')
 def test_tdf_review_includes_only_safe_blocked_or_original_approved_submissions(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve()
  for state in ('draft','approved','blocked','publishing','processing','unknown','published','cancelled'):
   for external in (None,'existing-provider-effect'):
    with self.subTest(state=state,external=external):
     with self.store.db(True) as d:d.execute('UPDATE pieces SET status=?,external_id=? WHERE id=?',(state,external,pid))
     pending=self.store.tdf_review(self.admin)
     self.assertEqual([p['id'] for p in pending],[pid] if state=='approved' or (state=='blocked' and external is None) else [])
 def test_blocked_shared_review_keeps_admin_and_exact_version_authority(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class NoProvider:
   def publish(*args):raise AssertionError('No publication before administrator approval')
  run_one(self.store,NoProvider(),lambda a:'')
  for actor in (self.a,self.b):
   with self.assertRaises(Denied) as denied:self.store.tdf_review(actor,pid,1)
   self.assertEqual(denied.exception.status,403)
  with self.assertRaises(Denied) as denied:self.store.tdf_review(self.admin,pid,999)
  self.assertEqual(denied.exception.status,409)
  self.store.piece(self.a,self.cid,{'name':'Changed submission','platform':'instagram','format':'reel','account':self.account,'copy':'Changed','due_at':time.time()-1,'late_seconds':60},pid)
  self.assertEqual(self.store.tdf_review(self.admin),[])
  with self.assertRaises(Denied):self.store.tdf_review(self.admin,pid,1)
  with self.store.db() as d:
   piece=d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone()
   self.assertEqual((piece['version'],piece['approved_version'],piece['tdf_approval_version']),(2,None,None))
 def test_review_of_blocked_shared_piece_does_not_bypass_other_execution_guards(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class NoProvider:
   def publish(*args):raise AssertionError('No provider call in guarded fixture')
  run_one(self.store,NoProvider(),lambda a:'');self.store.tdf_review(self.admin,pid,1)
  with self.store.db(True) as d:d.execute('UPDATE pieces SET due_at=? WHERE id=?',(time.time()-3600,pid))
  with self.store.db() as d:
   with self.assertRaises(Denied):self.store.execution_guard(d,d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
  with self.store.db(True) as d:
   d.execute('UPDATE pieces SET due_at=? WHERE id=?',(time.time()-1,pid));d.execute('UPDATE accounts SET revoked=1 WHERE id=?',(self.account,))
  with self.store.db() as d:
   with self.assertRaises(Denied):self.store.execution_guard(d,d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
  with self.store.db(True) as d:
   d.execute('UPDATE accounts SET revoked=0 WHERE id=?',(self.account,));d.execute('UPDATE campaigns SET approved_version=NULL WHERE id=?',(self.cid,))
  with self.store.db() as d:
   piece=d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone()
   with self.assertRaises(Denied):self.store.execution_guard(d,piece)
   self.assertEqual(piece['tdf_approval_version'],1)
   self.assertEqual(d.execute('SELECT state FROM jobs WHERE piece=?',(pid,)).fetchone()[0],'blocked')
 def test_blocked_shared_review_cannot_reauthorize_an_existing_provider_session(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET owner=3,scope="tdf" WHERE id=?',(self.account,))
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class NoProvider:
   def publish(*args):raise AssertionError('No provider operation in partial-session fixture')
  run_one(self.store,NoProvider(),lambda a:'')
  with self.store.db(True) as d:
   d.execute('INSERT INTO provider_sessions VALUES(?,?,?)',(pid,json.dumps({'stage':'upload_initialized','fixture':True}),time.time()))
   before=dict(d.execute('SELECT * FROM jobs WHERE piece=?',(pid,)).fetchone())
  self.assertEqual(self.store.tdf_review(self.admin),[])
  with self.assertRaises(Denied) as denied:self.store.tdf_review(self.admin,pid,1)
  self.assertEqual(denied.exception.status,409)
  with self.store.db() as d:
   self.assertEqual(dict(d.execute('SELECT * FROM jobs WHERE piece=?',(pid,)).fetchone()),before)
   self.assertIsNone(d.execute('SELECT tdf_approval_version FROM pieces WHERE id=?',(pid,)).fetchone()[0])
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
 def test_unexpected_publication_error_is_visible_and_not_replayed(self):
  pid=self.piece();self.approve();self.store.status(self.a,self.cid,'active')
  class P:
   def publish(*args):raise RuntimeError('Internal fixture error')
  run_one(self.store,P(),lambda a:'');self.assertIsNone(self.store.claim())
  with self.store.db() as d:
   self.assertEqual(d.execute('SELECT status FROM pieces WHERE id=?',(pid,)).fetchone()[0],'unknown')
   self.assertTrue(d.execute('SELECT 1 FROM notices WHERE owner=1 AND severity="error"').fetchone())
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
 def test_completed_render_preserves_manual_media_revision_with_retained_binding(self):
  from media import bind_rendered_clip
  pid=self.piece(name='CL01',settings={'render_job':'render-owned','custom':'keep'})
  manual=self.store.reserve_asset(self.a,self.cid,'manual.mp4',4)
  with self.store.db(True) as d:d.execute('UPDATE assets SET status="ready" WHERE id=?',(manual,))
  self.store.piece(self.a,self.cid,{'name':'CL01','platform':'instagram','format':'reel','account':self.account,'asset':manual,'copy':'Manual revision','due_at':time.time()+60,'settings':{'render_job':'render-owned','custom':'keep'}},pid)
  before=self.store.detail(self.a,self.cid)
  bind_rendered_clip(self.store,{'id':'render-owned','campaign':self.cid},{'label':'CL01'},'old-render')
  self.assertEqual(self.store.detail(self.a,self.cid),before)
 def test_completed_render_preserves_nonmedia_revision_and_unapproved_state(self):
  from media import bind_rendered_clip
  pid=self.piece(name='CL01',settings={'render_job':'render-owned'})
  self.store.piece(self.a,self.cid,{'name':'CL01','platform':'instagram','format':'reel','copy':'Human copy','due_at':time.time()+60,'settings':{'render_job':'render-owned'}},pid)
  before=self.store.detail(self.a,self.cid)
  bind_rendered_clip(self.store,{'id':'render-owned','campaign':self.cid},{'label':'CL01'},'old-render')
  self.assertEqual(self.store.detail(self.a,self.cid),before)
 def test_completed_render_attaches_only_once_before_review(self):
  from media import bind_rendered_clip
  pid=self.piece(name='CL01',settings={'render_job':'render-owned'})
  job={'id':'render-owned','campaign':self.cid};segment={'label':'CL01'}
  with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
   list(pool.map(lambda _:bind_rendered_clip(self.store,job,segment,'finished-asset'),range(4)))
  before=self.store.detail(self.a,self.cid);piece=next(p for p in before['pieces'] if p['id']==pid)
  self.assertEqual((piece['version'],piece['asset'],piece['status'],piece['approved_version']),(2,'finished-asset','draft',None))
  bind_rendered_clip(self.store,job,segment,'duplicate-output')
  self.assertEqual(self.store.detail(self.a,self.cid),before)
  with self.store.db() as d:self.assertEqual(d.execute('SELECT count(*) FROM audit WHERE action="render.bind_draft"').fetchone()[0],1)
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
 def youtube_verification_recovery_fixture(self):
  with self.store.db(True) as d:d.execute('UPDATE accounts SET platform="youtube" WHERE id=?',(self.account,))
  pid=self.piece(platform='youtube',format='video',settings={'preserved_setting':'original'})
  self.approve()
  verification=uid();publication=uid();payload=json.dumps({'existing_verification':'private-video'})
  with self.store.db(True) as d:
   d.execute('UPDATE pieces SET external_id="private-video",status="processing" WHERE id=?',(pid,))
   d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at,state,attempts) VALUES(?,?,?,?,?,?,?,?)',(publication,self.cid,pid,'publish','{"existing_upload":"private-video"}',time.time()-1,'completed',1))
   d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at,state,attempts) VALUES(?,?,?,?,?,?,?,?)',(verification,self.cid,pid,'verify_youtube',payload,time.time()+3600,'failed',4))
   d.execute('INSERT INTO provider_sessions VALUES(?,?,?)',(pid,'{"synthetic_resumable_upload":"preserved"}',time.time()))
  self.store.status(self.a,self.cid,'active')
  return pid,verification,publication,payload,{'piece':pid,'version':1,'video_id':'private-video','checks_passed':True}
 def test_youtube_studio_confirmation_requeues_failed_or_blocked_verification_with_fresh_budget(self):
  pid,jid,publication,payload,data=self.youtube_verification_recovery_fixture()
  with self.store.db() as d:
   campaign=dict(d.execute('SELECT * FROM campaigns WHERE id=?',(self.cid,)).fetchone())
   previous_upload=dict(d.execute('SELECT * FROM jobs WHERE id=?',(publication,)).fetchone())
   session=dict(d.execute('SELECT * FROM provider_sessions WHERE piece=?',(pid,)).fetchone())
  for state in ('failed','blocked'):
   with self.subTest(state=state):
    with self.store.db(True) as d:d.execute('UPDATE jobs SET state=?,attempts=4,due_at=? WHERE id=?',(state,time.time()+3600,jid))
    self.store.youtube_checks(self.a,self.cid,data)
    with self.store.db() as d:
     job=dict(d.execute('SELECT * FROM jobs WHERE id=?',(jid,)).fetchone())
     self.assertEqual((job['state'],job['attempts'],job['kind'],job['piece'],job['campaign'],job['payload']),('queued',0,'verify_youtube',pid,self.cid,payload))
     self.assertLessEqual(job['due_at'],time.time())
     self.assertEqual(d.execute('SELECT count(*) FROM jobs').fetchone()[0],2)
     self.assertEqual(dict(d.execute('SELECT * FROM jobs WHERE id=?',(publication,)).fetchone()),previous_upload)
     self.assertEqual(dict(d.execute('SELECT * FROM campaigns WHERE id=?',(self.cid,)).fetchone()),campaign)
     self.assertEqual(dict(d.execute('SELECT * FROM provider_sessions WHERE piece=?',(pid,)).fetchone()),session)
     piece=d.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone()
     self.assertEqual((piece['external_id'],piece['status'],piece['version'],piece['approved_version']),('private-video','processing',1,1))
     self.assertEqual(json.loads(piece['settings']),{'preserved_setting':'original','youtube_checks_reviewed':True})
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:claims=list(pool.map(lambda _:self.store.claim(),range(4)))
    claimed=[job for job in claims if job]
    self.assertEqual(len(claimed),1)
    self.assertEqual((claimed[0]['id'],claimed[0]['kind'],claimed[0]['attempts']),(jid,'verify_youtube',0))
    with self.store.db() as d:self.assertEqual(d.execute('SELECT attempts FROM jobs WHERE id=?',(jid,)).fetchone()[0],1)
 def test_youtube_studio_confirmation_preserves_nonrecoverable_verification_states(self):
  pid,jid,publication,payload,data=self.youtube_verification_recovery_fixture()
  for state in ('queued','running','completed','unknown','cancelled'):
   with self.subTest(state=state):
    with self.store.db(True) as d:d.execute('UPDATE jobs SET state=?,attempts=4,claimed_at=? WHERE id=?',(state,time.time(),jid))
    self.store.youtube_checks(self.a,self.cid,data)
    with self.store.db() as d:
     job=d.execute('SELECT * FROM jobs WHERE id=?',(jid,)).fetchone()
     self.assertEqual((job['state'],job['attempts'],job['payload']),(state,4,payload))
     self.assertEqual(d.execute('SELECT count(*) FROM jobs WHERE piece=?',(pid,)).fetchone()[0],2)
     self.assertEqual(d.execute('SELECT state FROM jobs WHERE id=?',(publication,)).fetchone()[0],'completed')
 def test_youtube_verification_recovery_still_requires_current_explicit_authority(self):
  pid,jid,publication,payload,data=self.youtube_verification_recovery_fixture()
  other=self.store.create(self.b,{'name':'Otra campaña','objective':'Escuchar','destination':''})
  cases=[(self.b,self.cid,data),(self.b,other,data),(self.a,self.cid,{**data,'version':2}),(self.a,self.cid,{**data,'video_id':'different'}),(self.a,self.cid,{**data,'checks_passed':False}),(self.a,self.cid,{**data,'checks_passed':1})]
  for actor,cid,confirmation in cases:
   with self.subTest(actor=actor['id'],campaign=cid,confirmation=confirmation):
    with self.assertRaises(Denied):self.store.youtube_checks(actor,cid,confirmation)
    with self.store.db() as d:
     self.assertEqual(tuple(d.execute('SELECT state,attempts,payload FROM jobs WHERE id=?',(jid,)).fetchone()),('failed',4,payload))
     self.assertNotIn('youtube_checks_reviewed',json.loads(d.execute('SELECT settings FROM pieces WHERE id=?',(pid,)).fetchone()[0]))
  with self.store.db(True) as d:d.execute('UPDATE accounts SET revoked=1 WHERE id=?',(self.account,))
  with self.assertRaises(Denied):self.store.youtube_checks(self.a,self.cid,data)
  with self.store.db() as d:self.assertEqual(tuple(d.execute('SELECT state,attempts FROM jobs WHERE id=?',(jid,)).fetchone()),('failed',4))
 def test_external_video_cannot_be_edited_even_when_blocked_or_cancelled(self):
  with self.store.db(True) as db:db.execute('UPDATE accounts SET platform="youtube" WHERE id=?',(self.account,))
  pid=self.piece(platform='youtube',format='video');self.approve()
  data={'platform':'youtube','format':'video','account':self.account,'copy':'Replacement','due_at':time.time()}
  for status in ('blocked','cancelled','draft'):
   with self.subTest(status=status):
    with self.store.db(True) as db:db.execute('UPDATE pieces SET external_id="existing-private-video",status=? WHERE id=?',(status,pid))
    before=self.store.detail(self.a,self.cid)
    with self.assertRaises(Denied) as denied:self.store.piece(self.a,self.cid,data,pid)
    self.assertEqual(denied.exception.status,409)
    self.assertEqual(self.store.detail(self.a,self.cid),before)
  with self.store.db(True) as db:db.execute('UPDATE pieces SET status="blocked" WHERE id=?',(pid,))
  self.store.youtube_checks(self.a,self.cid,{'piece':pid,'version':1,'video_id':'existing-private-video','checks_passed':True})
  piece=self.store.detail(self.a,self.cid)['pieces'][0]
  self.assertEqual(piece['external_id'],'existing-private-video');self.assertEqual(piece['status'],'processing')
 def test_paused_youtube_upload_keeps_its_piece_version_and_resumes_original_bytes(self):
  import hashlib,io,urllib.error
  from unittest.mock import patch,Mock
  with self.store.db(True) as db:db.execute('UPDATE accounts SET platform="youtube" WHERE id=?',(self.account,))
  content=b'a'*(8*1024*1024)+b'end';aid=self.store.reserve_asset(self.a,self.cid,'master.mp4',len(content))
  target=Path(self.tmp.name)/'assets'/aid;target.parent.mkdir();target.write_bytes(content)
  with self.store.db(True) as db:db.execute('UPDATE assets SET uploaded=?,size=?,sha256=?,status="ready" WHERE id=?',(len(content),len(content),hashlib.sha256(content).hexdigest(),aid))
  pid=self.piece(platform='youtube',format='video',asset=aid);self.approve();self.store.status(self.a,self.cid,'active')
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{'STATE_DIR':self.tmp.name})
  opener=Mock()
  def acknowledge_first_chunk(request,timeout):
   self.assertEqual(request.data,content[:8*1024*1024]);self.store.status(self.a,self.cid,'paused')
   raise urllib.error.HTTPError(request.full_url,308,'Resume',{'Range':'bytes=0-8388607'},io.BytesIO())
  opener.open.side_effect=acknowledge_first_chunk
  with patch.object(adapter,'token',return_value='test-token'),patch('youtube_upload.request',return_value=({}, {'Location':'https://www.googleapis.com/upload/session-original'})) as initialize,patch('youtube_upload.urllib.request.build_opener',return_value=opener):
   self.assertTrue(run_one(self.store,adapter,lambda _: 'unused'))
   before=self.store.detail(self.a,self.cid);piece=before['pieces'][0]
   self.assertEqual(piece['status'],'blocked');self.assertIsNone(piece['external_id'])
   with self.store.db() as db:session=dict(db.execute('SELECT * FROM provider_sessions WHERE piece=?',(pid,)).fetchone())
   self.assertEqual(adapter.vault.open(session['payload'])['offset'],8*1024*1024)
   with self.assertRaises(Denied):self.store.piece(self.a,self.cid,{'platform':'youtube','format':'video','account':self.account,'copy':'Changed text','due_at':time.time()},pid)
   self.assertEqual(self.store.detail(self.a,self.cid),before)
   with self.store.db() as db:self.assertEqual(dict(db.execute('SELECT * FROM provider_sessions WHERE piece=?',(pid,)).fetchone()),session)
   # Reapproving the unchanged original resumes the same provider session.
   self.approve();self.store.status(self.a,self.cid,'active')
   response=Mock();response.__enter__=Mock(return_value=response);response.__exit__=Mock(return_value=False);response.read.return_value=b'{"id":"original-video"}'
   def finish_chunk(request,timeout):
    self.assertEqual(request.full_url,'https://www.googleapis.com/upload/session-original');self.assertEqual(request.data,b'end');return response
   opener.open.side_effect=finish_chunk
   self.assertTrue(run_one(self.store,adapter,lambda _: 'unused'));self.assertEqual(initialize.call_count,1)
  piece=self.store.detail(self.a,self.cid)['pieces'][0]
  self.assertEqual(piece['version'],1);self.assertEqual(piece['asset'],aid);self.assertEqual(piece['external_id'],'original-video')
 def test_analytics_preserves_pending_and_maps_actual_watch_time(self):
  from unittest.mock import patch
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{})
  public={'items':[{'statistics':{'viewCount':'8'},'snippet':{'publishedAt':'2026-10-28T00:00:00Z'}}]}
  report={'columnHeaders':[{'name':'estimatedMinutesWatched'},{'name':'subscribersGained'}],'rows':[[17.5,2]]}
  with patch.object(adapter,'token',return_value='test-only'),patch('providers.request',side_effect=[(public,{}),(report,{}),({'rows':[]},{}),({'rows':[]},{})]):
   result=adapter.metrics({'platform':'youtube','external_id':'video','account':self.account})
  self.assertEqual(result['watch_time'],17.5);self.assertEqual(result['attributed_subscribers'],2);self.assertEqual(result['retention']['status'],'pending');self.assertIsNone(result['thumbnail_ctr'])
 def test_youtube_analytics_dates_use_pacific_calendar_in_summer_and_winter(self):
  import datetime
  from providers import youtube_report_dates,ProviderError
  cases=[('2026-10-29T00:34:10.599Z','2026-10-30T00:34:10Z',('2026-10-28','2026-10-29')),('2026-01-29T07:30:00Z','2026-01-30T07:30:00Z',('2026-01-28','2026-01-29')),('2026-03-08T09:30:00Z','2026-03-09T06:30:00Z',('2026-03-08','2026-03-08'))]
  for published,now,expected in cases:
   with self.subTest(published=published):self.assertEqual(youtube_report_dates(published,datetime.datetime.fromisoformat(now.replace('Z','+00:00'))),expected)
  for invalid in [None,'invalid','2026-10-28']:
   with self.assertRaises(ProviderError):youtube_report_dates(invalid)

 def test_analytics_all_reports_include_publication_day_in_provider_timezone(self):
  import datetime,urllib.parse
  from unittest.mock import patch
  adapter=Providers(self.store,Vault(Fernet.generate_key()),{})
  public={'items':[{'statistics':{'viewCount':'8'},'snippet':{'publishedAt':'2026-10-29T00:34:10.599Z'}}]}
  now=datetime.datetime.fromisoformat('2026-10-30T00:34:10+00:00').timestamp()
  with patch.object(adapter,'token',return_value='test-only'),patch('providers.time.time',return_value=now),patch('providers.request',side_effect=[(public,{}),({'rows':[]},{}),({'rows':[]},{}),({'rows':[]},{})]) as network:
   result=adapter.metrics({'platform':'youtube','external_id':'video','account':self.account})
  self.assertEqual(result['analytics_date_range']['timezone'],'America/Los_Angeles')
  for call in network.call_args_list[1:]:
   query=urllib.parse.parse_qs(urllib.parse.urlsplit(call.args[0]).query)
   self.assertEqual(query['startDate'],['2026-10-28']);self.assertEqual(query['endDate'],['2026-10-29'])
  self.assertIsNone(result['watch_time']);self.assertIsNone(result['attributed_subscribers'])

if __name__=='__main__':unittest.main()
