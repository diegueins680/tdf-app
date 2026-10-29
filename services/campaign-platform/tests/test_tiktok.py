import hashlib
import json
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from cryptography.fernet import Fernet
from domain import Store,Denied,uid
from providers import Providers,Vault,ProviderError
import tiktok
from worker import run_one


class TikTokTests(unittest.TestCase):
    def setUp(self):
        self.tmp=tempfile.TemporaryDirectory();self.root=Path(self.tmp.name)
        (self.root/'assets').mkdir();self.store=Store(self.root/'campaigns.sqlite3')
        self.actor={'id':1,'name':'Artista','admin':False};self.store.user(self.actor)
        self.cid=self.store.create(self.actor,{'name':'Sesión','objective':'Escuchar'})
        self.aid=uid();self.vault=Vault(Fernet.generate_key())
        self.provider=Providers(self.store,self.vault,{'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform','STATE_DIR':str(self.root),'MEDIA_KEY':'test-key','TIKTOK_CLIENT_KEY':'client','TIKTOK_CLIENT_SECRET':'test-secret','TIKTOK_CLIENT_AUDITED':True,'TIKTOK_VERIFIED_MEDIA_PREFIX':'https://api.tdfrecords.net/campaign-platform/media/'})
        with self.store.db(True) as db:db.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',(self.aid,1,'tiktok','creator1','Artista','own',self.vault.seal({'access_token':'test-token','refresh_token':'refresh1'}),'"user.info.basic,video.publish,video.list"',time.time()+3600,0))
        self.info={'creator_username':'artista','creator_nickname':'Artista','privacy_level_options':['PUBLIC_TO_EVERYONE','MUTUAL_FOLLOW_FRIENDS','SELF_ONLY'],'comment_disabled':False,'duet_disabled':False,'stitch_disabled':False,'max_video_post_duration_sec':180}

    def tearDown(self):self.tmp.cleanup()

    def account(self):
        with self.store.db() as db:return dict(db.execute('SELECT * FROM accounts WHERE id=?',(self.aid,)).fetchone())

    def asset(self,name='clip.mp4',campaign=None):
        aid=self.store.reserve_asset(self.actor,campaign or self.cid,name,4)
        (self.root/'assets'/aid).write_bytes(b'data')
        with self.store.db(True) as db:db.execute('UPDATE assets SET status="ready",size=4,uploaded=4,sha256=? WHERE id=?',(hashlib.sha256(b'data').hexdigest(),aid))
        return aid

    def data(self,fmt='video',**changes):
        options={'privacy':'PUBLIC_TO_EVERYONE','allow_comment':False,'allow_duet':False,'allow_stitch':False,'commercial':False,'brand_own':False,'brand_paid':False,'is_aigc':False,'title':'','images':[],'cover_index':0}
        value={'name':'Fragmento','platform':'tiktok','format':fmt,'account':self.aid,'asset':self.asset(),'copy':'Sesión completa en YouTube: TDF Records','destination':'https://youtu.be/example','due_at':time.time()+30,'settings':{'tiktok':options}}
        value.update(changes);return value

    def review(self,data,info=None,actor=None):
        info=info or self.info;actor=actor or self.actor
        with patch('tiktok.creator',return_value=info):context=tiktok.review_context(self.provider,actor,self.cid,self.aid)
        result=tiktok.consent(self.provider,actor,self.cid,{**data,'context':context['context'],'music_agreed':True,'preview_confirmed':True,'original_content_confirmed':True,'brand_policy_agreed':True})
        data['settings']['tiktok']['review_token']=result['review_token'];return data

    def prepare(self,data=None):
        data=self.review(data or self.data());pid=self.store.piece(self.actor,self.cid,data)
        self.store.approve(self.actor,self.cid,{'version':self.store.detail(self.actor,self.cid)['version'],'rights_confirmed':True})
        self.store.status(self.actor,self.cid,'active')
        with self.store.db(True) as db:
            db.execute('UPDATE pieces SET due_at=? WHERE id=?',(data['due_at'],pid))
        with self.store.db() as db:return dict(db.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())

    def upload(self,data=None):
        piece=self.prepare(data)
        with patch('tiktok.creator',return_value=self.info),patch('tiktok.check_media'),patch('tiktok.call',return_value={'publish_id':'v_pub~test'}) as api:
            result=self.provider.publish(piece,self.account(),'unused')
        piece['external_id']=result['external_id'];return piece,api.call_args.args

    def test_default_missing_audit_and_domain_block_before_any_network(self):
        for key,value in [('TIKTOK_CLIENT_KEY',None),('TIKTOK_CLIENT_AUDITED',False),('TIKTOK_VERIFIED_MEDIA_PREFIX','https://other.example/')]:
            old=self.provider.config[key];self.provider.config[key]=value
            with patch('tiktok.request') as network:
                with self.assertRaises(Denied):tiktok.review_context(self.provider,self.actor,self.cid,self.aid)
                network.assert_not_called()
            self.provider.config[key]=old

    def test_creator_real_error_and_actual_permissions_are_required(self):
        with patch('tiktok.request',return_value=({'data':{},'error':{'code':'spam_risk_too_many_posts'}},{})):
            with self.assertRaises(ProviderError):tiktok.creator(self.provider,self.account())
        account=self.account();account['permissions']='"requested_in_oauth"'
        with patch('tiktok.request') as network:
            with self.assertRaises(Denied):tiktok.creator(self.provider,account)
            network.assert_not_called()

    def test_foreign_account_and_context_cannot_authorize_content(self):
        other={'id':2,'name':'Otra persona','admin':False};self.store.user(other)
        with self.assertRaises(Denied):tiktok.review_context(self.provider,other,self.cid,self.aid)
        data=self.data()
        with patch('tiktok.creator',return_value=self.info):context=tiktok.review_context(self.provider,self.actor,self.cid,self.aid)
        self.store.member(self.actor,self.cid,{'user_id':2,'role':'editor'})
        with self.assertRaises(Denied):tiktok.consent(self.provider,other,self.cid,{**data,'context':context['context'],'music_agreed':True,'preview_confirmed':True,'original_content_confirmed':True})

    def test_consent_requires_explicit_preview_music_and_supported_manual_choices(self):
        data=self.data()
        with patch('tiktok.creator',return_value=self.info):context=tiktok.review_context(self.provider,self.actor,self.cid,self.aid)
        full={**data,'context':context['context'],'music_agreed':True,'preview_confirmed':True,'original_content_confirmed':True}
        for key in ['music_agreed','preview_confirmed','original_content_confirmed']:
            with self.assertRaises(Denied):tiktok.consent(self.provider,self.actor,self.cid,{**full,key:False})
        for changes in [{'privacy':''},{'privacy':'FOLLOWER_OF_CREATOR'},{'commercial':True},{'commercial':True,'brand_paid':True,'privacy':'SELF_ONLY'}]:
            altered={**data,'settings':{'tiktok':{**data['settings']['tiktok'],**changes}}}
            with self.assertRaises(Denied):tiktok.consent(self.provider,self.actor,self.cid,{**full,**altered})

    def test_one_consent_cannot_be_replayed_on_another_piece(self):
        data=self.review(self.data());pid=self.store.piece(self.actor,self.cid,data)
        with self.assertRaises(Denied):self.store.piece(self.actor,self.cid,data)
        with self.store.db() as db:self.assertEqual(db.execute('SELECT count(*) FROM pieces').fetchone()[0],1)
        with self.assertRaises(Denied):self.store.piece(self.actor,self.cid,data,pid)

    def test_drafts_can_be_saved_before_oauth_without_granting_publication(self):
        data=self.data();pid=self.store.piece(self.actor,self.cid,data)
        self.store.approve(self.actor,self.cid,{'version':self.store.detail(self.actor,self.cid)['version'],'rights_confirmed':True});self.store.status(self.actor,self.cid,'active')
        with self.store.db() as db:piece=dict(db.execute('SELECT * FROM pieces WHERE id=?',(pid,)).fetchone())
        with patch('tiktok.request') as network:
            with self.assertRaises(Denied):self.provider.publish(piece,self.account())
            network.assert_not_called()

    def test_context_is_single_use_and_nonfinite_date_is_refused(self):
        data=self.data()
        with patch('tiktok.creator',return_value=self.info):context=tiktok.review_context(self.provider,self.actor,self.cid,self.aid)
        reviewed={**data,'context':context['context'],'music_agreed':True,'preview_confirmed':True,'original_content_confirmed':True}
        with self.assertRaises(Denied):tiktok.consent(self.provider,self.actor,self.cid,{**reviewed,'due_at':float('nan')})
        tiktok.consent(self.provider,self.actor,self.cid,reviewed)
        with self.assertRaises(Denied):tiktok.consent(self.provider,self.actor,self.cid,reviewed)

    def test_tdf_approval_is_required_before_any_network_call(self):
        with self.store.db(True) as db:db.execute('UPDATE accounts SET scope="tdf" WHERE id=?',(self.aid,))
        piece=self.prepare()
        with patch('tiktok.request') as network:
            with self.assertRaises(Denied):self.provider.publish(piece,self.account())
            network.assert_not_called()

    def test_status_retry_is_read_only_and_does_not_restart_publication(self):
        piece,_=self.upload()
        with self.store.db(True) as db:
            db.execute('UPDATE jobs SET state="completed" WHERE kind="publish"')
            db.execute('UPDATE jobs SET due_at=? WHERE kind="verify_tiktok"',(time.time()-1,))
        with patch('tiktok.call',side_effect=ProviderError('temporary_status_failure',True)) as network:
            run_one(self.store,self.provider,lambda _: '')
            self.assertEqual(network.call_count,1)
            self.assertEqual(network.call_args.args[0],'/post/publish/status/fetch/')
        with self.store.db() as db:
            self.assertEqual(db.execute('SELECT state FROM jobs WHERE kind="verify_tiktok"').fetchone()[0],'queued')
            self.assertEqual(db.execute('SELECT state FROM jobs WHERE kind="publish"').fetchone()[0],'completed')

    def test_changed_text_asset_or_privacy_invalidates_consent(self):
        piece=self.prepare()
        for change in [{'copy':'Otro texto'},{'asset':self.asset()}]:
            with patch('tiktok.creator',return_value=self.info),patch('tiktok.check_media'),patch('tiktok.call',return_value={'publish_id':'test-publish'}) as network:
                with self.assertRaises(Denied):tiktok.publish(self.provider,{**piece,**change},self.account())
                network.assert_not_called()
        values=json.loads(piece['settings']);values['tiktok']['privacy']='SELF_ONLY'
        with self.assertRaises(Denied):tiktok.publish(self.provider,{**piece,'settings':json.dumps(values)},self.account())

    def test_current_creator_changes_and_pause_prevent_initialization(self):
        piece=self.prepare();restricted={**self.info,'privacy_level_options':['SELF_ONLY']}
        with patch('tiktok.creator',return_value=restricted),patch('tiktok.call') as network:
            with self.assertRaises(Denied):tiktok.publish(self.provider,piece,self.account())
            network.assert_not_called()
        self.store.status(self.actor,self.cid,'paused')
        with patch('tiktok.creator',return_value=self.info),patch('tiktok.check_media'),patch('tiktok.call') as network:
            with self.assertRaises(Denied):tiktok.publish(self.provider,piece,self.account())
            network.assert_not_called()

    def test_video_uses_only_approved_caption_and_server_pull_url(self):
        piece,args=self.upload();body=args[2]
        self.assertEqual(args[0],'/post/publish/video/init/')
        self.assertEqual(body['post_info']['title'],piece['copy'])
        self.assertTrue(body['post_info']['disable_comment'])
        self.assertEqual(body['source_info']['source'],'PULL_FROM_URL')
        self.assertTrue(body['source_info']['video_url'].startswith(self.provider.config['TIKTOK_VERIFIED_MEDIA_PREFIX']))
        with self.store.db() as db:self.assertEqual(db.execute('SELECT state FROM jobs WHERE kind="verify_tiktok"').fetchone()[0],'queued')
        with patch('tiktok.call') as network:
            with self.assertRaises(ProviderError):tiktok.publish(self.provider,piece,self.account())
            network.assert_not_called()

    def test_uncertain_initialization_is_never_resubmitted(self):
        piece=self.prepare()
        with patch('tiktok.creator',return_value=self.info),patch('tiktok.check_media'),patch('tiktok.call',side_effect=ProviderError('timeout',True)) as network:
            with self.assertRaises(ProviderError):tiktok.publish(self.provider,piece,self.account())
            self.assertEqual(network.call_count,1)
        with patch('tiktok.call') as network:
            with self.assertRaises(ProviderError):tiktok.publish(self.provider,piece,self.account())
            network.assert_not_called()

    def test_photo_validates_all_assets_and_never_adds_music_automatically(self):
        data=self.data('photo',asset=None);data['settings']['tiktok']['images']=[self.asset('one.jpg'),self.asset('two.webp')]
        piece,args=self.upload(data);body=args[2]
        self.assertEqual(args[0],'/post/publish/content/init/')
        self.assertEqual(body['post_mode'],'DIRECT_POST');self.assertFalse(body['post_info']['auto_add_music'])
        self.assertEqual(len(body['source_info']['photo_images']),2)
        foreign=self.store.create(self.actor,{'name':'Otra','objective':'Otra sesión'})
        bad=self.data('photo',asset=None);bad['settings']['tiktok']['images']=[self.asset('one.jpg'),self.asset('foreign.jpg',foreign)]
        with self.assertRaises(Denied):self.review(bad)

    def test_processing_or_moderation_without_public_id_is_not_publication(self):
        piece,_=self.upload()
        for response in [{'status':'PROCESSING_DOWNLOAD'},{'status':'PUBLISH_COMPLETE','publicaly_available_post_id':[]}]:
            with patch('tiktok.call',return_value=response):self.assertEqual(tiktok.verify(self.provider,piece,self.account())['state'],'waiting_for_tiktok')
        with patch('tiktok.call',return_value={'status':'PUBLISH_COMPLETE','publicaly_available_post_id':[123]}):
            result=tiktok.verify(self.provider,piece,self.account());self.assertEqual(result['state'],'published');self.assertIsNone(result['url'])
        with patch('tiktok.call',return_value={'status':'FAILED','fail_reason':'copyright_check_failed'}):
            with self.assertRaises(Denied):tiktok.verify(self.provider,piece,self.account())

    def test_private_completion_is_labeled_restricted_without_invented_url(self):
        data=self.data();data['settings']['tiktok']['privacy']='SELF_ONLY';piece,_=self.upload(data)
        with patch('tiktok.call',return_value={'status':'PUBLISH_COMPLETE'}):
            result=tiktok.verify(self.provider,piece,self.account());self.assertEqual(result['state'],'published_restricted');self.assertIsNone(result['url'])
        with self.store.db(True) as db:db.execute('UPDATE jobs SET due_at=0 WHERE piece=? AND kind="verify_tiktok"',(piece['id'],))
        with patch('tiktok.call',return_value={'status':'PUBLISH_COMPLETE'}):self.assertTrue(run_one(self.store,self.provider,lambda _: 'unused'))
        with self.store.db() as db:self.assertEqual(db.execute('SELECT status FROM pieces WHERE id=?',(piece['id'],)).fetchone()[0],'published_restricted')
        with self.assertRaises(Denied):self.store.piece(self.actor,self.cid,data,piece['id'])

    def test_media_checks_reject_mutated_bytes_and_current_duration_limit(self):
        data=self.data();piece={**data,'campaign':self.cid};assets=tiktok.assets_for(self.provider,piece,data['settings']['tiktok'])
        meta={'streams':[{'codec_type':'video','codec_name':'h264','width':1080,'height':1920,'avg_frame_rate':'30/1'}],'format':{'duration':'200'}}
        with patch('tiktok.subprocess.run',return_value=type('Result',(),{'stdout':json.dumps(meta).encode()})()):
            with self.assertRaises(Denied):tiktok.check_media(self.provider,piece,assets,self.info)
        (self.root/'assets'/data['asset']).write_bytes(b'evil')
        with patch('tiktok.subprocess.run') as probe:
            with self.assertRaises(Denied):tiktok.check_media(self.provider,piece,assets,self.info)
            probe.assert_not_called()

    def test_refresh_rotation_uses_latest_account_and_revocation_is_immediate(self):
        with self.store.db(True) as db:db.execute('UPDATE accounts SET expires_at=? WHERE id=?',(time.time()-10,self.aid))
        fresh={'access_token':'new-token','refresh_token':'refresh2','open_id':'creator1','expires_in':86400,'scope':'video.publish'}
        with patch('tiktok_token.request',return_value=(fresh,{})) as network:
            self.assertEqual(self.provider.token(self.account()),'new-token');self.assertEqual(self.provider.token(self.account()),'new-token');self.assertEqual(network.call_count,1)
        stale=self.account()
        with self.store.db(True) as db:db.execute('UPDATE accounts SET revoked=1 WHERE id=?',(self.aid,))
        with patch('tiktok_token.request') as network:
            with self.assertRaises(Denied):self.provider.token(stale)
            network.assert_not_called()


if __name__=='__main__':unittest.main()
