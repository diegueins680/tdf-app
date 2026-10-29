import json
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from cryptography.fernet import Fernet
from domain import Store, Denied, uid
from providers import Providers, Vault, ProviderError
import facebook
from worker import run_one


class FacebookTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.store = Store(Path(self.tmp.name) / 'state.sqlite3')
        self.actor = {'id': 1, 'name': 'Artista', 'admin': False}
        self.store.user(self.actor)
        self.cid = self.store.create(self.actor, {'name': 'Sesión', 'objective': 'Escuchar'})
        self.account = uid()
        self.adapter = Providers(self.store, Vault(Fernet.generate_key()),
                                 {'PUBLIC_BASE': 'https://api.tdfrecords.net/campaign-platform',
                                  'MEDIA_KEY': 'test', 'META_API_VERSION': 'v23.0'})
        with self.store.db(True) as db:
            db.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',
                       (self.account, 1, 'facebook', 'page1', 'Página', 'own',
                        self.adapter.vault.seal({'access_token': 'test-token'}),
                        '["pages_manage_posts","pages_read_engagement"]', None, 0))

    def tearDown(self):
        self.tmp.cleanup()

    def asset(self, name='clip.mp4', campaign=None):
        identifier = self.store.reserve_asset(self.actor, campaign or self.cid, name, 10)
        with self.store.db(True) as db:
            db.execute('UPDATE assets SET status="ready" WHERE id=?', (identifier,))
        return identifier

    def prepare(self, fmt='video', **extra):
        pid = self.store.piece(self.actor, self.cid,
                               {'platform': 'facebook', 'format': fmt, 'name': 'Fragmento',
                                'account': self.account, 'asset': self.asset(),
                                'due_at': time.time(), 'copy': 'Sesión completa', **extra})
        self.store.approve(self.actor, self.cid,
                           {'version': self.store.detail(self.actor, self.cid)['version'],
                            'rights_confirmed': True})
        self.store.status(self.actor, self.cid, 'active')
        with self.store.db() as db:
            return dict(db.execute('SELECT * FROM pieces WHERE id=?', (pid,)).fetchone()), dict(
                db.execute('SELECT * FROM accounts WHERE id=?', (self.account,)).fetchone())

    def uploaded(self):
        piece, account = self.prepare()
        with patch('facebook.request', return_value=({'id': '1234'}, {})) as network:
            result = self.adapter.publish(piece, account, 'https://media.example/approved')
        self.assertFalse(network.call_args.args[2]['published'])
        self.assertEqual(result['state'], 'processing')
        piece['external_id'] = '1234'
        return piece, account

    def test_private_video_upload_does_not_publish_or_reupload(self):
        piece, account = self.uploaded()
        with self.store.db() as db:
            self.assertEqual(db.execute('SELECT kind FROM jobs WHERE piece=? AND kind!=? ',
                                        (piece['id'], 'publish')).fetchone()[0], 'verify_facebook')
        with patch('facebook.request') as network:
            with self.assertRaises(ProviderError):
                self.adapter.publish(piece, account, 'https://media.example/approved')
            network.assert_not_called()

    def test_ready_is_not_published_and_finish_is_never_replayed(self):
        piece, account = self.uploaded()
        ready = {'id': '1234', 'published': False, 'status': {'video_status': 'ready'}}
        with patch('facebook.request', side_effect=[(ready, {}), ({'success': True}, {})]) as network:
            result = facebook.verify(self.adapter, piece, account)
            self.assertEqual(result['state'], 'waiting_for_facebook')
            self.assertEqual(network.call_args.args[2], {'published': True})
        with patch('facebook.request', return_value=(ready, {})) as network:
            facebook.verify(self.adapter, piece, account)
            self.assertEqual(network.call_count, 1)
        public = {**ready, 'published': True, 'permalink_url': 'https://www.facebook.com/watch/?v=1234'}
        with patch('facebook.request', return_value=(public, {})):
            self.assertEqual(facebook.verify(self.adapter, piece, account)['state'], 'published')

    def test_pause_after_upload_prevents_final_publication(self):
        piece, account = self.uploaded()
        self.store.status(self.actor, self.cid, 'paused')
        with patch('facebook.request', return_value=({'published': False, 'status': {'video_status': 'ready'}}, {})) as network:
            with self.assertRaises(Denied):facebook.verify(self.adapter, piece, account)
            self.assertEqual(network.call_count, 1)

    def test_uncertain_finish_and_crashed_verification_are_quarantined(self):
        piece, account = self.uploaded()
        with self.store.db(True) as db:
            db.execute('UPDATE jobs SET state="completed" WHERE kind="publish"')
            db.execute('UPDATE jobs SET due_at=? WHERE kind="verify_facebook"', (time.time()-1,))
        ready = {'published': False, 'status': {'video_status': 'ready'}}
        with patch('facebook.request', side_effect=[(ready, {}), ProviderError('timeout', True)]):
            run_one(self.store, self.adapter, lambda _: '')
        self.assertIsNone(self.store.claim())
        with self.store.db() as db:
            self.assertEqual(db.execute('SELECT status FROM pieces WHERE id=?', (piece['id'],)).fetchone()[0], 'unknown')
        with self.store.db(True) as db:
            db.execute('UPDATE jobs SET state="running",claimed_at=? WHERE kind="verify_facebook"', (time.time()-700,))
        self.assertIsNone(self.store.claim())
        with self.store.db() as db:
            self.assertEqual(db.execute('SELECT state FROM jobs WHERE kind="verify_facebook"').fetchone()[0], 'unknown')

    def test_missing_real_page_grant_blocks_all_provider_calls(self):
        piece, account = self.prepare()
        account['permissions'] = '"requested_in_oauth"'
        with patch('facebook.request') as network:
            with self.assertRaises(Denied):self.adapter.publish(piece, account, 'https://media.example/approved')
            network.assert_not_called()

    def test_carousel_rejects_foreign_child_before_uploading_any_image(self):
        other = self.store.create(self.actor, {'name': 'Otra', 'objective': 'Otra sesión'})
        good, foreign = self.asset('slide.png'), self.asset('slide.png', other)
        piece, account = self.prepare('carousel', asset=good, settings={'children': [good, foreign]})
        with patch('facebook.request') as network:
            with self.assertRaises(Denied):self.adapter.publish(piece, account)
            network.assert_not_called()

    def test_carousel_uploads_private_images_then_one_reviewed_feed_post(self):
        images = [self.asset('one.png'), self.asset('two.jpg')]
        piece, account = self.prepare('carousel', asset=images[0], settings={'children': images})
        responses = [({'id': 'photo1'}, {}), ({'id': 'photo2'}, {}), ({'id': 'page1_post1'}, {}),
                     ({'id': 'page1_post1', 'permalink_url': 'https://www.facebook.com/page1/posts/post1'}, {})]
        with patch('facebook.request', side_effect=responses) as network:
            result = self.adapter.publish(piece, account)
        self.assertEqual(result['state'], 'published')
        self.assertEqual([call.args[2]['published'] for call in network.call_args_list[:2]], [False, False])
        self.assertEqual(network.call_args_list[2].args[2]['attached_media'],
                         [{'media_fbid': 'photo1'}, {'media_fbid': 'photo2'}])

    def test_reel_refuses_foreign_upload_host_without_transmitting_credentials(self):
        piece, account = self.prepare('reel')
        with patch('facebook.request', return_value=({'video_id': '1234', 'upload_url': 'https://evil.example/video-upload/v23.0/1234'}, {})) as network:
            with self.assertRaises(ProviderError):self.adapter.publish(piece, account, 'https://media.example/approved')
            self.assertEqual(network.call_count, 1)

    def test_reel_stages_native_upload_and_preserves_approved_caption(self):
        piece, account = self.prepare('reel')
        responses = [({'video_id': '1234', 'upload_url': 'https://rupload.facebook.com/video-upload/v23.0/1234'}, {}), ({'success': True}, {})]
        with patch('facebook.request', side_effect=responses) as network:
            result = self.adapter.publish(piece, account, 'https://media.example/approved')
        self.assertEqual(result['state'], 'processing')
        self.assertEqual(network.call_args.kwargs['headers']['file_url'], 'https://media.example/approved')
        piece['external_id'] = '1234'
        with patch('facebook.request', side_effect=[({'published': False, 'status': {'video_status': 'ready'}}, {}), ({'success': True}, {})]) as network:
            facebook.verify(self.adapter, piece, account)
            self.assertEqual(network.call_args.args[2]['description'], piece['copy'])
            self.assertEqual(network.call_args.args[2]['video_state'], 'PUBLISHED')

    def test_oauth_shared_scope_imports_only_registered_tdf_pages(self):
        import urllib.parse
        admin = {'id': 1, 'name': 'Admin', 'admin': True}
        self.adapter.config.update(FACEBOOK_APP_ID='app', FACEBOOK_APP_SECRET='secret',
                                   TDF_ACCOUNT_IDS={'facebook': ['official']})
        for scope, expected in [('tdf', ['official']), ('own', ['unrelated'])]:
            with self.store.db(True) as db:db.execute('DELETE FROM accounts')
            url = self.adapter.start(admin, 'facebook', scope)
            state = urllib.parse.parse_qs(urllib.parse.urlsplit(url).query)['state'][0]
            responses = [({'access_token':'user-token'}, {}),
                         ({'data':[{'permission':'pages_manage_posts','status':'granted'}]}, {}),
                         ({'data':[{'id':'unrelated','name':'Other project','access_token':'other-token'},
                                   {'id':'official','name':'TDF','access_token':'tdf-token'}]}, {})]
            with patch('providers.request', side_effect=responses):
                self.adapter.callback('facebook', state, 'code', admin)
            with self.store.db() as db:
                rows = db.execute('SELECT external_id,scope,permissions FROM accounts').fetchall()
            self.assertEqual([r['external_id'] for r in rows], expected)
            self.assertTrue(all(r['scope']==scope for r in rows))
            self.assertTrue(all(json.loads(r['permissions'])==['pages_manage_posts'] for r in rows))

    def test_registered_tdf_identity_cannot_publish_as_personal_account(self):
        piece, account = self.prepare()
        self.adapter.config['TDF_ACCOUNT_IDS'] = {'facebook': ['page1']}
        with patch('facebook.request') as network:
            with self.assertRaises(Denied):self.adapter.publish(piece, account, 'https://media.example/approved')
            network.assert_not_called()


if __name__ == '__main__':unittest.main()
