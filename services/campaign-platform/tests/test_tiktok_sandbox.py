import json
import tempfile
import time
import unittest
import urllib.parse
from pathlib import Path
from unittest.mock import patch
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from cryptography.fernet import Fernet
from domain import Store, Denied, uid
from providers import Providers, Vault
import tiktok
import tiktok_environment as environment
import test_tiktok as fixture


class TikTokSandboxTests(unittest.TestCase):
    asset = fixture.TikTokTests.asset
    data = fixture.TikTokTests.data
    review = fixture.TikTokTests.review
    account = fixture.TikTokTests.account

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name) / 'tdf-campaign-platform-sandbox'
        (self.root / 'assets').mkdir(parents=True)
        self.store = Store(self.root / 'campaigns.sqlite3')
        self.actor = {'id': 1, 'name': 'Administrador de prueba', 'admin': True}
        self.store.user(self.actor)
        self.cid = self.store.create(self.actor, {'name': 'Prueba privada', 'objective': 'Verificar integración'})
        self.aid = uid()
        self.vault = Vault(Fernet.generate_key())
        base = 'https://api.tdfrecords.net/campaign-platform-sandbox'
        self.provider = Providers(self.store, self.vault, {
            'STATE_DIR': str(self.root), 'PUBLIC_BASE': base, 'MEDIA_KEY': 'test-key',
            'TIKTOK_MODE': 'sandbox', 'TIKTOK_CLIENT_AUDITED': False,
            'TIKTOK_CLIENT_KEY': 'sandbox-client', 'TIKTOK_CLIENT_SECRET': 'test-secret',
            'TIKTOK_SANDBOX_ADMIN_ID': 1, 'TIKTOK_SANDBOX_USERNAME': 'artista',
            'TIKTOK_VERIFIED_MEDIA_PREFIX': base + '/media/'})
        token = {'access_token': 'test-token', 'refresh_token': 'test-refresh',
                 '_tdf_tiktok_client_binding': environment.binding(self.provider.config)}
        with self.store.db(True) as db:
            db.execute('INSERT INTO accounts VALUES(?,?,?,?,?,?,?,?,?,?)',
                       (self.aid, 1, 'tiktok', 'creator1', 'Artista', 'tdf', self.vault.seal(token),
                        '"user.info.basic,video.publish"', time.time() + 3600, 0))
        self.info = {'creator_username': 'artista', 'creator_nickname': 'Artista',
                     'privacy_level_options': ['PUBLIC_TO_EVERYONE', 'SELF_ONLY'],
                     'comment_disabled': False, 'duet_disabled': False, 'stitch_disabled': False,
                     'max_video_post_duration_sec': 180}

    def tearDown(self):
        self.tmp.cleanup()

    def private_data(self):
        data = self.data()
        data['settings']['tiktok']['privacy'] = 'SELF_ONLY'
        return data

    def prepared(self):
        data = self.review(self.private_data())
        pid = self.store.piece(self.actor, self.cid, data)
        self.store.approve(self.actor, self.cid, {
            'version': self.store.detail(self.actor, self.cid)['version'],
            'rights_confirmed': True, 'tdf_approve': True})
        self.store.status(self.actor, self.cid, 'active')
        with self.store.db() as db:
            return dict(db.execute('SELECT * FROM pieces WHERE id=?', (pid,)).fetchone())

    def test_sandbox_cannot_share_production_paths_or_claim_audit(self):
        for key, value in [('PUBLIC_BASE', 'https://api.tdfrecords.net/campaign-platform'),
                           ('STATE_DIR', '/var/lib/tdf-campaign-platform'),
                           ('TIKTOK_CLIENT_AUDITED', True), ('TIKTOK_MODE', 'other'),
                           ('TIKTOK_SANDBOX_ADMIN_ID', True), ('TIKTOK_SANDBOX_USERNAME', '')]:
            old = self.provider.config[key]
            self.provider.config[key] = value
            with patch('tiktok.request') as network:
                with self.assertRaises(Denied):
                    tiktok.creator(self.provider, self.account())
                network.assert_not_called()
            self.provider.config[key] = old

    def test_sandbox_only_accepts_designated_admin_and_tdf_tiktok_scope(self):
        for actor, platform, scope in [({**self.actor, 'admin': False}, 'tiktok', 'tdf'),
                                       ({**self.actor, 'id': 2}, 'tiktok', 'tdf'),
                                       (self.actor, 'youtube', 'tdf'),
                                       (self.actor, 'tiktok', 'own')]:
            with self.assertRaises(Denied):
                self.provider.start(actor, platform, scope)
        params = urllib.parse.parse_qs(urllib.parse.urlsplit(self.provider.start(self.actor, 'tiktok', 'tdf')).query)
        self.assertEqual(params['redirect_uri'], [self.provider.config['PUBLIC_BASE'] + '/oauth/callback/tiktok'])

    def test_creator_uses_real_private_option_and_exact_authorized_username(self):
        with patch('tiktok.request', return_value=({'data': self.info, 'error': {'code': 'ok'}}, {})):
            info = tiktok.creator(self.provider, self.account())
        self.assertEqual(info['privacy_level_options'], ['SELF_ONLY'])
        self.assertEqual(info['environment'], 'sandbox')
        for changes in [{'creator_username': 'otro'}, {'privacy_level_options': ['PUBLIC_TO_EVERYONE']}]:
            with patch('tiktok.request', return_value=({'data': {**self.info, **changes}, 'error': {'code': 'ok'}}, {})):
                with self.assertRaises(Denied):
                    tiktok.creator(self.provider, self.account())

    def test_callback_checks_real_provider_username_before_connecting_shared_account(self):
        for username in ['otro', 'artista']:
            params = urllib.parse.parse_qs(urllib.parse.urlsplit(self.provider.start(self.actor, 'tiktok', 'tdf')).query)
            replies = [({'access_token': 'test-token', 'refresh_token': 'test-refresh', 'expires_in': 3600, 'scope': 'user.info.basic,video.publish'}, {}),
                       ({'data': {'user': {'open_id': 'creator1', 'display_name': 'Artista'}}}, {}),
                       ({'error': {'code': 'ok'}, 'data': {'creator_username': username}}, {})]
            with patch('providers.request', side_effect=replies) as network:
                if username == 'otro':
                    with self.assertRaises(Denied):
                        self.provider.callback('tiktok', params['state'][0], 'code', self.actor)
                else:
                    self.provider.callback('tiktok', params['state'][0], 'code', self.actor)
            self.assertEqual(network.call_count, 3)
        self.assertEqual(self.account()['scope'], 'tdf')
        token = self.vault.open(self.account()['credentials'])
        self.assertEqual(token['_tdf_tiktok_client_binding'], environment.binding(self.provider.config))

    def test_nonprivate_consent_and_initialization_are_blocked_without_network(self):
        with self.assertRaises(Denied):
            self.review(self.data())
        piece = self.prepared()
        settings = json.loads(piece['settings'])
        settings['tiktok']['privacy'] = 'PUBLIC_TO_EVERYONE'
        with patch('tiktok.request') as network:
            with self.assertRaises(Denied):
                tiktok.publish(self.provider, {**piece, 'settings': json.dumps(settings)}, self.account())
            network.assert_not_called()

    def test_private_send_keeps_tdf_approval_consent_and_restricted_completion(self):
        piece = self.prepared()
        with patch('tiktok.creator', return_value=self.info), patch('tiktok.check_media'), \
                patch('tiktok.call', return_value={'publish_id': 'sandbox-publish'}) as network:
            result = self.provider.publish(piece, self.account())
        self.assertEqual(result['state'], 'processing')
        self.assertEqual(network.call_args.args[2]['post_info']['privacy_level'], 'SELF_ONLY')
        piece['external_id'] = result['external_id']
        with patch('tiktok.call', return_value={'status': 'PUBLISH_COMPLETE'}):
            self.assertEqual(tiktok.verify(self.provider, piece, self.account())['state'], 'published_restricted')

    def test_changing_client_invalidates_review_consent_and_cached_access_token(self):
        piece = self.prepared()
        self.provider.config['TIKTOK_CLIENT_KEY'] = 'different-client'
        with patch('tiktok.request') as network, patch('tiktok_token.request') as refresh:
            with self.assertRaises(Denied):
                tiktok.publish(self.provider, piece, self.account())
            with self.assertRaises(Denied):
                self.provider.token(self.account())
            network.assert_not_called()
            refresh.assert_not_called()

    def test_sandbox_cannot_dispatch_to_other_platforms(self):
        with patch('providers.request') as network:
            with self.assertRaises(Denied):
                self.provider.publish({'platform': 'youtube'}, {})
            network.assert_not_called()

    def test_personal_test_account_keeps_own_scope_after_exact_username_check(self):
        self.provider.config['TIKTOK_SANDBOX_SCOPE'] = 'own'
        with self.store.db(True) as db:
            db.execute('UPDATE accounts SET scope="own" WHERE id=?', (self.aid,))
        with self.assertRaises(Denied):
            self.provider.start(self.actor, 'tiktok', 'tdf')
        url = self.provider.start(self.actor, 'tiktok', 'own')
        state = urllib.parse.parse_qs(urllib.parse.urlsplit(url).query)['state'][0]
        replies = [({'access_token': 'test-token', 'refresh_token': 'test-refresh', 'expires_in': 3600, 'scope': 'user.info.basic,video.publish'}, {}),
                   ({'data': {'user': {'open_id': 'creator1', 'display_name': 'Artista'}}}, {}),
                   ({'error': {'code': 'ok'}, 'data': {'creator_username': 'artista'}}, {})]
        with patch('providers.request', side_effect=replies):
            self.provider.callback('tiktok', state, 'code', self.actor)
        self.assertEqual(self.account()['scope'], 'own')
        self.assertFalse(self.provider.known_tdf_identity('tiktok', 'creator1'))
        piece = self.prepared()
        with patch('tiktok.creator', return_value=self.info), patch('tiktok.check_media'), \
                patch('tiktok.call', return_value={'publish_id': 'personal-private-test'}):
            self.assertEqual(self.provider.publish(piece, self.account())['state'], 'processing')

    def test_changing_target_or_scope_invalidates_context_and_tdf_cannot_be_personal(self):
        data = self.private_data()
        with patch('tiktok.creator', return_value=self.info):
            context = tiktok.review_context(self.provider, self.actor, self.cid, self.aid)
        for key, value in [('TIKTOK_SANDBOX_USERNAME', 'otra'), ('TIKTOK_SANDBOX_SCOPE', 'own')]:
            previous = self.provider.config.get(key)
            self.provider.config[key] = value
            with self.assertRaises(Denied):
                tiktok.consent(self.provider, self.actor, self.cid, {
                    **data, 'context': context['context'], 'music_agreed': True,
                    'preview_confirmed': True, 'original_content_confirmed': True})
            if previous is None:self.provider.config.pop(key)
            else:self.provider.config[key] = previous
        self.provider.config.update(TIKTOK_SANDBOX_USERNAME='tdf.records.label', TIKTOK_SANDBOX_SCOPE='own')
        with self.assertRaises(Denied):
            environment.mode(self.provider.config)
