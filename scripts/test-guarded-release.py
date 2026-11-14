#!/usr/bin/env python3
"""Orchestration controls for the guarded Hetzner release.

The journal, release files and bundle archive are real; production effects run
against an in-memory host model. Real Docker/PostgreSQL/SFTP behaviour is
qualified separately by the staging rehearsal, not by these tests.
"""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import types
import unittest

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('guarded_release', ROOT / 'ops/hetzner/guarded-release.py')
gr = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gr)

OLD_REV, NEW_REV = '1' * 40, '2' * 40
OLD_IMAGE = 'diegueins680/tdf-hq@sha256:' + 'a' * 64
NEW_IMAGE = 'diegueins680/tdf-hq@sha256:' + 'b' * 64
PG_IMAGE = 'postgres@sha256:' + 'c' * 64
ORIGINAL_CADDY = b'api.tdfrecords.net {\n\treverse_proxy api:8080\n}\n'
OLD_LEDGER = ['2026-10-01_a', '2026-10-02_b']
NEW_IDS = OLD_LEDGER + ['2026-11-01_c', '2026-11-02_d']
SYSID = '7400000000000000001'
PARAMS = {
    'sourceRevision': NEW_REV, 'mobileRevision': '3' * 40, 'toolRevision': '4' * 40,
    'candidateImage': NEW_IMAGE, 'manifestHash': 'd' * 64, 'migrationIds': NEW_IDS,
    'recipient': 'age1' + 'q' * 58,
    'storage': {'host': 'u12345.your-storagebox.de', 'port': 23, 'user': 'u12345-sub1', 'directory': 'tdf-backups'},
    'smokePaths': ['/music/releases'],
}
CONFIRM = NEW_REV[:12]
OVERRIDE = '/opt/tdf-mail/api-smtp-image-override.yaml'
COMPOSE_FILES = ['/opt/tdf/production/compose.yaml', OVERRIDE]


def digest(data):
    return hashlib.sha256(data).hexdigest()


class FakeHost:
    def __init__(self, root):
        self.root = root
        self.tools = root / 'tools'
        self.files = {gr.DIRECTORY / 'Caddyfile': ORIGINAL_CADDY,
                      gr.DIRECTORY / '.env': ('POSTGRES_IMAGE=' + PG_IMAGE + '\nTDF_IMAGE=' + OLD_IMAGE + '\n').encode(),
                      # As in production: an override pins the API image by its bare id.
                      Path(OVERRIDE): gr.render_image_override('sha256:' + 'a' * 64)}
        self.compose_calls = []
        self.edge_config = ORIGINAL_CADDY
        self.running = {'api': True, 'db': True, 'edge': True}
        self.api_image = OLD_IMAGE
        self.ledger_rows = list(OLD_LEDGER)
        self.timer = 'active'
        self.clients = 0
        self.peers = None
        self.storage = {}
        self.events = []
        self.fail = set()
        self.api_id = 'e' * 64

    # observations used by plan()
    def observe_runtime(self):
        raw = self.files[Path(OVERRIDE)]
        return {'apiContainer': self.api_id, 'apiImage': self.api_image, 'composeFiles': list(COMPOSE_FILES),
                'apiImageOverride': {'path': OVERRIDE, 'sha256': digest(raw),
                                     'image': gr.parse_image_override(raw.decode())},
                'databaseContainer': 'f' * 64,
                'databaseImage': PG_IMAGE, 'databaseImageId': 'sha256:' + '9' * 64, 'publicCommit': OLD_REV,
                'caddyfileSha256': digest(self.files[gr.DIRECTORY / 'Caddyfile'])}

    def container(self, service):
        image = {'api': self.api_image, 'db': PG_IMAGE, 'edge': 'caddy'}[service]
        return {'Id': self.api_id if service == 'api' else service * 8,
                'Image': 'sha256:' + '9' * 64,
                'State': {'Running': self.running[service], 'Health': {'Status': 'healthy' if self.running[service] else 'none'}},
                'Config': {'Image': image},
                'NetworkSettings': {'Networks': {'tdf-production_outbound': {'IPAddress': '172.20.0.5'}}}}

    def docker(self, *args, **kwargs):
        self.events.append(('docker',) + args[:2])
        if args[:1] == ('exec',) and args[2:4] == ('cat', '/etc/caddy/Caddyfile'):
            # The bind-mounted file is visible at once; the active config changes on reload.
            return self.files[gr.DIRECTORY / 'Caddyfile'].decode()
        if args[:1] == ('exec',) and args[2] == 'caddy':
            self.edge_config = self.files[gr.DIRECTORY / 'Caddyfile']
            return ''
        if args[:1] == ('pull',):
            return ''
        if args[:2] == ('image', 'inspect'):
            return json.dumps([NEW_IMAGE])
        if args[:1] == ('run',):
            assert args[-1] == NEW_IMAGE and not self.running['api'] and self.running['db']
            if 'migrate' in self.fail:
                self.ledger_rows.append(NEW_IDS[2])  # partially applied, then failed
                raise gr.Rejected('host command failed: run')
            applied = NEW_IDS[:-1] if 'short-migrate' in self.fail else NEW_IDS
            self.ledger_rows = sorted(set(self.ledger_rows) | set(applied))
            return ''
        raise AssertionError('unexpected docker call %r' % (args,))

    def admin_file(self, path):
        return self.files[Path(path)]

    def compose(self, files, *args, **kwargs):
        self.compose_calls.append(list(files))
        self.events.append(('compose',) + args)
        verb, service = args[0], args[-1]
        if verb == 'stop':
            self.running[service] = False
        elif verb == 'start':
            self.running[service] = True
        elif verb == 'up':
            # Compose applies the override last, so its image wins over TDF_IMAGE.
            image = gr.parse_image_override(self.files[Path(OVERRIDE)].decode()) if OVERRIDE in files else None
            env = self.files[gr.DIRECTORY / '.env'].decode()
            configured = [line for line in env.split('\n') if line.startswith('TDF_IMAGE=')][0][10:]
            if image is None:
                self.api_image = configured
            else:
                self.api_image = image if '@' in image else {'a' * 64: OLD_IMAGE, 'b' * 64: NEW_IMAGE}[image[7:]]
            self.running['api'] = True
        return ''

    def read_bytes(self, path):
        path = Path(path)
        if path in self.files:
            return self.files[path]
        return path.read_bytes()

    def write_in_place(self, path, content):
        assert Path(path) in self.files
        self.files[Path(path)] = content

    def write_new(self, path, content):
        with open(path, 'xb') as handle:
            handle.write(content)

    def systemctl(self, *args):
        if args[0] == 'stop':
            self.timer = 'inactive'
        if args[0] == 'start':
            self.timer = 'active'
        state = self.timer if args[-1] == gr.BACKUP_TIMER else 'inactive'
        return types.SimpleNamespace(stdout=state + '\n', returncode=0)

    def sleep(self, seconds):
        pass

    def http(self, url, method='GET', body=None, timeout=15):
        if url.startswith(gr.PUBLIC):
            if gr.MAINTENANCE_MARKER.encode() in self.edge_config and 'maintenance' not in self.fail:
                return 503, {'X-Tdf-Maintenance': gr.MAINTENANCE_MARKER}, b'updating'
            if not self.running['api']:
                return 502, {}, b''
        elif not self.running['api']:
            return 0, {}, b''
        path = url.split('8080', 1)[-1] if '8080' in url else url[len(gr.PUBLIC):]
        commit = NEW_REV if self.api_image == NEW_IMAGE else OLD_REV
        if path == '/health':
            return 200, {}, json.dumps({'status': 'ok', 'db': 'ok'}).encode()
        if path == '/version':
            return 200, {}, json.dumps({'commit': commit}).encode()
        if path == '/music/releases':
            return (200 if commit == NEW_REV and 'smoke' not in self.fail else 404), {}, b'[]'
        return 404, {}, b''

    def ledger(self):
        assert self.running['db']
        return sorted(self.ledger_rows)

    def system_identifier(self):
        return SYSID

    def other_database_clients(self):
        return self.clients

    def database_network_peers(self):
        return self.peers if self.peers is not None else ['f' * 64]

    def cold_control(self, image):
        assert not self.running['db']
        return 'cold-control'

    def control_identity(self, text, system_id):
        return {'systemIdentifier': system_id, 'state': 'shut down', 'majorVersion': 17}

    def capture_tree(self, source, destination):
        data = ('captured:' + str(source)).encode()
        if str(source) == str(gr.VOLUME_DATA):
            data += json.dumps(self.ledger_rows).encode()
        Path(destination).write_bytes(data)
        return {'schemaVersion': 1, 'entries': [{'path': '', 'kind': 'directory'}], 'bytes': len(data),
                'source': str(source)}

    def admit_cold_manifest(self, manifest):
        pass

    def restore_tree(self, archive, manifest, destination):
        Path(destination).mkdir()
        (Path(destination) / 'restored').write_bytes(Path(archive).read_bytes())

    def encrypt(self, binary, source, destination, recipient):
        plain = Path(source).read_bytes()
        cipher = b'age:' + plain
        Path(destination).write_bytes(cipher)
        return {'plaintext': {'sha256': digest(plain), 'bytes': len(plain)},
                'ciphertext': {'sha256': digest(cipher), 'bytes': len(cipher)},
                'recipientSha256': digest(recipient.encode()), 'toolSha256': '0' * 64}

    def sftp(self, storage, batch):
        for line in batch.strip().split('\n'):
            parts = line.split()
            if parts[0] == 'put':
                self.storage[parts[2]] = Path(parts[1]).read_bytes()
            elif parts[0] == 'get':
                data = self.storage[parts[1]]
                if 'corrupt' in self.fail:
                    data = data[:-1] + b'X'
                Path(parts[2]).write_bytes(data)

    def isolated_start(self, release_dir, nonce, runtime, manifest, system_id):
        captured = (release_dir / 'database.tar').read_bytes()
        ledger = json.loads(captured[captured.index(b'['):])
        return {'status': 'physical-clone-started'}, sorted(ledger)


class GuardedReleaseTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        for name in ('backups', 'journal', 'tools', 'volume'):
            (self.root / name).mkdir(mode=0o700)
        (self.root / 'volume' / '_data').mkdir(mode=0o700)
        self.originals = {k: getattr(gr, k) for k in ('BACKUPS', 'VOLUME_DATA', 'CONFIG')}
        gr.BACKUPS, gr.VOLUME_DATA, gr.CONFIG = self.root / 'backups', self.root / 'volume' / '_data', self.root
        self.addCleanup(lambda: [setattr(gr, k, v) for k, v in self.originals.items()])
        self.host = FakeHost(self.root)

    def journal(self):
        return gr.journal_module.open_journal(str(self.root / 'journal'))

    def plan(self, params=PARAMS):
        with self.journal() as journal:
            return gr.plan(self.host, journal, copy.deepcopy(params))

    def run_phase(self, release_dir, phase, confirm=CONFIRM):
        with self.journal() as journal:
            return gr.Release(self.host, journal, release_dir).run_phase(phase, confirm)

    def rollback(self, release_dir, confirm='rollback-' + CONFIRM):
        with self.journal() as journal:
            return gr.Release(self.host, journal, release_dir).rollback(confirm)

    def status(self):
        with self.journal() as journal:
            return journal.status()

    def test_full_release_reaches_candidate_and_restores_edge(self):
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        # During the whole backup phase the public edge serves maintenance.
        self.assertIn(gr.MAINTENANCE_MARKER.encode(), self.host.edge_config)
        self.assertFalse(self.host.running['api'])
        self.assertTrue(any(k.endswith('/bundle.tar.age') for k in self.host.storage))
        self.run_phase(release, 'migrate')
        self.assertEqual(self.host.ledger(), sorted(NEW_IDS))
        self.assertFalse(self.host.running['api'])
        status = self.run_phase(release, 'cutover')
        self.assertTrue(status['sequenceComplete'])
        self.assertEqual(self.host.api_image, NEW_IMAGE)
        self.assertEqual(self.host.edge_config, ORIGINAL_CADDY)
        self.assertEqual(self.host.timer, 'active')
        env = self.host.files[gr.DIRECTORY / '.env'].decode()
        self.assertIn('TDF_IMAGE=' + NEW_IMAGE, env)
        self.assertEqual(self.host.files[Path(OVERRIDE)], gr.render_image_override(NEW_IMAGE))
        self.assertTrue(self.host.compose_calls and all(f == COMPOSE_FILES for f in self.host.compose_calls))
        self.assertTrue((release / 'compose-override.original').exists())
        self.assertIn('POSTGRES_IMAGE=' + PG_IMAGE, env)
        record = json.loads((release / 'bundle-record.json').read_bytes())
        self.assertEqual(record['sourceRevision'], NEW_REV)
        for stage in gr.STAGES:
            self.assertTrue((release / (stage + '.json')).exists(), stage)

    def test_phases_require_confirmation_and_order(self):
        release = Path(self.plan()['releaseDirectory'])
        with self.assertRaisesRegex(gr.Rejected, 'confirmation'):
            self.run_phase(release, 'backup', confirm='wrong')
        with self.assertRaisesRegex(gr.Rejected, 'in order'):
            self.run_phase(release, 'migrate')
        self.assertEqual(self.status()['completedStages'], [])
        self.assertTrue(self.host.running['api'])

    def test_rerunning_a_completed_phase_is_a_no_op(self):
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        before = len(self.host.events)
        self.run_phase(release, 'backup')
        self.assertEqual(len(self.host.events), before)

    def test_missing_maintenance_response_stops_and_blocks_retry(self):
        self.host.fail.add('maintenance')
        release = Path(self.plan()['releaseDirectory'])
        with self.assertRaisesRegex(gr.Rejected, 'maintenance response'):
            self.run_phase(release, 'backup')
        self.assertEqual(self.status()['pendingStage'], 'maintenance')
        self.assertTrue(self.host.running['api'])
        with self.assertRaisesRegex(gr.Rejected, 'interrupted'):
            self.run_phase(release, 'backup')

    def test_lingering_database_clients_stop_before_capture(self):
        self.host.clients = 2
        release = Path(self.plan()['releaseDirectory'])
        with self.assertRaisesRegex(gr.Rejected, 'client connections'):
            self.run_phase(release, 'backup')
        self.assertEqual(self.status()['pendingStage'], 'stop-writers')
        self.assertTrue(self.host.running['db'])

    def test_extra_database_network_peer_stops_release(self):
        self.host.peers = ['f' * 64, '0' * 64]
        release = Path(self.plan()['releaseDirectory'])
        with self.assertRaisesRegex(gr.Rejected, 'database network'):
            self.run_phase(release, 'backup')

    def test_corrupt_off_host_copy_stops_before_any_migration(self):
        self.host.fail.add('corrupt')
        release = Path(self.plan()['releaseDirectory'])
        with self.assertRaisesRegex(gr.Rejected, 'off-host copy'):
            self.run_phase(release, 'backup')
        self.assertEqual(self.status()['pendingStage'], 'retrieve-off-host')
        self.assertEqual(self.host.ledger_rows, OLD_LEDGER)

    def test_caddyfile_drift_since_plan_is_rejected(self):
        release = Path(self.plan()['releaseDirectory'])
        self.host.files[gr.DIRECTORY / 'Caddyfile'] = ORIGINAL_CADDY + b'# edited\n'
        with self.assertRaisesRegex(gr.Rejected, 'Caddyfile changed'):
            self.run_phase(release, 'backup')

    def test_tampered_parameters_no_longer_match_the_journal(self):
        release = Path(self.plan()['releaseDirectory'])
        tampered = dict(PARAMS, smokePaths=[])
        (release / 'params.json').unlink()
        (release / 'params.json').write_bytes(gr.canonical(tampered))
        with self.assertRaisesRegex(gr.Rejected, 'no longer match'):
            self.run_phase(release, 'backup')

    def test_plan_refuses_when_candidate_already_runs(self):
        self.host.api_image = NEW_IMAGE
        with self.assertRaisesRegex(gr.Rejected, 'already running'):
            self.plan()

    def test_failed_migration_rollback_restores_captured_database(self):
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        self.host.fail.add('migrate')
        with self.assertRaises(gr.Rejected):
            self.run_phase(release, 'migrate')
        self.assertEqual(self.status()['pendingStage'], 'migrate')
        # The fake volume restore reinstates the captured (pre-migration) ledger.
        self.host.ledger_rows = list(OLD_LEDGER)
        receipt = self.rollback(release)
        self.assertTrue(receipt['databaseRestored'])
        self.assertTrue((release / 'failed-data').is_dir())
        self.assertTrue((gr.VOLUME_DATA / 'restored').exists())
        self.assertEqual(self.host.api_image, OLD_IMAGE)
        self.assertEqual(self.host.edge_config, ORIGINAL_CADDY)
        self.assertEqual(self.host.timer, 'active')
        self.assertIn('reopened', receipt['steps'])

    def test_successful_migration_run_with_missing_ledger_row_is_rejected(self):
        # e.g. an image built from a different manifest exits 0 but skips a migration.
        self.host.fail.add('short-migrate')
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        with self.assertRaisesRegex(gr.Rejected, 'migrations missing after apply: ' + NEW_IDS[-1]):
            self.run_phase(release, 'migrate')
        self.assertEqual(self.status()['pendingStage'], 'migrate')
        self.assertFalse(self.host.running['api'])

    def test_rollback_before_migration_keeps_production_database(self):
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        receipt = self.rollback(release)
        self.assertFalse(receipt['databaseRestored'])
        self.assertFalse((release / 'failed-data').exists())
        self.assertTrue(self.host.running['db'] and self.host.running['api'])
        self.assertEqual(self.host.api_image, OLD_IMAGE)
        self.assertEqual(self.host.edge_config, ORIGINAL_CADDY)

    def test_failed_smoke_check_rolls_back_the_candidate_image(self):
        self.host.fail.add('smoke')
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        self.run_phase(release, 'migrate')
        with self.assertRaisesRegex(gr.Rejected, 'smoke check failed'):
            self.run_phase(release, 'cutover')
        self.assertIn(gr.MAINTENANCE_MARKER.encode(), self.host.edge_config)
        self.host.ledger_rows = list(OLD_LEDGER)
        receipt = self.rollback(release)
        self.assertTrue(receipt['databaseRestored'])
        self.assertIn('restored-env', receipt['steps'])
        self.assertIn('restored-override', receipt['steps'])
        self.assertEqual(self.host.files[Path(OVERRIDE)], gr.render_image_override('sha256:' + 'a' * 64))
        self.assertIn('TDF_IMAGE=' + OLD_IMAGE, self.host.files[gr.DIRECTORY / '.env'].decode())
        self.assertEqual(self.host.api_image, OLD_IMAGE)

    def test_rollback_after_reopen_is_refused(self):
        release = Path(self.plan()['releaseDirectory'])
        for phase in ('backup', 'migrate', 'cutover'):
            self.run_phase(release, phase)
        with self.assertRaisesRegex(gr.Rejected, 'discard real writes'):
            self.rollback(release)

    def test_rollback_requires_its_own_confirmation(self):
        release = Path(self.plan()['releaseDirectory'])
        self.run_phase(release, 'backup')
        with self.assertRaisesRegex(gr.Rejected, 'rollback confirmation'):
            self.rollback(release, confirm=CONFIRM)

    def test_second_release_follows_a_completed_one(self):
        release = Path(self.plan()['releaseDirectory'])
        for phase in ('backup', 'migrate', 'cutover'):
            self.run_phase(release, phase)
        newer = dict(PARAMS, sourceRevision='5' * 40, candidateImage='diegueins680/tdf-hq@sha256:' + '6' * 64)
        self.host.docker = lambda *a, **k: json.dumps([newer['candidateImage']]) if a[:2] == ('image', 'inspect') else ''
        second = self.plan(newer)
        self.assertEqual(second['releaseNonce'], self.status()['releaseNonce'])
        self.assertEqual(self.status()['completedStages'], [])
        self.assertEqual(self.status()['priorReleases'], 1)


class OverrideTests(unittest.TestCase):
    def test_compose_files_follow_the_api_label(self):
        label = ','.join(COMPOSE_FILES)
        self.assertEqual(gr.admit_compose_files(label, ['/opt/tdf/production/compose.yaml'] * 2), COMPOSE_FILES)
        self.assertEqual(gr.admit_compose_files('/opt/tdf/production/compose.yaml', []),
                         ['/opt/tdf/production/compose.yaml'])
        for bad_label, others in [
                ('/srv/other/compose.yaml', []),
                (label + ',/opt/tdf-mail/second.yaml', []),
                ('/opt/tdf/production/compose.yaml,/opt/../etc/x.yaml', []),
                ('/opt/tdf/production/compose.yaml,/tmp/x.yaml', []),
                (label, ['/opt/tdf/production/compose.yaml,/opt/tdf-mail/edge.yaml'])]:
            with self.assertRaises(gr.Rejected):
                gr.admit_compose_files(bad_label, others)

    def test_only_an_exact_image_override_is_admitted(self):
        for image in (NEW_IMAGE, 'sha256:' + 'a' * 64):
            self.assertEqual(gr.parse_image_override(gr.render_image_override(image).decode()), image)
        for text in ['services:\n  api:\n    image: diegueins680/tdf-hq:latest\n',
                     'services:\n  api:\n    image: ' + NEW_IMAGE + '\n    environment:\n      A: b\n',
                     'services:\n  db:\n    image: ' + NEW_IMAGE + '\n',
                     'services:\n  api:\n    image: ' + NEW_IMAGE]:
            with self.assertRaises(gr.Rejected):
                gr.parse_image_override(text)


def production_shaped(service, *, files=None, image=None, running=True):
    """Container inspect output shaped like the live host on 13 November 2026."""
    labels = {'com.docker.compose.project': 'tdf-production', 'com.docker.compose.service': service,
              'com.docker.compose.project.working_dir': '/opt/tdf/production',
              'com.docker.compose.project.config_files': files or '/opt/tdf/production/compose.yaml'}
    networks = {'db': ['tdf-production_database'], 'api': ['tdf-production_database', 'tdf-production_outbound'],
                'edge': ['tdf-production_outbound']}[service]
    container = {'Id': {'api': 'e', 'db': 'f', 'edge': 'd'}[service] * 64, 'Image': 'sha256:' + '9' * 64,
                 'State': {'Running': running, 'Health': {'Status': 'healthy'}},
                 'Config': {'Image': image, 'Labels': labels, 'Env': ['PGDATA=/var/lib/postgresql/data']},
                 'NetworkSettings': {'Networks': {n: {} for n in networks}, 'Ports': {}},
                 'Mounts': []}
    if service == 'db':
        container['Mounts'] = [{'Type': 'volume', 'Name': gr.VOLUME, 'Destination': '/var/lib/postgresql/data'}]
    return container


class ObservedRuntimeTests(unittest.TestCase):
    """The real Host.observe_runtime against production-shaped inspect output."""

    def host(self, *, api_files=','.join(COMPOSE_FILES), api_image='sha256:' + 'a' * 64,
             override=None, env_image=OLD_IMAGE, repo_digests=(OLD_IMAGE,)):
        test = self

        class Observed(gr.Host):
            def __init__(self):
                self._runtime = gr.load('observed_runtime', 'inspect-runtime.py')

            def container(self, service):
                return {'api': production_shaped('api', files=api_files, image=api_image),
                        'db': production_shaped('db', image=PG_IMAGE),
                        'edge': production_shaped('edge', image='caddy@sha256:' + '8' * 64)}[service]

            def read_bytes(self, path):
                if Path(path) == gr.DIRECTORY / '.env':
                    return ('POSTGRES_IMAGE=' + PG_IMAGE + '\nTDF_IMAGE=' + env_image + '\n').encode()
                return ORIGINAL_CADDY

            def admin_file(self, path):
                test.assertEqual(path, OVERRIDE)
                return override if override is not None else gr.render_image_override(api_image)

            def docker(self, *args, **kwargs):
                test.assertEqual(args[:2], ('image', 'inspect'))
                return json.dumps(list(repo_digests))

            def http(self, url, **kwargs):
                return 200, {}, json.dumps({'commit': OLD_REV}).encode()
        return Observed()

    def test_live_shape_with_image_override_is_admitted(self):
        runtime = self.host().observe_runtime()
        self.assertEqual(runtime['composeFiles'], COMPOSE_FILES)
        self.assertEqual(runtime['apiImage'], OLD_IMAGE)
        self.assertEqual(runtime['apiImageOverride']['image'], 'sha256:' + 'a' * 64)

    def test_plain_compose_without_override_is_admitted(self):
        runtime = self.host(api_files='/opt/tdf/production/compose.yaml', api_image=OLD_IMAGE).observe_runtime()
        self.assertIsNone(runtime['apiImageOverride'])

    def test_override_naming_a_different_image_than_running_is_rejected(self):
        with self.assertRaisesRegex(gr.Rejected, 'configured image'):
            self.host(override=gr.render_image_override('sha256:' + 'c' * 64)).observe_runtime()

    def test_running_image_without_the_tdf_image_digest_is_rejected(self):
        with self.assertRaisesRegex(gr.Rejected, 'TDF_IMAGE digest'):
            self.host(repo_digests=(NEW_IMAGE,)).observe_runtime()

    def test_override_with_extra_settings_is_rejected(self):
        extra = gr.render_image_override('sha256:' + 'a' * 64) + b'    environment:\n      SMTP_HOST: x\n'
        with self.assertRaisesRegex(gr.Rejected, 'unreviewed shape'):
            self.host(override=extra).observe_runtime()

    def test_compose_commands_name_every_planned_file(self):
        calls = []

        class Recording(gr.Host):
            def __init__(self):
                pass

            def docker(self, *args, **kwargs):
                calls.append(args)
                return ''
        Recording().compose(COMPOSE_FILES, 'up', '--detach', '--no-deps', 'api')
        args = calls[0]
        self.assertEqual([args[i + 1] for i, a in enumerate(args) if a == '-f'], COMPOSE_FILES)
        self.assertEqual(args[-4:], ('up', '--detach', '--no-deps', 'api'))

    def test_journal_is_the_single_global_directory(self):
        self.assertEqual(gr.JOURNAL, Path('/opt/tdf/release-control'))
        self.assertNotIn(gr.JOURNAL, [gr.CONFIG, *gr.CONFIG.parents][:-3])
        self.assertFalse(str(gr.CONFIG).startswith(str(gr.JOURNAL) + '/'))


class ParameterTests(unittest.TestCase):
    def test_env_value_replacement_is_exact(self):
        text = 'A=1\nTDF_IMAGE=' + OLD_IMAGE + '\nB=2'
        self.assertEqual(gr.replace_env_value(text, 'TDF_IMAGE', OLD_IMAGE, NEW_IMAGE),
                         'A=1\nTDF_IMAGE=' + NEW_IMAGE + '\nB=2')
        with self.assertRaises(gr.Rejected):
            gr.replace_env_value(text, 'TDF_IMAGE', NEW_IMAGE, OLD_IMAGE)
        with self.assertRaises(gr.Rejected):
            gr.replace_env_value(text + '\nTDF_IMAGE=x', 'TDF_IMAGE', OLD_IMAGE, NEW_IMAGE)

    def test_parameter_validation(self):
        gr.validate_params(copy.deepcopy(PARAMS))
        bad = [dict(PARAMS, candidateImage='diegueins680/tdf-hq:latest'),
               dict(PARAMS, recipient='AGE-SECRET-KEY-1' + 'Q' * 58),
               dict(PARAMS, storage=dict(PARAMS['storage'], directory='../escape')),
               dict(PARAMS, smokePaths=['https://elsewhere.example/']),
               dict(PARAMS, migrationIds=['a', 'a']),
               dict(PARAMS, extra=True)]
        for params in bad:
            with self.assertRaises(gr.Rejected):
                gr.validate_params(params)

    def test_phases_cover_journal_stages(self):
        self.assertEqual(sum(gr.PHASES.values(), ()), gr.STAGES)


if __name__ == '__main__':
    unittest.main()
