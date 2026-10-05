#!/usr/bin/env python3
"""Cold-copy admission controls; Docker integration is a separate opt-in check."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch, Mock
from contextlib import contextmanager
from types import SimpleNamespace

spec = importlib.util.spec_from_file_location('physical', Path(__file__).resolve().parent.parent/
                                            'ops/hetzner/physical-postgres-recovery.py')
physical = importlib.util.module_from_spec(spec); spec.loader.exec_module(physical)
CONTROL = 'pg_control version number: 1700\nDatabase system identifier: 12345\nDatabase cluster state: shut down\n'


class PhysicalRecoveryTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(); self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.directory = self.root/('rehearsal-'+'a'*32); self.directory.mkdir(mode=0o700)
        self.data = self.directory/'physical-data'; self.data.mkdir(mode=0o700)
        for name in ('base', 'global', 'pg_wal', 'pg_tblspc'): (self.data/name).mkdir()
        (self.data/'PG_VERSION').write_text('17\n')
        (self.data/'global'/'pg_control').write_bytes(b'synthetic control file')
        (self.data/'postgresql.auto.conf').write_text("archive_command = 'unsafe-original'\n")
        self.host_patch = patch.object(physical, 'HOST_ROOT', self.root); self.host_patch.start()
        self.addCleanup(self.host_patch.stop)
        self.mount_patch = patch.object(physical, 'verify_mounts'); self.mount_patch.start()
        self.addCleanup(self.mount_patch.stop)
        self.clone = physical.PhysicalClone('1'*64, 'pgvector/pgvector@sha256:'+'2'*64,
                    'sha256:'+'2'*64, 'a'*32, self.directory, '12345')

    def manifest(self):
        with physical.files.directory(str(self.data)) as fd: return physical.files.walk(fd)

    def candidate_manifest(self):
        value = self.manifest(); value['entries'][0].update(uid=999, gid=999)
        return value

    def prepared(self):
        # Unit fixture cannot chown on ordinary developer hosts. The real Linux
        # integration checks UID999; do not weaken production admission for it.
        with patch.object(physical, 'admit_cold_manifest'):
            return self.clone.prepare(self.manifest())

    def inspection(self):
        return {'Id': '3'*64, 'Image': self.clone.image_id,
            'Config': {'Labels': {physical.restore.LABEL: self.clone.nonce}, 'Image': self.clone.image,
                       'User': '999:999', 'Entrypoint': ['/bin/sleep'], 'Cmd': ['600']},
            'NetworkSettings': {'Networks': {'none': {}}},
            'HostConfig': {'NetworkMode': 'none', 'ReadonlyRootfs': True,
                'Memory': physical.restore.MEMORY_LIMIT, 'MemorySwap': physical.restore.MEMORY_LIMIT,
                'NanoCpus': 500000000, 'PidsLimit': 64, 'CapDrop': ['ALL'], 'IpcMode': 'private',
                'SecurityOpt': ['no-new-privileges:true'], 'Tmpfs': {'/tmp': 'rw,nosuid,nodev,size=16777216'}},
            'Mounts': [{'Destination': physical.DATA, 'Type': 'bind', 'Source': str(self.data),
                        'RW': True, 'Propagation': 'rprivate'},
                       {'Destination': physical.CONFIG, 'Type': 'bind', 'Source': str(self.clone.config),
                        'RW': False, 'Propagation': 'rprivate'}, {'Destination': '/tmp', 'Type': 'tmpfs'}]}

    def test_cold_cluster_identity_and_forced_stop_rejection(self):
        self.assertEqual(physical.control_identity(CONTROL, '12345')['majorVersion'], 17)
        for value in (CONTROL.replace('shut down', 'in production'),
                      CONTROL.replace('shut down', 'shut down in recovery'),
                      CONTROL.replace('12345', '67890'), CONTROL.replace('1700', '1300'),
                      CONTROL+'Database cluster state: shut down\n', ''):
            with self.subTest(value=value), self.assertRaises(ValueError):
                physical.control_identity(value, '12345')

    def test_live_backup_standby_and_external_tablespaces_rejected(self):
        physical.admit_cold_manifest(self.candidate_manifest())
        for name in ('postmaster.pid', 'backup_label', 'tablespace_map', 'recovery.signal',
                     'standby.signal', 'pg_tblspc/12345'):
            (self.data/name).write_text('synthetic')
            with self.subTest(name=name), self.assertRaises(ValueError):
                physical.admit_cold_manifest(self.candidate_manifest())
            (self.data/name).unlink()
        value = self.candidate_manifest(); value['entries'][0]['uid'] = 0
        with self.assertRaises(ValueError): physical.admit_cold_manifest(value)

    def test_prepare_verifies_bytes_before_changing_only_clone_settings(self):
        before = self.manifest()
        result = self.prepared()
        self.assertEqual((self.data/'postgresql.auto.conf').read_bytes(), b'')
        self.assertEqual((self.data/'global'/'pg_control').read_bytes(), b'synthetic control file')
        self.assertNotEqual(result['originalManifestHash'], result['cloneManifestHash'])
        self.clone.verify_prepared()
        self.assertEqual(len(before['entries']), len(self.clone.prepared_manifest['entries']))
        with self.assertRaises(ValueError): self.prepared()

    def test_restrictive_umask_keeps_clone_config_readable_and_verified(self):
        previous = os.umask(0o077)
        try: self.prepared(); self.clone.verify_prepared()
        finally: os.umask(previous)
        for path in self.clone.config.iterdir(): self.assertEqual(path.stat().st_mode & 0o777, 0o444)

    def test_bind_mounts_of_backup_parent_or_target_rejected_before_mutation(self):
        root_line = '1 0 8:1 / / rw - ext4 /dev/root rw\n'
        physical.admit_mounts(root_line)
        for mount in (self.root, self.directory, self.data, self.data/'global', self.root.parent):
            with self.subTest(mount=str(mount)), self.assertRaises(ValueError):
                physical.admit_mounts(root_line+'2 1 8:1 /production '+str(mount)+' rw - ext4 /dev/root rw\n')
        with patch.object(physical, 'verify_mounts', side_effect=ValueError('mount rejected')):
            with self.assertRaises(ValueError): self.prepared()
        self.assertIn('unsafe-original', (self.data/'postgresql.auto.conf').read_text())

    def test_tampered_copy_never_changes_auto_conf(self):
        before = self.manifest()
        (self.data/'global'/'pg_control').write_bytes(b'tampered')
        with patch.object(physical, 'admit_cold_manifest'), self.assertRaises(ValueError):
            self.clone.prepare(before)
        self.assertIn('unsafe-original', (self.data/'postgresql.auto.conf').read_text())
        self.assertFalse(self.clone.config.exists())

    def test_wrong_major_version_has_no_configuration_effect(self):
        (self.data/'PG_VERSION').write_text('16\n')
        with self.assertRaises(ValueError): self.prepared()
        self.assertIn('unsafe-original', (self.data/'postgresql.auto.conf').read_text())

    def test_post_prepare_tampering_rejected(self):
        self.prepared()
        file = self.clone.config/'pg_hba.conf'
        file.chmod(0o644); file.write_text('host all all 0.0.0.0/0 trust\n'); file.chmod(0o444)
        with self.assertRaises(ValueError): self.clone.verify_prepared()

    def test_symlink_ancestor_rejected(self):
        path = self.directory/'alias'; path.symlink_to(self.data, target_is_directory=True)
        self.clone.data = path
        with patch.object(physical, 'admit_cold_manifest'), self.assertRaises(OSError):
            self.clone.prepare(self.manifest())

    def test_container_admission_rejects_production_network_mount_and_privilege(self):
        self.clone.admit(self.inspection())
        before_start = self.inspection(); before_start['Mounts'].pop()
        self.clone.admit(before_start)
        mutations = [lambda d: d['HostConfig'].update(NetworkMode='host'),
            lambda d: d['HostConfig'].update(Privileged=True),
            lambda d: d['HostConfig'].update(CapAdd=['SYS_ADMIN']),
            lambda d: d['HostConfig'].update(PortBindings={'5432/tcp': [{}]}),
            lambda d: d['Mounts'][0].update(Source='/var/lib/docker/volumes/production/_data'),
            lambda d: d['Mounts'][0].update(Propagation='rshared'),
            lambda d: d['Mounts'][1].update(RW=True),
            lambda d: d['Mounts'].append(dict(d['Mounts'][0])),
            lambda d: d['Config'].update(Entrypoint=['docker-entrypoint.sh']),
            lambda d: d.update(Id='1'*64),
            lambda d: d['Config'].update(User='0:0'),
            lambda d: d['HostConfig'].update(MemorySwap=0)]
        for mutate in mutations:
            data = self.inspection(); mutate(data)
            with self.subTest(mutation=mutate), self.assertRaises(ValueError): self.clone.admit(data)

    def test_start_requires_live_reservation_before_any_docker_call(self):
        self.prepared()
        with patch.object(physical.restore, 'execute') as execute, self.assertRaises(ValueError):
            self.clone.start()
        execute.assert_not_called()

    def test_restored_application_copies_require_full_content_metadata_and_registration(self):
        application = SimpleNamespace(database=self.clone, nonce=self.clone.nonce, directory=self.directory)
        self.clone.reservation_pid = os.getpid(); self.clone.active_application = application
        manifests = {}
        actual_walk = physical.files.walk
        observed_uid = 1000
        def observe_test_owner(fd):
            value = actual_walk(fd)
            # Ordinary macOS accounts cannot chown1000. Only the root ownership
            # observation is substituted; byte/metadata scans remain real. The
            # Linux combined Docker fixture restores actual UID/GID1000 copies.
            value['entries'][0].update(uid=observed_uid, gid=1000)
            return value
        for name in ('assets', 'uploads'):
            path = self.directory/('canary-'+name); path.mkdir(mode=0o700)
            (path/'sentinel').write_bytes(b'original synthetic content')
            with physical.files.directory(str(path)) as fd:
                manifests[name] = observe_test_owner(fd)
        with patch.object(physical.files, 'walk', side_effect=observe_test_owner):
            evidence = self.clone.admit_application_content(application, manifests)
            self.assertEqual(set(evidence), {'assets', 'uploads'})
            self.assertEqual(evidence['uploads']['bytes'], len(b'original synthetic content'))
            path = self.directory/'canary-uploads'/'sentinel'
            path.write_bytes(b'tampered synthetic content')
            with self.assertRaises(ValueError): self.clone.admit_application_content(application, manifests)
            path.write_bytes(b'original synthetic content')
            original = next(row for row in manifests['uploads']['entries'] if row['path'] == 'sentinel')
            os.utime(path, ns=(original['mtimeNs'], original['mtimeNs']))
            self.clone.admit_application_content(application, manifests)
            for change in (lambda m: m.pop('uploads'), lambda m: m.update(extra=manifests['assets'])):
                invalid = copy.deepcopy(manifests); change(invalid)
                with self.assertRaises(ValueError): self.clone.admit_application_content(application, invalid)
            # Match the complete observed manifests so only the image-user
            # ownership/permission policy can reject these controls.
            observed_uid = 0
            invalid = copy.deepcopy(manifests)
            for manifest in invalid.values(): manifest['entries'][0]['uid'] = 0
            with self.assertRaises(ValueError): self.clone.admit_application_content(application, invalid)
            observed_uid = 1000
            invalid = copy.deepcopy(manifests)
            for name, manifest in invalid.items():
                (self.directory/('canary-'+name)).chmod(0o500)
                manifest['entries'][0]['mode'] = 0o500
            try:
                with self.assertRaises(ValueError): self.clone.admit_application_content(application, invalid)
            finally:
                for name in manifests: (self.directory/('canary-'+name)).chmod(0o700)
            self.clone.admit_application_content(application, manifests)
        self.clone.active_application = None
        with patch.object(physical.files, 'walk') as scan:
            with self.assertRaises(ValueError): self.clone.admit_application_content(application, manifests)
        scan.assert_not_called()

    @contextmanager
    def reservation(self):
        # Real permanent lock and pending marker; only Linux host inventory and
        # resource readings are synthetic on developer hosts.
        original_read = physical.Path.read_text
        def read(path, *args, **kwargs):
            return 'MemAvailable: 2097152 kB\n' if str(path) == '/proc/meminfo' else original_read(path, *args, **kwargs)
        with patch.object(physical.restore, 'execute', return_value=''), \
             patch.object(physical.Path, 'read_text', new=read), \
             patch.object(physical.os, 'statvfs', return_value=SimpleNamespace(f_bavail=4*1024**3, f_frsize=1)):
            with self.clone.reserved(): yield

    def application(self, cleanup=None):
        app = SimpleNamespace(database=self.clone, nonce=self.clone.nonce,
              directory=self.clone.directory, target=None, creation_attempted=False, paused=False)
        app.cleanup = Mock(side_effect=cleanup)
        return app

    def test_application_dependency_is_registered_before_creation_and_removed_first(self):
        order=[]; app=self.application(lambda: order.append('application'))
        def database_cleanup():
            order.append('database'); self.clone.target=None
        with patch.object(self.clone, 'cleanup', side_effect=database_cleanup):
            with self.reservation():
                self.clone.target='3'*64
                with self.clone.with_application(app):
                    self.clone.require_application_owner(app)
                    self.assertTrue((self.root/physical.restore.PENDING_NAME).exists())
            self.assertEqual(order,['application','database'])
        self.assertFalse((self.root/physical.restore.PENDING_NAME).exists())
        self.assertIsNone(self.clone.active_application)
        self.assertIsNone(self.clone.reservation_pid)

    def test_failed_application_cleanup_preserves_database_and_durable_marker(self):
        app=self.application(lambda: (_ for _ in ()).throw(ValueError('Synthetic uncertain removal')))
        with patch.object(self.clone,'cleanup') as database_cleanup:
            with self.assertRaises(ValueError):
                with self.reservation():
                    self.clone.target='3'*64
                    with self.clone.with_application(app): pass
            database_cleanup.assert_not_called()
        self.assertEqual(self.clone.target,'3'*64)
        self.assertIs(self.clone.active_application,app)
        self.assertTrue((self.root/physical.restore.PENDING_NAME).exists())
        self.assertIsNone(self.clone.reservation_pid)
        with self.assertRaises(ValueError):
            with self.reservation(): pass

    def test_cleanup_return_without_removed_application_does_not_release_database(self):
        app=self.application()
        with patch.object(self.clone,'cleanup') as database_cleanup:
            with self.assertRaises(ValueError):
                with self.reservation():
                    self.clone.target='3'*64
                    with self.clone.with_application(app):
                        app.creation_attempted=True
            database_cleanup.assert_not_called()
        self.assertTrue((self.root/physical.restore.PENDING_NAME).exists())

    def test_application_failure_still_cleans_up_in_dependency_order(self):
        app=self.application()
        def database_cleanup(): self.clone.target=None
        with patch.object(self.clone,'cleanup',side_effect=database_cleanup):
            with self.assertRaisesRegex(RuntimeError,'Synthetic application failure'):
                with self.reservation():
                    self.clone.target='3'*64
                    with self.clone.with_application(app):
                        raise RuntimeError('Synthetic application failure')
        app.cleanup.assert_called_once()
        self.assertFalse((self.root/physical.restore.PENDING_NAME).exists())

    def test_foreign_unregistered_and_already_started_application_rejected(self):
        app=self.application()
        with self.assertRaises(ValueError): self.clone.require_application_owner(app)
        with self.reservation():
            self.clone.target='3'*64
            with self.assertRaises(ValueError): self.clone.require_application_owner(app)
            for key, value in [('nonce','b'*32), ('database',object()), ('target','4'*64),
                               ('creation_attempted',True), ('paused',True),
                               ('directory',self.root/'foreign')]:
                original=getattr(app,key);setattr(app,key,value)
                with self.subTest(key=key),self.assertRaises(ValueError):
                    with self.clone.with_application(app): pass
                setattr(app,key,original)
            self.clone.target=None  # no Docker creation occurred in this fixture
        app.cleanup.assert_not_called()

    def test_command_has_no_initdb_or_original_configuration(self):
        self.prepared(); command = self.clone.create_command()
        self.assertIn('--pull=never', command)
        self.assertIn('--network=none', command)
        self.assertEqual(command[-2:], [self.clone.image, '600'])
        self.assertNotIn('docker-entrypoint.sh', command)
        self.assertNotIn('initdb', command)
        self.assertNotIn('POSTGRES_HOST_AUTH_METHOD=trust', command)
        self.assertIn('config_file=/recovery-config/postgresql.conf', physical.POSTGRES)


if __name__ == '__main__': unittest.main()
