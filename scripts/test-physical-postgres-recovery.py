#!/usr/bin/env python3
"""Cold-copy admission controls; Docker integration is a separate opt-in check."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

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
