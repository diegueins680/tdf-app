#!/usr/bin/env python3
"""Admission controls; retained Linux namespace execution has a Docker fixture."""
import copy
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
from contextlib import ExitStack
from types import SimpleNamespace

spec = importlib.util.spec_from_file_location('stopped', Path(__file__).resolve().parent.parent/
                                            'ops/hetzner/stopped-application-storage.py')
s = importlib.util.module_from_spec(spec); spec.loader.exec_module(s)
TARGET='1'*64; IMAGE='pgvector/pgvector@sha256:'+'2'*64; IMAGE_ID='sha256:'+'3'*64


def state(running=True):
    return {'Id':TARGET, 'Image':IMAGE_ID, 'Config':{'Image':IMAGE}, 'Mounts':[],
            'State':{'Running':running, 'Restarting':False, 'Paused':False, 'Dead':False,
                     'OOMKilled':False, 'StartedAt':'2026-10-05T00:00:00Z',
                     'Pid':123 if running else 0, 'Status':'running' if running else 'exited', 'ExitCode':0}}


class StorageTests(unittest.TestCase):
    def test_source_identity_state_mount_and_restart_binding(self):
        subject=s.RetainedRoot(TARGET,IMAGE,IMAGE_ID)
        valid=state()
        with patch.object(s,'inspection',return_value=valid): subject.inspect(running=True)
        for change in (lambda v:v.update(Id='4'*64), lambda v:v.update(Image='sha256:'+'4'*64),
                       lambda v:v['Config'].update(Image='pgvector:latest'),
                       lambda v:v['State'].update(Running=False), lambda v:v['State'].update(Pid=True),
                       lambda v:v['State'].update(Restarting=True), lambda v:v['State'].update(Paused=True),
                       lambda v:v['State'].update(Dead=True), lambda v:v['State'].update(OOMKilled=True)):
            value=copy.deepcopy(valid); change(value)
            with patch.object(s,'inspection',return_value=value),self.assertRaises(ValueError):subject.inspect(running=True)
        subject.start_time=valid['State']['StartedAt'];subject.mounts=[];subject.pid=123
        for change in (lambda v:v['State'].update(StartedAt='changed'), lambda v:v['State'].update(Pid=124),
                       lambda v:v.update(Mounts=[{'Destination':'/data'}])):
            value=copy.deepcopy(valid);change(value)
            with patch.object(s,'inspection',return_value=value),self.assertRaises(ValueError):subject.inspect(running=True)

    def test_legacy_tree_cannot_be_a_mount_or_have_shadowed_children(self):
        s.admit_legacy_mounts([{'Destination':'/data/assets'}])
        for path in ('/', '/app', '/app/uploads', '/app/uploads/child', '/app/../app/uploads', '/app//uploads'):
            with self.subTest(path=path),self.assertRaises(ValueError):s.admit_legacy_mounts([{'Destination':path}])

    def test_actual_mount_table_rejects_runtime_same_filesystem_aliases(self):
        root='1 0 8:1 / / rw - overlay overlay rw\n'
        s.admit_mountinfo(root+'2 1 0:1 / /proc rw - proc proc rw\n')
        for path in ('/app','/app/uploads','/app/uploads/hidden'):
            with self.subTest(path=path),self.assertRaises(ValueError):
                s.admit_mountinfo(root+'3 1 8:1 /private '+path+' rw - overlay overlay rw\n')
        for invalid in ('',root+root,'broken'):
            with self.assertRaises(ValueError):s.admit_mountinfo(invalid)

    def test_capture_requires_retained_owner_and_stopped_source(self):
        subject=s.RetainedRoot(TARGET,IMAGE,IMAGE_ID)
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve(); output=root/'never.tar'
            with patch.object(s,'inspection') as inspect,self.assertRaises(ValueError):subject.capture_uploads(str(output))
            inspect.assert_not_called();self.assertFalse(output.exists())
            with s.files.directory(str(root)) as fd:
                subject.owner_pid=os.getpid();subject.root_fd=subject.namespace_fd=subject.pid_fd=fd
                with patch.object(s,'inspection',return_value=state()),self.assertRaises(ValueError):subject.capture_uploads(str(output))
                for key,value in (('ExitCode',137),('ExitCode',True),('Pid',123),('Pid',False),('Status','created')):
                    wrong=state(False);wrong['State'][key]=value
                    with patch.object(s,'inspection',return_value=wrong),self.assertRaises(ValueError):subject.capture_uploads(str(output))
                self.assertFalse(output.exists())

    def test_process_exit_during_root_admission_closes_all_handles_and_never_yields(self):
        subject=s.RetainedRoot(TARGET,IMAGE,IMAGE_ID)
        same_inode=SimpleNamespace(st_dev=1,st_ino=2)
        with ExitStack() as stack:
            stack.enter_context(patch.object(s.sys,'platform','linux'))
            stack.enter_context(patch.object(s.os,'geteuid',return_value=0))
            stack.enter_context(patch.object(s.os,'pidfd_open',create=True,return_value=10))
            stack.enter_context(patch.object(s.os,'open',side_effect=[11,12]))
            closed=stack.enter_context(patch.object(s.os,'close'))
            stack.enter_context(patch.object(s.os,'stat',return_value=same_inode))
            stack.enter_context(patch.object(s.os,'fstat',return_value=same_inode))
            stack.enter_context(patch.object(s.Path,'read_text',return_value='0::/system.slice/docker-'+TARGET+'.scope\n'))
            stack.enter_context(patch.object(s,'inspection',return_value=state()))
            stack.enter_context(patch.object(s,'verify_namespace'))
            stack.enter_context(patch.object(s.select,'select',return_value=([10],[],[])))
            with self.assertRaises(ValueError):
                with subject.pinned(): pass
            self.assertEqual([call.args[0] for call in closed.call_args_list],[12,11,10])
        self.assertIsNone(subject.owner_pid)
        with self.assertRaises(ValueError):subject.guard()

    def test_real_retained_tree_capture_absence_and_symlink_rejection(self):
        subject=s.RetainedRoot(TARGET,IMAGE,IMAGE_ID)
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve(); source=root/'rootfs';source.mkdir()
            with s.files.directory(str(source)) as fd,patch.object(s,'inspection',return_value=state(False)), \
                    patch.object(s,'verify_namespace'):
                # Only Linux process/root admission is substituted. Reads,
                # no-follow traversal, manifests and archive replay are real.
                subject.owner_pid=os.getpid();subject.root_fd=subject.namespace_fd=subject.pid_fd=fd
                absent=subject.capture_uploads(str(root/'absent.tar'))
                self.assertEqual(absent['presence'],'absent');self.assertFalse((root/'absent.tar').exists())
                (source/'app').mkdir(); (source/'app'/'uploads').symlink_to(root, target_is_directory=True)
                with self.assertRaises(OSError): subject.capture_uploads(str(root/'alias.tar'))
                self.assertFalse((root/'alias.tar').exists());(source/'app'/'uploads').unlink()
                uploads=source/'app'/'uploads';uploads.mkdir()
                (uploads/'private').write_bytes(bytes(range(256)))
                captured=subject.capture_uploads(str(root/'uploads.tar'))
                self.assertEqual(captured['presence'],'present')
                self.assertFalse(captured['productionContainerStoppedByHelper'])
                s.files.restore(str(root/'uploads.tar'),captured['manifest'],str(root/'restored'))
                self.assertEqual((root/'restored'/'private').read_bytes(),bytes(range(256)))


if __name__=='__main__':unittest.main()
