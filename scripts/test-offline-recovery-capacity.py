#!/usr/bin/env python3
"""Budget boundaries and early authority denials; no production effect."""
import importlib.util
import os
from pathlib import Path
from types import SimpleNamespace
from contextlib import contextmanager
import copy
import tempfile
import unittest
from unittest.mock import Mock,patch

spec=importlib.util.spec_from_file_location('offline_capacity',Path(__file__).resolve().parent.parent/'ops/hetzner/offline-recovery-capacity.py')
c=importlib.util.module_from_spec(spec);spec.loader.exec_module(c)


class CapacityTests(unittest.TestCase):
    def test_limits_derive_from_actual_container_caps_and_keep_headroom(self):
        memory=c.physical.restore.MEMORY_LIMIT+c.canary.MEMORY+c.HEADROOM
        self.assertEqual(memory,1408*c.MiB)
        for bundle in (1,640*c.MiB,c.MAX_BUNDLE):
            disk=8*bundle+6*4096+c.DISK_HEADROOM
            inodes=c.MIN_FREE_INODES+6
            row=c.admit(memory,disk,inodes,bundle,6,4096)
            self.assertEqual(row['requiredMemoryBytes'],memory)
            for values in ((memory-1,disk,inodes,bundle,6,4096),
                           (memory,disk-1,inodes,bundle,6,4096),
                           (memory,disk,inodes-1,bundle,6,4096)):
                with self.assertRaises(ValueError):c.admit(*values)
        for size in (0,-1,True,1.0,c.MAX_BUNDLE+1):
            with self.assertRaises(ValueError):c.admit(2**40,2**40,2**30,size,6,4096)
    def test_memory_estimate_must_be_unique_and_in_kernel_units(self):
        self.assertEqual(c.available_memory('MemTotal: 2000 kB\nMemAvailable:    1536000 kB\n'),1500*c.MiB)
        for text in ('','MemAvailable: -1 kB','MemAvailable: 1 MB','MemAvailable: 1 kB\nMemAvailable: 2 kB'):
            with self.assertRaises(ValueError):c.available_memory(text)
    def fixture(self):
        status={'releaseNonce':'a'*32,'pendingStage':None,'newWritesPossible':False,
                'completedStages':['maintenance','stop-writers','stop-database']}
        fence=SimpleNamespace(journal=Mock(),expected={'db':{'containerId':'1'*64}},observe=Mock())
        fence.journal.status.return_value=status
        fence.observe.return_value={'sources':{'dockerWritersStopped':True},
            'units':{'timerStopped':True,'backupServiceInactive':True}}
        clone=SimpleNamespace(reservation_pid=os.getpid(),target=None,creation_attempted=False,start_attempted=False,
                              nonce='a'*32,source='1'*64)
        return fence,clone,status
    def test_online_or_unowned_context_rejects_before_capacity_io(self):
        changes=[lambda f,t,s:setattr(t,'reservation_pid',-1),lambda f,t,s:setattr(t,'target','2'*64),
                 lambda f,t,s:setattr(t,'source','2'*64),lambda f,t,s:s.update(pendingStage='stop-database'),
                 lambda f,t,s:s.update(newWritesPossible=True),lambda f,t,s:s.update(releaseNonce='b'*32),
                 lambda f,t,s:s.update(completedStages=['maintenance']),
                 lambda f,t,s:f.observe.return_value['sources'].update(dockerWritersStopped=False),
                 lambda f,t,s:f.observe.return_value['units'].update(timerStopped=False),
                 lambda f,t,s:f.observe.return_value['units'].update(backupServiceInactive=False)]
        for change in changes:
            fence,clone,status=self.fixture();change(fence,clone,status)
            with patch.object(c.physical,'verify_mounts') as mounts,patch.object(c.Path,'read_text') as read:
                with self.assertRaises(ValueError):c.observe(fence,clone,640*c.MiB,24000)
                mounts.assert_not_called();read.assert_not_called()
    def sample(self, fence, clone):
        with tempfile.TemporaryDirectory() as root:
            @contextmanager
            def directory(path,private):
                self.assertEqual(path,root);self.assertTrue(private)
                fd=os.open(root,os.O_RDONLY)
                try:yield fd
                finally:os.close(fd)
            disk=SimpleNamespace(f_bavail=2**30,f_frsize=4096,f_favail=2**30)
            with patch.object(c.physical,'HOST_ROOT',Path(root)), \
                 patch.object(c.physical,'verify_mounts'), \
                 patch.object(c.physical.files,'directory',directory), \
                 patch.object(c.os,'fstatvfs',return_value=disk) as stat, \
                 patch.object(c.Path,'read_text',return_value='MemAvailable: 1800000 kB'):
                result=c.observe(fence,clone,640*c.MiB,24000)
                self.assertEqual(stat.call_count,1)
                return result
    def test_observation_keeps_limits_and_explicit_scope(self):
        fence,clone,_=self.fixture()
        result=self.sample(fence,clone)
        self.assertEqual(result['requiredMemoryBytes'],1408*c.MiB)
        self.assertEqual(result['minimumInodes'],24000+c.MIN_FREE_INODES)
        self.assertFalse(result['deploymentAuthorized'])
        self.assertFalse(result['hostWorkerExclusionVerifiedByHelper'])
        self.assertFalse(result['resourcesAllocated'])
        self.assertEqual(fence.observe.call_count,2)
        self.assertEqual(fence.journal.guard.call_count,2)
    def test_changed_source_or_journal_rejects_sample(self):
        for changed_journal in (False,True):
            fence,clone,status=self.fixture()
            if changed_journal:
                second=copy.deepcopy(status);second['pendingStage']='capture'
                fence.journal.status.side_effect=[status,second]
            else:
                first=copy.deepcopy(fence.observe.return_value)
                second=copy.deepcopy(first);second['sources']['dockerWritersStopped']=False
                fence.observe.side_effect=[first,second]
            with self.assertRaises(ValueError):self.sample(fence,clone)
    def test_entry_and_allocation_bounds(self):
        for entries,block in ((5,4096),(600001,4096),(True,4096),(6,0),(6,513),(6,131072)):
            with self.assertRaises(ValueError):c.admit(2**40,2**40,2**30,100,entries,block)
        c.admit(2**40,2**40,2**30,100,600000,65536)
    def test_rejected_backup_alias_precedes_capacity_read(self):
        fence,clone,_=self.fixture()
        with patch.object(c.physical,'verify_mounts',side_effect=ValueError('synthetic alias')),patch.object(c.Path,'read_text') as read:
            with self.assertRaisesRegex(ValueError,'synthetic alias'):c.observe(fence,clone,640*c.MiB,24000)
            read.assert_not_called()


if __name__=='__main__':unittest.main()
