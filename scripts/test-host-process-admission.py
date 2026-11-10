#!/usr/bin/env python3
"""Process-class rejection controls; no process lifecycle operations."""
import copy
from contextlib import contextmanager
import os
import tempfile
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('processes',Path(__file__).resolve().parent.parent/'ops/hetzner/host-process-admission.py')
p=importlib.util.module_from_spec(spec);spec.loader.exec_module(p)

class ProcessTests(unittest.TestCase):
    def fixture(self):
        manager={'units':[],'executable':'/usr/lib/systemd/systemd','executableSha256':'a'*64,'invocation':'native-os'}
        ssh={'units':[],'executable':'/usr/sbin/sshd','executableSha256':'b'*64,'invocation':'native-os'}
        daemon={'units':['cron.service'],'executable':'/usr/sbin/cron','executableSha256':'c'*64,'invocation':'native-os'}
        policy={'schemaVersion':1,'provenance':'synthetic','classes':[manager,ssh,daemon]}
        def row(pid,parent,kind):return {'pid':pid,'parent':parent,'startTicks':pid*10,'cgroup':'/synthetic',**kind}
        rows=[row(1,0,manager),row(2,1,ssh),row(3,2,{'observer':True}),row(4,1,daemon),row(5,1,{'containerId':'d'*64})]
        return rows,policy,frozenset({'d'*64})
    def test_known_classes_and_exact_observer_ancestry_pass(self):
        rows,policy,ids=self.fixture();result=p.admit(rows,policy,ids,3)
        self.assertEqual(result,{'observer':1,'docker':1,'trustedOs':1,'observerAncestry':2})
    def test_unknown_executable_unit_or_hash_rejects(self):
        for field,value in [('executable','/usr/bin/python3'),('executableSha256','e'*64),('units',['unknown.service']),('invocation','unknown')]:
            rows,policy,ids=self.fixture();rows[3][field]=value
            with self.assertRaises(ValueError):p.admit(rows,policy,ids,3)
    def test_unrelated_ssh_and_duplicate_or_missing_observer_reject(self):
        for change in ('ssh','duplicate','missing','cycle'):
            rows,policy,ids=self.fixture()
            if change=='ssh':rows.append({**rows[1],'pid':8})
            if change=='duplicate':rows.append(dict(rows[2]))
            if change=='missing':rows.pop(2)
            if change=='cycle':rows[1]['parent']=3
            with self.assertRaises(ValueError):p.admit(rows,policy,ids,3)
    def test_other_container_rejects(self):
        rows,policy,_=self.fixture()
        with self.assertRaises(ValueError):p.admit(rows,policy,frozenset({'e'*64}),3)
    def test_observer_flag_on_another_task_rejects(self):
        rows,policy,ids=self.fixture();rows.append({'pid':9,'parent':1,'startTicks':1,'cgroup':'/','observer':True})
        with self.assertRaises(ValueError):p.admit(rows,policy,ids,3)
    def test_two_samples_must_match_identity(self):
        for changed in (False,True):
            rows,policy,ids=self.fixture();second=copy.deepcopy(rows)
            if changed:second[3]['startTicks']+=1
            with patch.object(p,'processes',side_effect=[rows,second]),patch.object(p.os,'getpid',return_value=3):
                if changed:
                    with self.assertRaises(ValueError):p.observe(policy,ids)
                else:
                    result=p.observe(policy,ids)
                    self.assertFalse(result['continuousWriterExclusion']);self.assertFalse(result['deploymentAuthorized'])
    def test_process_stat_handles_spaces_and_parentheses_without_pid_alias(self):
        fields=[b'S',b'7']+[b'0']*17+[b'12345']
        self.assertEqual(p.process_stat(b'9 (weird ) name) '+b' '.join(fields)),{'pid':9,'parent':7,'state':'S','startTicks':12345})
        with self.assertRaises(ValueError):p.process_stat(b'9 incomplete')
    def test_only_supported_unified_cgroup_shape(self):
        self.assertEqual(p.cgroup(b'0::/system.slice/cron.service\n'),'/system.slice/cron.service')
        for value in (b'1:cpu:/',b'0::/\n0::/other',b'0::/a/../b',b'0:://'):
            with self.assertRaises(ValueError):p.cgroup(value)
    @contextmanager
    def fake_proc(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary);task=root/'10';task.mkdir()
            fields=[b'S',b'1']+[b'0']*17+[b'12345']
            (task/'stat').write_bytes(b'10 (synthetic) '+b' '.join(fields))
            (task/'status').write_bytes(b'Kthread:\t0\n')
            (task/'cgroup').write_bytes(b'0::/system.slice/fixture.service\n')
            (task/'cmdline').write_bytes(b'/synthetic/executable\0')
            (task/'comm').write_bytes(b'synthetic\n')
            executable=root/'executable';executable.write_bytes(b'synthetic-executable')
            (task/'exe').symlink_to(executable)
            opened=set();actual_open=p.os.open;actual_close=p.os.close
            def open_fd(*args,**kw):
                fd=actual_open(*args,**kw);opened.add(fd);return fd
            def close_fd(fd):
                actual_close(fd);opened.discard(fd)
            with patch.object(p,'PROC',root),patch.object(p.os,'geteuid',return_value=0), \
                 patch.object(p.os,'getpid',return_value=99),patch.object(p.os,'open',side_effect=open_fd), \
                 patch.object(p.os,'close',side_effect=close_fd):
                try:yield root,task,executable
                finally:self.assertEqual(opened,set(),'collector leaked a descriptor')
    def test_collector_hashes_real_file_and_closes_descriptors(self):
        with self.fake_proc() as (_,task,executable):
            rows=p.processes()
            self.assertEqual(len(rows),1)
            self.assertEqual(rows[0]['executableSha256'],p.hashlib.sha256(executable.read_bytes()).hexdigest())
            self.assertEqual(rows[0]['startTicks'],12345)
    def test_live_task_missing_executable_rejects_and_closes_descriptors(self):
        with self.fake_proc() as (_,task,_):
            (task/'exe').unlink()
            with self.assertRaises(ValueError):p.processes()
    def test_truly_disappeared_task_is_omitted_without_descriptor_leak(self):
        with self.fake_proc() as (root,task,_):
            original=p.os.readlink
            def gone(name,**kw):
                if name=='exe':
                    target=root/'exited-task';task.rename(target);(target/'exe').unlink()
                return original(name,**kw)
            with patch.object(p.os,'readlink',side_effect=gone):self.assertEqual(p.processes(),[])
    def test_same_pid_executable_replacement_rejects_and_closes_descriptors(self):
        with self.fake_proc() as (root,task,executable):
            replacement=root/'replacement';replacement.write_bytes(b'changed-executable')
            inode=executable.stat().st_ino;actual_read=p.os.read;changed=False
            def replacing(fd,count):
                nonlocal changed
                data=actual_read(fd,count)
                if not changed and p.os.fstat(fd).st_ino==inode:
                    changed=True;(task/'exe').unlink();(task/'exe').symlink_to(replacement)
                return data
            with patch.object(p.os,'read',side_effect=replacing):
                with self.assertRaises(ValueError):p.processes()
            self.assertTrue(changed)
    def test_interpreter_and_pam_shapes_are_explicit(self):
        args=[b'/usr/bin/python3',p.UNATTENDED.encode(),b'--wait-for-signal']
        self.assertEqual(p.invocation('/usr/bin/python3.12 (deleted)',['unattended-upgrades.service'],args,b'x'), 'unattended-shutdown-waiter')
        for changed in (args[:-1],args+[b'extra'],[args[0],b'/tmp/other.py',args[2]]):
            with self.assertRaises(ValueError):p.invocation('/usr/bin/python3.12',['unattended-upgrades.service'],changed,b'x')
        self.assertEqual(p.invocation('/usr/lib/systemd/systemd-executor',['user@0.service'],[b'(sd-pam)'],b'(sd-pam)\n'),'root-user-pam-helper')
        with self.assertRaises(ValueError):p.invocation('/usr/lib/systemd/systemd-executor',['user@0.service'],[b'other'],b'other')

if __name__=='__main__':unittest.main()
