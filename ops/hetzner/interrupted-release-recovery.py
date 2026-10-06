#!/usr/bin/env python3
"""Durable abort admission and a fresh-boot barrier, not a restart executor.

No reboot, service start, old-database restore, journal repair or reset is exposed.
The coordinator must independently persist the original full admission before
shutdown, authenticate this host, and later re-admit the original deployment.
"""
from contextlib import contextmanager
import fcntl
import importlib.util
import json
import os
from pathlib import Path
import re
import stat

_spec = importlib.util.spec_from_file_location('abort_release_journal', Path(__file__).with_name('release-journal.py'))
j = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(j)
require, canonical, sha = j.require, j.canonical, j.sha
MAX_ADMISSION = 1024 * 1024
PREWRITE_STAGES = ('maintenance', 'stop-writers', 'stop-database', 'capture',
                   'encrypt', 'retrieve-off-host', 'restore-isolate')


def boot_identity():
    """Local trusted Linux kernel/host inputs; not remote host authentication."""
    require(os.name == 'posix' and Path('/proc/self').exists())
    result = {'machineId': Path('/etc/machine-id').read_text().strip(),
              'bootId': Path('/proc/sys/kernel/random/boot_id').read_text().strip()}
    validate_host(result)
    return result


def validate_host(value):
    require(isinstance(value, dict) and set(value) == {'machineId', 'bootId'}
            and isinstance(value['machineId'], str) and re.fullmatch('[a-f0-9]{32}', value['machineId'])
            and isinstance(value['bootId'], str)
            and re.fullmatch('[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}', value['bootId']))


def read_file(parent, name, *, maximum, links=(1,)):
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
    try:
        before = os.fstat(fd)
        require(stat.S_ISREG(before.st_mode) and before.st_uid == os.geteuid()
                and stat.S_IMODE(before.st_mode) == 0o600 and before.st_nlink in links
                and 0 < before.st_size <= maximum)
        raw = os.read(fd, maximum + 1)
        require(len(raw) == before.st_size
                and j.files.identity(before) == j.files.identity(os.fstat(fd))
                and j.files.identity(before) == j.files.identity(os.stat(name, dir_fd=parent, follow_symlinks=False)))
        return raw, before
    finally: os.close(fd)


def frozen_prefix(parent):
    """Preserve recognized pre-write publication states without completing them."""
    require(j.STAGES[:len(PREWRITE_STAGES)] == PREWRITE_STAGES
            and j.STAGES[len(PREWRITE_STAGES)] == 'start-database')
    names = set(os.listdir(parent)) - {'release.lock', 'abort'}
    require(names and len(names) <= 16)
    by_index = {}
    for name in names:
        match = re.fullmatch(r'(\d{3})\.json(\.pending)?', name)
        # Reject any possible production-write intent before parsing its bytes.
        require(match is not None and int(match[1]) < 15)
        by_index.setdefault(int(match[1]), []).append(name)
    require(set(by_index) == set(range(len(by_index))))
    records, snapshot, previous = [], [], None
    for index, entries in sorted(by_index.items()):
        require(len(entries) <= 2)
        data = {name: read_file(parent, name, maximum=j.MAX_RECORD, links=(1, 2)) for name in entries}
        final = index == len(by_index)-1
        pending = '%03d.json.pending' % index
        if pending in entries:
            require(final and index > 0)
        else: require(entries == ['%03d.json' % index])
        if len(entries) == 2:
            left, right = data.values()
            require(left[0] == right[0] and left[1].st_nlink == right[1].st_nlink == 2
                    and (left[1].st_dev, left[1].st_ino) == (right[1].st_dev, right[1].st_ino))
        else: require(next(iter(data.values()))[1].st_nlink == 1)
        raw = next(iter(data.values()))[0]
        row = json.loads(raw)
        require(isinstance(row, dict) and canonical(row) == raw
                and set(row) == {'schemaVersion','releaseNonce','planHash','sequence','previousHash','event'}
                and type(row['schemaVersion']) is int and row['schemaVersion'] == 1
                and type(row['sequence']) is int and row['sequence'] == index
                and j.hash_value(row['releaseNonce'],32) and j.hash_value(row['planHash'])
                and row['previousHash'] == previous)
        event = row['event']; require(isinstance(event, dict))
        if index == 0:
            require(set(event) == {'kind','plan'} and event['kind'] == 'plan')
            j.validate_plan(event['plan']); require(row['planHash'] == sha(canonical(event['plan'])))
        else:
            require(row['releaseNonce'] == records[0]['releaseNonce'] and row['planHash'] == records[0]['planHash'])
            stage = j.STAGES[(index-1)//2]
            require(event.get('stage') == stage)
            if index % 2:
                require(set(event) == {'kind','stage','targetsHash','operationId'} and event['kind'] == 'intent'
                        and j.hash_value(event['targetsHash'])
                        and event['operationId'] == sha(canonical({'releaseNonce':row['releaseNonce'],
                            'planHash':row['planHash'],'stage':stage,'targetsHash':event['targetsHash']})))
            else:
                require(set(event) == {'kind','stage','operationId','evidenceHash'} and event['kind'] == 'observed'
                        and event['operationId'] == records[-1]['event']['operationId'] and j.hash_value(event['evidenceHash']))
        records.append(row); previous = sha(raw)
        for name, (content, info) in sorted(data.items()):
            snapshot.append({'name':name,'sha256':sha(content),'bytes':len(content),
                'inode':info.st_ino,'device':info.st_dev,'links':info.st_nlink})
    return {'releaseNonce':records[0]['releaseNonce'],'planHash':records[0]['planHash'],
            'plan':records[0]['event']['plan'],'files':snapshot}


def publish(parent, name, value):
    """Exclusive durable publication; leftovers are retained and never repaired."""
    raw = canonical(value); require(len(raw) <= MAX_ADMISSION)
    temporary = name+'.pending'
    fd = os.open(temporary, os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW, 0o600, dir_fd=parent)
    try:
        j.private_file(fd)
        with os.fdopen(fd,'wb',closefd=False) as out:
            out.write(raw); out.flush(); os.fsync(fd)
        os.link(temporary,name,src_dir_fd=parent,dst_dir_fd=parent,follow_symlinks=False)
        os.fsync(parent); os.unlink(temporary,dir_fd=parent); os.fsync(parent)
    finally: os.close(fd)


class Abort:
    def __init__(self,parent,lock):
        self.parent,self.lock,self.owner = parent,lock,os.getpid()
        self.closed = False

    def guard(self):
        require(not self.closed and self.owner == os.getpid())
        held = j.private_file(self.lock)
        named = os.stat('release.lock',dir_fd=self.parent,follow_symlinks=False)
        require((held.st_dev,held.st_ino) == (named.st_dev,named.st_ino))

    def latch(self, admission, expected_admission_hash):
        """The supplied private admission must have been durably saved pre-shutdown.

        This boundary binds that supplied document; it does not invent or prove
        the caller's original runtime/storage admission or its earlier durability.
        """
        self.guard(); require(j.hash_value(expected_admission_hash))
        require(isinstance(admission,dict) and sha(canonical(admission)) == expected_admission_hash)
        require(set(admission) == {'schemaVersion','releaseNonce','planHash','host','originalDeployment'})
        require(type(admission['schemaVersion']) is int and admission['schemaVersion'] == 1)
        validate_host(admission['host'])
        require(isinstance(admission['originalDeployment'],dict) and admission['originalDeployment'])
        require(len(canonical(admission)) <= MAX_ADMISSION//2)
        original = frozen_prefix(self.parent)
        # No unrestricted reboot/recovery is exposed for exceptional legacy plans.
        # Future integration must establish the live restricted boundary first.
        require('legacyStopPolicyHash' not in original['plan'])
        require(admission['releaseNonce'] == original['releaseNonce'] and admission['planHash'] == original['planHash'])
        os.mkdir('abort',0o700,dir_fd=self.parent)
        # Directory existence itself is the irreversible normal-release latch.
        os.fsync(self.parent)
        fd = os.open('abort',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=self.parent)
        try:
            publish(fd,'latch.json',{'schemaVersion':1,'admission':admission,
                'admissionHash':expected_admission_hash,'original':original,'originalHash':sha(canonical(original))})
        finally: os.close(fd)
        return self.status()

    def _read(self):
        self.guard()
        fd = os.open('abort',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=self.parent)
        try:
            info=os.fstat(fd);require(info.st_uid==os.geteuid() and stat.S_IMODE(info.st_mode)==0o700)
            names=set(os.listdir(fd))
            if 'recovery' in names:
                recovery=os.open('recovery',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=fd)
                try:
                    info=os.fstat(recovery);require(info.st_uid==os.geteuid() and stat.S_IMODE(info.st_mode)==0o700)
                finally:os.close(recovery)
                names.remove('recovery')
            require(names in ({'latch.json'},{'latch.json','reboot-intent.json'}))
            raw,_=read_file(fd,'latch.json',maximum=MAX_ADMISSION)
            latch=json.loads(raw)
            require(canonical(latch)==raw and set(latch)=={'schemaVersion','admission','admissionHash','original','originalHash'}
                    and type(latch['schemaVersion']) is int and latch['schemaVersion']==1
                    and sha(canonical(latch['admission']))==latch['admissionHash']
                    and sha(canonical(latch['original']))==latch['originalHash']
                    and frozen_prefix(self.parent)==latch['original'])
            intent=None
            if 'reboot-intent.json' in names:
                raw,_=read_file(fd,'reboot-intent.json',maximum=j.MAX_RECORD);intent=json.loads(raw)
                require(canonical(intent)==raw and set(intent)=={'schemaVersion','kind','latchHash','host','newWritesPossible'}
                        and type(intent['schemaVersion']) is int and intent['schemaVersion']==1
                        and intent['kind']=='abort-reboot' and intent['latchHash']==sha(canonical(latch))
                        and intent['newWritesPossible'] is True)
                validate_host(intent['host'])
                require(intent['host']['machineId']==latch['admission']['host']['machineId'])
            return latch,intent
        finally:os.close(fd)

    def status(self):
        latch,intent=self._read()
        return {'releaseNonce':latch['original']['releaseNonce'],'releaseContinuationAllowed':False,
                'rebootIntentRecorded':intent is not None,'newWritesPossible':intent is not None,
                'originalDeploymentRecovered':False}

    def request_reboot(self,effect):
        """Persist write-possible intent before invoking the caller's reboot effect.

        Never retry an existing intent. A fresh boot can instead be observed.
        """
        latch,intent=self._read();require(intent is None)
        host=boot_identity();require(host['machineId']==latch['admission']['host']['machineId'])
        fd=os.open('abort',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=self.parent)
        try:publish(fd,'reboot-intent.json',{'schemaVersion':1,'kind':'abort-reboot',
            'latchHash':sha(canonical(latch)),'host':host,'newWritesPossible':True})
        finally:os.close(fd)
        self._read()
        # No completion receipt: a returning callback cannot prove a new boot.
        effect()

    def observe_new_boot(self):
        latch,intent=self._read();require(intent is not None)
        host=boot_identity()
        require(host['machineId']==intent['host']['machineId'] and host['bootId']!=intent['host']['bootId'])
        return {**self.status(),'host':host,'oldBootTasksExcluded':True,
                'runtimeReadmissionVerified':False,'databaseCleanShutdownVerified':False,
                'continuousMaintenanceVerified':False}


@contextmanager
def open_abort(path):
    """Same permanent lock as normal release; never an independent abort lock."""
    with j.files.directory(path,private=True) as parent:
        lock=os.open('release.lock',os.O_RDWR|os.O_NOFOLLOW,dir_fd=parent)
        current=None
        try:
            j.private_file(lock);fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
            os.fsync(parent);current=Abort(parent,lock);current.guard();yield current
        finally:
            if current is not None:current.closed=True
            os.close(lock)
