#!/usr/bin/env python3
"""Sample explicit capacity only after canonical writers are stopped.

No stop, allocation, scheduling guarantee or deployment authority. The caller
keeps the journal/shared reservation and separately admits host workers/custody.
The online rehearsal's 2GiB threshold is unchanged.
"""
import importlib.util
import os
from pathlib import Path
import re


def load(name, filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


physical=load('offline_capacity_physical','physical-postgres-recovery.py')
canary=load('offline_capacity_canary','isolated-application-canary.py')
MiB=1024**2
HEADROOM=512*MiB
DISK_HEADROOM=2*1024**3
MIN_FREE_INODES=65536
MAX_BUNDLE=2*1024**3


def require(value):
    if not value:raise ValueError('Offline recovery capacity rejected')


def available_memory(text):
    require(isinstance(text,str) and 0<len(text)<=65536)
    rows=[line for line in text.splitlines() if line.startswith('MemAvailable:')]
    require(len(rows)==1)
    match=re.fullmatch(r'MemAvailable:\s+([0-9]+) kB',rows[0])
    require(match is not None)
    return int(match.group(1))*1024


def admit(memory, disk, inodes, bundle_bytes, bundle_entries, block_size):
    require(all(type(value) is int and value>=0 for value in (memory,disk,inodes,bundle_bytes,bundle_entries,block_size)))
    require(0<bundle_bytes<=MAX_BUNDLE)
    require(6 <= bundle_entries <= 6*physical.files.MAX_ENTRIES)
    require(512 <= block_size <= 65536 and block_size & (block_size-1) == 0)
    # Seven retained forms: capture components, outer archive, ciphertext,
    # retrieved ciphertext, decrypted archive, replayed components, replayed trees.
    # Eighth equivalent covers envelope/metadata overhead; add one full block per
    # replayed entry for allocation rounding, then separate fixed growth headroom.
    # Renamed DB/content trees do not add copies. This is policy, not an ENOSPC proof.
    required_memory=physical.restore.MEMORY_LIMIT+canary.MEMORY+HEADROOM
    required_disk=8*bundle_bytes+bundle_entries*block_size+DISK_HEADROOM
    required_inodes=bundle_entries+MIN_FREE_INODES
    require(memory>=required_memory and disk>=required_disk and inodes>=required_inodes)
    return {'availableMemoryBytes':memory,'requiredMemoryBytes':required_memory,
            'databaseLimitBytes':physical.restore.MEMORY_LIMIT,'applicationLimitBytes':canary.MEMORY,
            'hostHeadroomBytes':HEADROOM,'availableDiskBytes':disk,'requiredDiskBytes':required_disk,
            'availableInodes':inodes,'minimumInodes':required_inodes,'bundleBytes':bundle_bytes,
            'bundleEntries':bundle_entries,'filesystemBlockBytes':block_size,
            'scope':'Sampled offline capacity policy, not memory allocation or a no-OOM/no-ENOSPC guarantee'}


def observe(fence, clone, bundle_bytes, bundle_entries):
    """Before any disposable container starts; do not admit online capacity here."""
    require(clone.reservation_pid==os.getpid() and clone.target is None
            and not clone.creation_attempted and not clone.start_attempted)
    fence.journal.guard()
    status=fence.journal.status()
    require(status['releaseNonce']==clone.nonce and status['pendingStage'] is None
            and status['newWritesPossible'] is False
            and status['completedStages'][:3]==['maintenance','stop-writers','stop-database'])
    require(clone.source==fence.expected['db']['containerId'])
    first=fence.observe()
    require(first['sources']['dockerWritersStopped'] is True
            and first['units']['timerStopped'] is True
            and first['units']['backupServiceInactive'] is True)
    physical.verify_mounts()
    with physical.files.directory(str(physical.HOST_ROOT),private=True) as fd:
        disk=os.fstatvfs(fd)
        result=admit(available_memory(Path('/proc/meminfo').read_text()),
                     disk.f_bavail*disk.f_frsize,disk.f_favail,bundle_bytes,bundle_entries,disk.f_frsize)
    # Source resampling cannot make kernel estimates atomic or reserve OS memory.
    require(fence.observe()==first)
    fence.journal.guard()
    require(fence.journal.status()==status)
    return {**result,'releaseNonce':clone.nonce,'canonicalDockerWritersStopped':True,
            'registeredBackupTimerStopped':True,'resourcesAllocated':False,
            'hostWorkerExclusionVerifiedByHelper':False,'deploymentAuthorized':False}
