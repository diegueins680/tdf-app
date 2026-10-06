#!/usr/bin/env python3
"""Abort-only original PostgreSQL restart. Never restores or initializes a cluster.

Library boundary, not a deployment entrypoint. The service journal must be in a
fresh admitted recovery epoch and its preceding disposable-cleanup stage complete.
The caller holds the common restore reservation throughout this object's lifetime.
"""
import os
from contextlib import contextmanager
from pathlib import Path
import stat
import time
import importlib.util

_spec=importlib.util.spec_from_file_location('database_original',Path(__file__).with_name('original-deployment-admission.py'))
o=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(o)
require,canonical,sha=o.require,o.canonical,o.sha
restore=o.load('abort_restore','rehearse-postgres-restore.py')


class Reservation:
    def __init__(self,directory,descriptor):
        self.directory=directory;self.descriptor=descriptor;self.owner=os.getpid();self.closed=False
        self.identity=o.directory_identity(directory)
        self.guard()

    def guard(self):
        require(not self.closed and os.getpid()==self.owner and o.directory_identity(self.directory)==self.identity)
        held=os.fstat(self.descriptor);named=os.stat(self.directory/'restore-rehearsal.lock',follow_symlinks=False)
        require(stat.S_ISREG(held.st_mode) and held.st_uid==os.geteuid() and held.st_nlink==1
                and stat.S_IMODE(held.st_mode)==0o600 and (held.st_dev,held.st_ino)==(named.st_dev,named.st_ino))


@contextmanager
def recovery_reservation():
    """Same permanent lock as all canonical physical/logical restore rehearsals."""
    directory=Path('/opt/tdf/backups')
    with o.abort.j.files.directory(str(directory),private=True),restore.rehearsal_lock(directory) as descriptor:
        reservation=Reservation(directory,descriptor)
        try:yield reservation
        finally:reservation.closed=True



def cluster_present(root):
    """Deny entrypoint initdb before start; not a control-file checksum proof."""
    with o.abort.j.files.directory(str(root)) as fd:
        info=os.fstat(fd);require(info.st_uid==999 and info.st_gid==999 and stat.S_IMODE(info.st_mode) in (0o700,0o750))
        for name in ('base','global','pg_wal','pg_tblspc'):
            child=os.open(name,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=fd)
            try:
                if name=='pg_tblspc':require(os.listdir(child)==[])
            finally:os.close(child)
        require(not any(name in os.listdir(fd) for name in ('backup_label','tablespace_map','recovery.signal','standby.signal')))
        for parts,expected_size in ((('PG_VERSION',),3),(('global','pg_control'),8192)):
            parent=os.dup(fd)
            try:
                for name in parts[:-1]:
                    child=os.open(name,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=parent)
                    os.close(parent);parent=child
                f=os.open(parts[-1],os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
                try:
                    before=os.fstat(f)
                    require(stat.S_ISREG(before.st_mode) and before.st_nlink==1 and before.st_uid==999
                            and before.st_gid==999 and before.st_size==expected_size)
                    if parts==('PG_VERSION',):require(os.read(f,4)==b'17\n')
                    # pg_control is mutable during crash recovery. Its existence
                    # and structure prevent empty-cluster admission; SQL below
                    # independently verifies the live system identifier.
                    after=os.stat(parts[-1],dir_fd=parent,follow_symlinks=False)
                    require((before.st_dev,before.st_ino)==(after.st_dev,after.st_ino))
                finally:os.close(f)
            finally:os.close(parent)
    return {'majorVersion':17,'existingClusterRequired':True,'controlChecksumVerified':False}


def observe(saved):
    capture=o.sources.inspector.capture;docker=o.sources.inspector.DOCKER
    ids=capture(docker+['ps','--all','--quiet','--no-trunc']).split()
    require(0<len(ids)<=128 and len(ids)==len(set(ids)) and all(o.abort.j.hash_value(cid) for cid in ids))
    import json
    rows=json.loads(capture(docker+['inspect',*ids]))
    volumes=json.loads(capture(docker+['volume','inspect',*o.sources.VOLUMES.values()]))
    require(len(volumes)==len(o.sources.VOLUMES))
    volumes={row['Name']:row for row in volumes}
    admitted=o.sources.admit_abort(rows,volumes,saved['expected'])
    require(admitted['runtimeConfigurationSha256']==saved['runtimeConfigurationSha256'] and volumes==saved['volumes'])
    require({name:o.directory_identity(path) for name,path in admitted['roots'].items()}==saved['roots'])
    require(o.configuration_files(o.sources.DIRECTORY)==saved['configurationFiles'])
    for value in saved['bindDirectories'].values():require(o.directory_identity(value['path'])==value)
    selected={service:next(row for row in rows if row['Id']==target['containerId']) for service,target in saved['expected'].items()}
    if selected['api']['State']['Running']:
        require(o.bind_directory_identities(selected['api'])==saved['bindDirectories'])
    # The enabled timer may have resumed on reboot. Active backup work or changed
    # unit definitions reject this attempt; no stop is performed during recovery.
    props=o.fence.parse_properties(o.fence.execute(['systemctl','show',o.fence.TIMER,
                                  '--property='+','.join(o.fence.PROPERTIES)]))
    require(props['ActiveState'] in ('active','inactive'))
    units=o.fence.observe_units(saved['units']['fileHashes'],timer_stopped=props['ActiveState']=='inactive')
    cluster=cluster_present(admitted['roots']['database'])
    require(sorted(capture(docker+['ps','--all','--quiet','--no-trunc']).split())==sorted(ids))
    return {'sources':admitted,'units':units,'cluster':cluster,
            'running':{service:row['State']['Running'] for service,row in selected.items()}}


class OriginalDatabase:
    def __init__(self,service_journal,prepared_directory,reservation):
        self.journal=service_journal;self.reservation=reservation;self.owner=os.getpid()
        require(isinstance(reservation,Reservation));reservation.guard()
        latch,_=service_journal.guard();admission=latch['admission']
        require(o.read_prepared(prepared_directory,admission['releaseNonce'],admission['planHash'])==admission)
        self.saved=admission['originalDeployment'];self.targets_hash=sha(canonical(self.saved))

    def guard(self):
        require(os.getpid()==self.owner)
        self.reservation.guard();self.journal.current()

    def recover(self):
        self.guard()
        def effect(context):
            self.guard();before=observe(self.saved);self.guard()
            target=self.saved['expected']['db']['containerId']
            submitted=not before['running']['db']
            if submitted:
                output=o.fence.execute(o.sources.inspector.DOCKER+['start',target]).strip()
                require(output==target)
            # Only read-only readiness probes may repeat. A lost start response
            # exits before this loop and leaves the durable intent unresolved.
            deadline=time.monotonic()+60
            while True:
                self.guard();after=observe(self.saved);require(after['running']['db'])
                try:database=o.database_identity(target)
                except ValueError:
                    require(time.monotonic()<deadline);time.sleep(0.25);continue
                require(database==self.saved['database']);break
            self.guard();require(observe(self.saved)['running']['db'])
            evidence={'runtimeHash':self.saved['runtimeConfigurationSha256'],
                      'databaseHash':sha(canonical(database)),'startSubmitted':submitted,
                      'existingDataRetained':True,'databaseCleanShutdownVerified':False,
                      'continuousWriterExclusionVerified':False,'originalDeploymentRecovered':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('recover-db',self.targets_hash,effect)
