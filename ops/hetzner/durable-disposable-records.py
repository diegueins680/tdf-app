#!/usr/bin/env python3
"""Durable typed creation records; no Docker command or deletion is executed.

Publication failure closes the writer. It must occur before external creation.
Recovery readers bind every retained record to the original prepared admission;
these records alone confer no removal authority or fresh-boot evidence.
"""
import importlib.util
import json
import os
from pathlib import Path

_spec=importlib.util.spec_from_file_location('durable_creation_spec',Path(__file__).with_name('disposable-creation-spec.py'))
s=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(s)
o=s.original
require,canonical,sha=s.require,s.canonical,s.sha
DIRECTORY='disposable-creation'
MAXIMUM=32768


def binding(admission):
    require(type(admission.get('schemaVersion')) is int and admission['schemaVersion']==1)
    require(o.abort.j.hash_value(admission['releaseNonce'],32) and o.abort.j.hash_value(admission['planHash']))
    expected=admission['originalDeployment']['expected'];require(set(expected)=={'db','api','edge'})
    originals={role:row['containerId'] for role,row in expected.items()};s.validate_originals(originals)
    return {'releaseNonce':admission['releaseNonce'],'planHash':admission['planHash'],
            'originalAdmissionSha256':sha(canonical(admission)),'originalContainerIds':originals}


def read_directory(prepared,admission):
    expected=binding(admission)
    require(o.read_prepared(prepared,expected['releaseNonce'],expected['planHash'])==admission)
    return Path(prepared)/DIRECTORY


def validate_record(row,admission,role):
    require(isinstance(row,dict) and set(row)=={'schemaVersion','binding','specification','physicalDescriptorSha256'}
            and type(row['schemaVersion']) is int and row['schemaVersion']==1
            and row['binding']==binding(admission) and row['specification']['role']==role
            and row['specification']['nonce']==admission['releaseNonce'])
    s.reconstruct(row['specification'],row['binding']['originalContainerIds'])
    if role=='physical-database':require(row['physicalDescriptorSha256'] is None)
    else:require(o.abort.j.hash_value(row['physicalDescriptorSha256']))
    return row


def read_records(prepared,admission):
    directory=read_directory(prepared,admission)
    records={}
    with o.abort.j.files.directory(str(directory),private=True) as parent:
        names=set(os.listdir(parent));require(names<={role+'.json' for role in s.ROLES})
        for role in s.ROLES:
            name=role+'.json'
            if name not in names:continue
            raw,_=o.abort.read_file(parent,name,maximum=MAXIMUM)
            value=json.loads(raw);require(canonical(value)==raw)
            records[role]=validate_record(value,admission,role)
        require(set(os.listdir(parent))==names)
    if 'application-canary' in records:
        require('physical-database' in records)
        application=records['application-canary'];database=records['physical-database']
        require(application['physicalDescriptorSha256']==sha(canonical(database)))
        for key in ('nonce','sourceContainer','directory'):
            require(application['specification'][key]==database['specification'][key])
    return records


class Writer:
    """One-process pre-create publisher under caller-owned release/restore locks.

    guard must revalidate those locks, the exact release journal pending stage,
    current reservation and prepared original admission on every invocation.
    It is called before/after publication and before the caller may dispatch.
    """
    def __init__(self,prepared,admission,guard):
        require(callable(guard));guard()
        self.prepared=Path(prepared);self.admission=json.loads(canonical(admission));self.guard_callback=guard
        self.owner=os.getpid();self.closed=False
        self.directory=read_directory(self.prepared,self.admission)
        with o.abort.j.files.directory(str(self.prepared),private=True) as parent:
            # Exclusive allocation: do not adopt records from a previous writer.
            os.mkdir(DIRECTORY,0o700,dir_fd=parent);os.fsync(parent)
        self.identity=o.directory_identity(self.directory)
        self.guard()

    def guard(self):
        require(not self.closed and os.getpid()==self.owner)
        self.guard_callback()
        require(read_directory(self.prepared,self.admission)==self.directory
                and o.directory_identity(self.directory)==self.identity)

    def publish(self,role,obj):
        self.guard();require(role in s.ROLES)
        existing=read_records(self.prepared,self.admission)
        require(role not in existing and (not existing if role=='physical-database' else set(existing)=={'physical-database'}))
        spec=s.snapshot(role,obj,binding(self.admission)['originalContainerIds'])
        value={'schemaVersion':1,'binding':binding(self.admission),'specification':spec,
               'physicalDescriptorSha256':None if role=='physical-database' else sha(canonical(existing['physical-database']))}
        validate_record(value,self.admission,role);require(len(canonical(value))<=MAXIMUM)
        if role=='application-canary':
            for key in ('nonce','sourceContainer','directory'):
                require(spec[key]==existing['physical-database']['specification'][key])
        self.guard()
        try:
            with o.abort.j.files.directory(str(self.directory),private=True) as parent:
                o.abort.publish(parent,role+'.json',value)
            self.guard()
            require(read_records(self.prepared,self.admission)=={**existing,role:value})
        except BaseException:
            self.closed=True
            raise
        return {'role':role,'descriptorSha256':sha(canonical(value)),'publishedBeforeCreation':True}
