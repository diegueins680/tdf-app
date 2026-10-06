#!/usr/bin/env python3
"""Fixed creation specifications for future abort-only disposable cleanup.

This module never creates, starts or removes containers. It reconstructs the same
existing admission predicates from bounded typed data, never stored commands.
The caller must additionally bind descriptors to a durable release reservation,
original admission, fresh-boot journal and complete current Docker inventory.
"""
import importlib.util
import json
from pathlib import Path
import re


POLICY_NAMES=('disposable-creation-spec.py','physical-postgres-recovery.py',
              'isolated-application-canary.py','rehearse-postgres-restore.py',
              'original-deployment-admission.py','recovery-files.py','release-journal.py')
# Capture before importing admission modules. Direct imports below compile these
# exact bytes rather than consulting a timestamp-based bytecode cache.
POLICY_BYTES={name:Path(__file__).with_name(name).read_bytes() for name in POLICY_NAMES}

def load(name, filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec)
    exec(compile(POLICY_BYTES[filename],str(Path(__file__).with_name(filename)),'exec'),module.__dict__)
    return module


physical=load('creation_physical','physical-postgres-recovery.py')
canary=load('creation_canary','isolated-application-canary.py')
original=load('creation_original','original-deployment-admission.py')
require,canonical,sha=original.require,original.canonical,original.sha
ROLES=('physical-database','application-canary')
COMMON={'schemaVersion','role','nonce','sourceContainer','image','imageId','directory','name',
        'commandSha256','directoryIdentities','admissionPolicySha256'}


def policy_digest():
    require(all(Path(__file__).with_name(name).read_bytes()==content for name,content in POLICY_BYTES.items()))
    return sha(canonical({name:sha(content) for name,content in POLICY_BYTES.items()}))


# Fail initialization if the policy changed while its dependent modules loaded.
INITIAL_POLICY_DIGEST=policy_digest()


def regional_configuration(value):
    require(isinstance(value,dict) and set(value)=={'SUPPORTED_LOCALES','DEFAULT_LOCALE',
                                                 'SUPPORTED_CURRENCIES','DEFAULT_CURRENCY'})
    rows={}
    for name,supported,default in (('locales','SUPPORTED_LOCALES','DEFAULT_LOCALE'),
                                    ('currencies','SUPPORTED_CURRENCIES','DEFAULT_CURRENCY')):
        require(all(isinstance(value[key],str) and 0<len(value[key])<=16384 for key in (supported,default)))
        codes=value[supported].split(',')
        rows[name]=[{'code':code,'default':code==value[default]} for code in codes]
    normalized=canary.regional_environment(rows)
    require(normalized==value)
    return normalized


def directory_paths(role,directory):
    names=('physical-data','physical-config') if role=='physical-database' else ('canary-assets','canary-uploads')
    return [Path(directory),*(Path(directory)/name for name in names)]


def validate_originals(original_ids):
    require(isinstance(original_ids,dict) and set(original_ids)=={'db','api','edge'}
            and all(isinstance(cid,str) and re.fullmatch('[a-f0-9]{64}',cid) for cid in original_ids.values())
            and len(set(original_ids.values()))==3)


def reconstruct(value,original_ids):
    """Pure reconstruction: no Docker call or dependency existence assumption."""
    validate_originals(original_ids)
    require(isinstance(value,dict) and value.get('role') in ROLES)
    role=value['role']
    extra={'systemIdentifier'} if role=='physical-database' else {'dependentDatabase','revision','regionalConfiguration'}
    require(set(value)==COMMON|extra and type(value['schemaVersion']) is int and value['schemaVersion']==1)
    require(isinstance(value['nonce'],str) and re.fullmatch('[a-f0-9]{32}',value['nonce']))
    require(value['sourceContainer']==original_ids['db'] and value['admissionPolicySha256']==policy_digest())
    require(value['directory']=='/opt/tdf/backups/rehearsal-'+value['nonce'])
    if role=='physical-database':
        require(isinstance(value['image'],str) and re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}',value['image']))
        obj=physical.PhysicalClone(value['sourceContainer'],value['image'],value['imageId'],
                                  value['nonce'],value['directory'],value['systemIdentifier'])
        # Used only by the command constructor; no manifest verification or start.
        obj.prepared_manifest={}
        command=obj.create_command()
        expected_name='tdf-audit-restore-'+value['nonce']
    else:
        dependency=value['dependentDatabase']
        require(isinstance(dependency,str) and re.fullmatch('[a-f0-9]{64}',dependency)
                and dependency not in original_ids.values())
        # Canary.admit needs identity fields only. A stopped or already absent DB
        # must not be started merely to validate and remove a surviving canary.
        from types import SimpleNamespace
        database=SimpleNamespace(source=value['sourceContainer'],target=dependency,nonce=value['nonce'])
        obj=canary.Canary(physical.restore,database,value['directory'],value['image'],value['revision'])
        require(isinstance(value['imageId'],str) and re.fullmatch('sha256:[a-f0-9]{64}',value['imageId']))
        obj.image_id=value['imageId']
        obj.regional_configuration=regional_configuration(value['regionalConfiguration'])
        environment={**canary.ENVIRONMENT,**obj.regional_configuration}
        obj.runtime_command=['env','-i',*[key+'='+text for key,text in environment.items()],'/app/production-entrypoint.sh']
        command=physical.restore.DOCKER+obj.command()
        expected_name=obj.name
    require(value['name']==expected_name and value['commandSha256']==sha(canonical(command)))
    identities=value['directoryIdentities']
    require(isinstance(identities,list) and len(identities)==3)
    for path,identity in zip(directory_paths(role,value['directory']),identities):
        require(isinstance(identity,dict) and set(identity)=={'path','device','inode','uid','gid','mode'}
                and identity['path']==str(path)
                and all(type(identity[key]) is int and identity[key]>=0 for key in ('device','inode','uid','gid','mode')))
    return obj


def snapshot(role,obj,original_ids):
    validate_originals(original_ids);require(role in ROLES)
    require(obj.target is None and not obj.creation_attempted)
    value={'schemaVersion':1,'role':role,'admissionPolicySha256':policy_digest(),'nonce':obj.nonce,'sourceContainer':obj.source if role=='physical-database' else obj.database.source,
           'image':obj.image,'imageId':obj.image_id,'directory':str(obj.directory),
           'name':'tdf-audit-restore-'+obj.nonce if role=='physical-database' else obj.name,
           'commandSha256':sha(canonical(obj.create_command() if role=='physical-database' else physical.restore.DOCKER+obj.command())),
           'directoryIdentities':[original.directory_identity(path) for path in directory_paths(role,obj.directory)]}
    if role=='physical-database':value['systemIdentifier']=obj.system_id
    else:value.update(dependentDatabase=obj.database.target,revision=obj.revision,regionalConfiguration=obj.regional_configuration)
    reconstruct(value,original_ids)
    return json.loads(canonical(value))


def admit(value,container,original_ids):
    obj=reconstruct(value,original_ids)
    require(isinstance(container,dict) and container.get('Id') not in original_ids.values()
            and container.get('Name')=='/'+value['name'])
    for saved in value['directoryIdentities']:
        require(original.directory_identity(saved['path'])==saved)
    obj.admit(container)
    return obj.target
