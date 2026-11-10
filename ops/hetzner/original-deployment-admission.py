#!/usr/bin/env python3
"""Privately retain actual original deployment identity before shutdown.

Read-only Docker/SQL sampling plus exclusive local evidence publication. No stop,
reboot, service recovery or deployment effect. Samples do not fence other writers.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import stat


def load(name,file):
    s=importlib.util.spec_from_file_location(name,Path(__file__).with_name(file))
    m=importlib.util.module_from_spec(s);s.loader.exec_module(m);return m


abort=load('original_abort','interrupted-release-recovery.py')
fence=load('original_fence','production-writer-fence.py')
sources=fence.sources
require,canonical,sha=abort.require,abort.canonical,abort.sha
NAME='original-deployment-admission.json'
RECEIPT='original-deployment-admission-receipt.json'
SQL="""BEGIN READ ONLY;
SELECT json_build_object('database',current_database(),
 'readOnly',current_setting('transaction_read_only'),
 'localConnection',inet_server_addr() IS NULL,
 'systemIdentifier',(SELECT system_identifier::text FROM pg_control_system()),
 'migrations',(SELECT coalesce(json_agg(x ORDER BY migration_id),'[]'::json) FROM
   (SELECT migration_id,checksum,source_commit FROM public.tdf_schema_migration) x));
ROLLBACK;
"""


def directory_identity(path):
    with abort.j.files.directory(str(path)) as fd:
        info=os.fstat(fd)
        return {'path':str(path),'device':info.st_dev,'inode':info.st_ino,
                'uid':info.st_uid,'gid':info.st_gid,'mode':stat.S_IMODE(info.st_mode)}


def bind_directory_identities(container):
    """Compare actual live API mounts with their no-follow host source paths."""
    cid=container['Id'];pid=container['State']['Pid']
    require(abort.j.hash_value(cid) and type(pid) is int and pid>0 and hasattr(os,'pidfd_open'))
    proc=os.open('/proc/'+str(pid),os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW)
    pidfd=root=None
    try:
        pidfd=os.pidfd_open(pid)
        groupfd=os.open('cgroup',os.O_RDONLY|os.O_NOFOLLOW,dir_fd=proc)
        try:groups=os.read(groupfd,65537).decode().splitlines()
        finally:os.close(groupfd)
        require(any(line.split(':',2)[-1].endswith('/docker-'+cid+'.scope')
                    or line.split(':',2)[-1].endswith('/docker/'+cid) for line in groups))
        # Deliberate kernel proc magic link through an already-held task directory.
        root=os.open('root',os.O_RDONLY|os.O_DIRECTORY,dir_fd=proc)
        result={}
        for mount in container['Mounts']:
            if mount['Destination'] not in ('/data/assets','/app/uploads'):continue
            require(mount['Type']=='bind')
            host=directory_identity(mount['Source'])
            fd=os.dup(root)
            try:
                for part in Path(mount['Destination']).parts[1:]:
                    child=os.open(part,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=fd)
                    os.close(fd);fd=child
                info=os.fstat(fd)
                require((info.st_dev,info.st_ino,info.st_uid,info.st_gid,stat.S_IMODE(info.st_mode))==
                        (host['device'],host['inode'],host['uid'],host['gid'],host['mode']))
                result[mount['Destination']]=host
            finally:os.close(fd)
        require('/data/assets' in result and not select.select([pidfd],[],[],0)[0])
        rows=json.loads(sources.inspector.capture(sources.inspector.DOCKER+['inspect',cid]))
        require(len(rows)==1 and rows[0]['Id']==cid and rows[0]['State']['Running'] is True
                and rows[0]['State']['Pid']==pid and rows[0]['State']['StartedAt']==container['State']['StartedAt'])
        return result
    finally:
        for fd in (root,pidfd,proc):
            if fd is not None:os.close(fd)


def configuration_files(path):
    """Root-level configuration files only; mutable asset/upload trees excluded."""
    result={}
    with abort.j.files.directory(str(path)) as parent:
        names=set(os.listdir(parent));require(len(names)<=128)
        for name in sorted(names):
            require(re.fullmatch('[A-Za-z0-9_.-]{1,160}',name))
            info=os.stat(name,dir_fd=parent,follow_symlinks=False)
            if stat.S_ISDIR(info.st_mode):continue
            require(stat.S_ISREG(info.st_mode))
            fd=os.open(name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
            try:
                before=os.fstat(fd)
                require(before.st_nlink==1 and 0<=before.st_size<=4*1024**2)
                value=hashlib.sha256();size=0
                while True:
                    chunk=os.read(fd,65536)
                    if not chunk:break
                    size+=len(chunk);require(size<=before.st_size);value.update(chunk)
                require(size==before.st_size and abort.j.files.identity(before)==abort.j.files.identity(os.fstat(fd))
                        and abort.j.files.identity(before)==abort.j.files.identity(os.stat(name,dir_fd=parent,follow_symlinks=False)))
                result[name]={'sha256':value.hexdigest(),'bytes':size,'device':before.st_dev,'inode':before.st_ino,
                    'uid':before.st_uid,'gid':before.st_gid,'mode':stat.S_IMODE(before.st_mode)}
            finally:os.close(fd)
        require(set(os.listdir(parent))==names)
    require({'compose.yaml','Caddyfile','postgres_password'}<=set(result))
    return result


def database_identity(target):
    command=sources.inspector.database_command(target)
    command[command.index('-U')+1]='postgres'
    value=json.loads(sources.inspector.capture(command,input=SQL))
    require(isinstance(value,dict) and set(value)=={'database','readOnly','localConnection','systemIdentifier','migrations'}
            and value['database']==sources.inspector.DATABASE and value['readOnly']=='on' and value['localConnection'] is True
            and isinstance(value['systemIdentifier'],str) and re.fullmatch('[1-9][0-9]{0,19}',value['systemIdentifier'])
            and isinstance(value['migrations'],list) and len(value['migrations'])<=4096)
    ids=[]
    for row in value['migrations']:
        require(isinstance(row,dict) and set(row)=={'migration_id','checksum','source_commit'})
        sources.inspector.token(row['migration_id']);require(abort.j.hash_value(row['checksum']))
        require(abort.j.hash_value(row['source_commit'],40))
        ids.append(row['migration_id'])
    require(len(ids)==len(set(ids)) and ids==sorted(ids))
    return value


def observe(expected,unit_hashes):
    capture=sources.inspector.capture;docker=sources.inspector.DOCKER
    ids=capture(docker+['ps','--all','--quiet','--no-trunc']).split()
    require(0<len(ids)<=128 and len(ids)==len(set(ids)) and all(abort.j.hash_value(x) for x in ids))
    containers=json.loads(capture(docker+['inspect',*ids]))
    volume_rows=json.loads(capture(docker+['volume','inspect',*sources.VOLUMES.values()]))
    require(len(volume_rows)==len(sources.VOLUMES))
    volumes={row['Name']:row for row in volume_rows};require(len(volumes)==len(volume_rows))
    admitted=sources.admit(containers,volumes,expected)
    units=fence.observe_units(unit_hashes,timer_stopped=False)
    selected={service:next(row for row in containers if row['Id']==binding['containerId'])
              for service,binding in expected.items()}
    stable={service:{'Id':row['Id'],'Image':row['Image'],'Config':row['Config'],'HostConfig':row['HostConfig'],
            'Mounts':sorted(row['Mounts'],key=lambda mount:mount['Destination'])} for service,row in selected.items()}
    require(sha(sources.canonical(stable))==admitted['runtimeConfigurationSha256'])
    result={'expected':expected,'runtimeConfigurationSha256':admitted['runtimeConfigurationSha256'],
        'containers':stable,'volumes':volumes,'roots':{name:directory_identity(path) for name,path in admitted['roots'].items()},
        'configurationFiles':configuration_files(sources.DIRECTORY),'legacyUploads':admitted['legacyUploads'],
        'bindDirectories':bind_directory_identities(selected['api']),
        'units':{'fileHashes':unit_hashes,'admission':units,'timerEnabled':True,'timerActive':True},
        'database':database_identity(expected['db']['containerId'])}
    require(sorted(capture(docker+['ps','--all','--quiet','--no-trunc']).split())==sorted(ids))
    return result


def prepare(journal,directory,expected,unit_hashes):
    """Call under the release and common restore reservations, before maintenance.

    Saves actual sampled identities/configuration/migration history. The caller
    must retain this private path and hash independently of volatile controller
    state and bind recovery to the same configured global control directory.
    """
    journal.guard();records=journal.records();require(len(records)==1)
    with abort.j.files.directory(str(directory),private=True) as parent:
        saved,control=os.fstat(parent),os.fstat(journal.directory_fd)
        require((saved.st_dev,saved.st_ino)!=(control.st_dev,control.st_ino))
    plan=records[0]['event']['plan'];host=abort.boot_identity()
    value=observe(expected,unit_hashes)
    require(value['runtimeConfigurationSha256']==plan['runtimeHash'])
    admission={'schemaVersion':1,'releaseNonce':records[0]['releaseNonce'],'planHash':records[0]['planHash'],
        'host':host,'originalDeployment':value}
    require(len(canonical(admission))<=abort.MAX_ADMISSION//2)
    with abort.j.files.directory(str(directory),private=True) as parent:
        abort.publish(parent,NAME,admission)
        raw,_=abort.read_file(parent,NAME,maximum=abort.MAX_ADMISSION)
        require(raw==canonical(admission))
    # Changed roots/configuration/ledger/host reject readiness even if a private
    # evidence file was published. Do not overwrite it or infer successful prepare.
    require(journal.records()==records and abort.boot_identity()==host and observe(expected,unit_hashes)==value)
    receipt={'schemaVersion':1,'sha256':sha(raw),'releaseNonce':admission['releaseNonce'],
             'planHash':admission['planHash'],'preparedBeforeShutdown':True}
    with abort.j.files.directory(str(directory),private=True) as parent:abort.publish(parent,RECEIPT,receipt)
    return {**receipt,'path':str(Path(directory)/NAME),'continuousWriterExclusionVerified':False}


def read(path,expected_hash,release_nonce,plan_hash):
    require(abort.j.hash_value(expected_hash) and abort.j.hash_value(release_nonce,32) and abort.j.hash_value(plan_hash))
    path=Path(path);require(path.name==NAME)
    with abort.j.files.directory(str(path.parent),private=True) as parent:
        raw,_=abort.read_file(parent,NAME,maximum=abort.MAX_ADMISSION)
    value=json.loads(raw)
    require(canonical(value)==raw and sha(raw)==expected_hash and value['releaseNonce']==release_nonce
            and value['planHash']==plan_hash)
    return value


def read_prepared(directory,release_nonce,plan_hash):
    with abort.j.files.directory(str(directory),private=True) as parent:
        raw,_=abort.read_file(parent,RECEIPT,maximum=abort.j.MAX_RECORD)
    receipt=json.loads(raw)
    require(canonical(receipt)==raw and set(receipt)=={'schemaVersion','sha256','releaseNonce','planHash','preparedBeforeShutdown'}
            and type(receipt['schemaVersion']) is int and receipt['schemaVersion']==1
            and receipt['preparedBeforeShutdown'] is True and receipt['releaseNonce']==release_nonce and receipt['planHash']==plan_hash)
    return read(Path(directory)/NAME,receipt['sha256'],release_nonce,plan_hash)
