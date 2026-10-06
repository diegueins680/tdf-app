#!/usr/bin/env python3
"""Read-only admission of canonical recovery roots and Docker storage writers.

No writer is stopped. The coordinator must hold its release/rehearsal locks,
reobserve this boundary after fencing, and separately admit host workers/mounts.
Raw container inspection remains private; only admitted roots and hashes return.
"""
import hashlib
import importlib.util
import json
from pathlib import Path
import re

_spec=importlib.util.spec_from_file_location('recovery_inspector',Path(__file__).with_name('inspect-runtime.py'))
inspector=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(inspector)
DIRECTORY='/opt/tdf/production'
PROJECT='tdf-production'
VOLUMES={'database':'tdf_production_postgres_data','edge-data':'tdf-restore_caddy_data',
         'edge-config':'tdf-restore_caddy_config'}


def require(value):
    if not value:raise ValueError('Production recovery source admission rejected')


def canonical(value):
    return (json.dumps(value,sort_keys=True,separators=(',',':'))+'\n').encode()


def path(value):
    require(isinstance(value,str) and value.startswith('/') and not value.startswith('//')
            and str(Path(value))==value and '..' not in Path(value).parts)
    return Path(value)


def labels(container,service):
    values=container['Config'].get('Labels') or {}
    require(values.get('com.docker.compose.project')==PROJECT
            and values.get('com.docker.compose.service')==service
            and values.get('com.docker.compose.project.working_dir')==DIRECTORY
            and values.get('com.docker.compose.project.config_files')==DIRECTORY+'/compose.yaml')


def added_capabilities(host,service):
    raw=host.get('CapAdd') or [];require(isinstance(raw,list))
    normalized=[]
    for item in raw:
        require(isinstance(item,str) and re.fullmatch('[A-Za-z0-9_]+',item))
        item=item.upper();normalized.append(item[4:] if item.startswith('CAP_') else item)
    require(len(normalized)==len(set(normalized))
            and set(normalized)<=({'NET_BIND_SERVICE'} if service=='edge' else set()))


def storage_mounts(container):
    values={}
    require(isinstance(container['Mounts'],list))
    for mount in container['Mounts']:
        target=str(path(mount['Destination']));require(target not in values)
        require(type(mount['RW']) is bool and mount['Type'] in ('bind','volume'))
        path(mount['Source']);values[target]=mount
    return values


def bind(mount,source,writable):
    require(mount['Type']=='bind' and mount['Source']==source and mount['RW'] is writable
            and mount.get('Propagation')=='rprivate')


def volume(mount,name,writable,volumes):
    require(mount['Type']=='volume' and mount.get('Name')==name and mount['RW'] is writable)
    value=volumes[name]
    require(value['Name']==name and value['Driver']=='local' and value['Scope']=='local'
            and not value.get('Options') and value['Mountpoint']==mount['Source'])
    path(value['Mountpoint']);return value['Mountpoint']


def _admit(containers,volumes,expected,*,stopped=False,stopped_services=None,abort_recovery=False):
    """Pure admission. Expected IDs/images come from the separately trusted plan.

    Only api/db/edge may run at initial admission. A stopped canonical canary and
    other stopped containers grant no writer authority. Every resample checks all
    containers again; concurrent privileged changes remain outside this boundary.
    """
    require(isinstance(containers,list) and isinstance(volumes,dict)
            and set(volumes)==set(VOLUMES.values()) and isinstance(expected,dict)
            and set(expected)=={'api','db','edge'} and type(stopped) is bool and type(abort_recovery) is bool)
    require(stopped_services is None or (stopped is False and isinstance(stopped_services, frozenset)
            and stopped_services <= {'api','db','edge'}))
    halted = frozenset(expected) if stopped else (stopped_services or frozenset())
    by_service={};seen=set()
    for item in containers:
        cid=item['Id'];require(isinstance(cid,str) and re.fullmatch('[a-f0-9]{64}',cid) and cid not in seen);seen.add(cid)
        state=item['State'];require(type(state['Running']) is bool)
        service=next((name for name,value in expected.items() if value['containerId']==cid),None)
        if service is None:
            require(state['Running'] is False and state['Restarting'] is False)
            require(type(state['Pid']) is int and state['Pid']==0 and state['Paused'] is False
                    and state['Dead'] is False and state['Status'] in ('created','exited'))
            require(item['HostConfig']['RestartPolicy']['Name'] in ('','no','unless-stopped'))
            # A duplicate canonical service, even stopped, indicates ambiguous
            # deployment identity. The optional stopped canary is distinct.
            tags=item['Config'].get('Labels') or {}
            if tags.get('com.docker.compose.project')==PROJECT:
                require(tags.get('com.docker.compose.service')=='canary')
                labels(item,'canary')
                require(item['HostConfig']['RestartPolicy']['Name'] in ('','no'))
            continue
        require(service not in by_service);labels(item,service)
        binding=expected[service]
        require(set(binding)=={'containerId','image','imageId'}
                and re.fullmatch(r'[a-zA-Z0-9./_-]+@sha256:[a-f0-9]{64}',binding['image'])
                and re.fullmatch(r'sha256:[a-f0-9]{64}',binding['imageId'])
                and item['Config']['Image']==binding['image'] and item['Image']==binding['imageId'])
        service_stopped = not state['Running'] if abort_recovery else service in halted
        if abort_recovery and service_stopped:halted=halted | {service}
        require(state['Running'] is (not service_stopped) and all(state[k] is False for k in ('Paused','Restarting','Dead','OOMKilled')))
        require(type(state['Pid']) is int and (state['Pid']==0 if service_stopped else state['Pid']>0))
        if service_stopped:
            require(state['Status']=='exited' and type(state['ExitCode']) is int
                    and state['ExitCode'] in ((0,137,143) if abort_recovery else ((0,) if service=='db' else (0,143))))
        host=item['HostConfig']
        added_capabilities(host,service)
        require(host['RestartPolicy']['Name']=='unless-stopped' and host['AutoRemove'] is False
                and host['Privileged'] is False and not host.get('Devices') and not host.get('VolumesFrom')
                and host.get('PidMode','')=='' and host.get('UTSMode','')==''
                and host.get('IpcMode')=='private' and not host.get('Tmpfs'))
        by_service[service]=item
    require(set(by_service)==set(expected))
    # Existing collector checks networking, DB mount identity and PGDATA, without
    # assuming a stopped container's health means its database shut down cleanly.
    for service,item in by_service.items():inspector.summarize_container(service,item)
    require(set(by_service['edge']['NetworkSettings']['Networks'])=={PROJECT+'_outbound'})
    db=storage_mounts(by_service['db']);api=storage_mounts(by_service['api']);edge=storage_mounts(by_service['edge'])
    require(set(db)=={'/var/lib/postgresql/data','/run/secrets/postgres_password'})
    require(set(api) in ({'/data/assets'},{'/data/assets','/app/uploads'}))
    require(set(edge)=={'/etc/caddy/Caddyfile','/data','/config'})
    roots={'production':DIRECTORY,
           'database':volume(db['/var/lib/postgresql/data'],VOLUMES['database'],True,volumes),
           'edge-data':volume(edge['/data'],VOLUMES['edge-data'],True,volumes),
           'edge-config':volume(edge['/config'],VOLUMES['edge-config'],True,volumes)}
    bind(db['/run/secrets/postgres_password'],DIRECTORY+'/postgres_password',False)
    bind(api['/data/assets'],DIRECTORY+'/assets',True)
    bind(edge['/etc/caddy/Caddyfile'],DIRECTORY+'/Caddyfile',False)
    if '/app/uploads' in api:bind(api['/app/uploads'],DIRECTORY+'/uploads',True)
    values=list(map(path,roots.values()))
    for index,left in enumerate(values):
        for right in values[index+1:]:require(left!=right and left not in right.parents and right not in left.parents)
    # All reported mount roots are absolute canonical paths. Descriptor/no-follow,
    # inode, host mount-table and complete-file admission remain separate checks.
    stable={service:{'Id':item['Id'],'Image':item['Image'],'Config':item['Config'],
                     'HostConfig':item['HostConfig'],
                     # Docker can reorder this destination-unique collection on stop.
                     # Keep every mount field while making set order irrelevant.
                     'Mounts':sorted(item['Mounts'], key=lambda mount: mount['Destination'])}
            for service,item in by_service.items()}
    return {'roots':roots,'legacyUploads':('/app/uploads' not in api),
            'runtimeConfigurationSha256':hashlib.sha256(canonical(stable)).hexdigest(),
            'dockerWritersStopped':halted == frozenset(expected),'hostWorkersFenced':False,'databaseCleanShutdownVerified':False,
            'scope':'Canonical configured roots and sampled Docker writers only; no mutation or snapshot proof'}


def admit(containers,volumes,expected,*,stopped=False,stopped_services=None):
    """Canonical capture admission; unclean exit remains forbidden."""
    return _admit(containers,volumes,expected,stopped=stopped,stopped_services=stopped_services)


def admit_abort(containers,volumes,expected):
    """Original in-place recovery only, after separately verified fresh boot.

    Mixed running/exited services and forced exit137 are allowed. This is neither
    writer exclusion nor clean-shutdown evidence. No OOM/dead/restarting service,
    changed target/configuration or extra running container is admitted.
    """
    result=_admit(containers,volumes,expected,abort_recovery=True)
    return {**result,'scope':'Sampled original runtime for in-place abort recovery only',
            'captureAuthorized':False,'databaseCleanShutdownVerified':False}


def observe(expected,*,stopped=False,stopped_services=None):
    """Fixed local Docker socket; raw inspection data never leaves this function."""
    capture=inspector.capture;docker=inspector.DOCKER
    ids=capture(docker+['ps','--all','--quiet','--no-trunc']).split()
    require(0<len(ids)<=128 and all(re.fullmatch('[a-f0-9]{64}',value) for value in ids))
    containers=json.loads(capture(docker+['inspect',*ids]))
    rows=json.loads(capture(docker+['volume','inspect',*VOLUMES.values()]))
    require(len(rows)==len(VOLUMES))
    volumes={item['Name']:item for item in rows};require(len(volumes)==len(rows))
    result=admit(containers,volumes,expected,stopped=stopped,stopped_services=stopped_services)
    # Reject inventory changes across the sequential inspection interval.
    require(sorted(capture(docker+['ps','--all','--quiet','--no-trunc']).split())==sorted(ids))
    return result
