#!/usr/bin/env python3
"""Read-only admission of reviewed OS schedulers, not a continuous writer fence.

Trusted OS/package behavior and privileged noncooperation remain assumptions.
The coordinator separately admits processes, Docker writers and the TDF timer.
"""
import grp
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import stat
import subprocess

spec=importlib.util.spec_from_file_location('scheduler_files',Path(__file__).with_name('recovery-files.py'))
files=importlib.util.module_from_spec(spec);spec.loader.exec_module(files)
ENV={'PATH':'/usr/sbin:/usr/bin:/sbin:/bin','LANG':'C.UTF-8','SYSTEMD_COLORS':'0'}
CRON_PATHS=('/etc/crontab','/etc/anacrontab','/etc/cron.d','/etc/cron.hourly',
            '/etc/cron.daily','/etc/cron.weekly','/etc/cron.monthly','/etc/cron.yearly',
            '/var/spool/cron/crontabs')
PROPERTIES=('Id','LoadState','FragmentPath','DropInPaths','NeedDaemonReload','Transient')
TDF_TIMER='tdf-postgres-backup.timer'
TDF_SERVICE='tdf-postgres-backup.service'


def require(value):
    if not value:raise ValueError('Host scheduler admission rejected')


def execute(args):
    environment=dict(ENV)
    if args[:2]==['systemctl','--user']:
        environment.update({'XDG_RUNTIME_DIR':'/run/user/0','DBUS_SESSION_BUS_ADDRESS':'unix:path=/run/user/0/bus'})
    result=subprocess.run(args,env=environment,text=True,capture_output=True,timeout=20)
    require(result.returncode==0 and len(result.stdout)<=1024**2)
    return result.stdout


def file_row(path):
    require(isinstance(path,str) and str(Path(path))==path and path.startswith('/')
            and '..' not in Path(path).parts)
    with files.directory(str(Path(path).parent)) as parent:
        fd=os.open(Path(path).name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
        try:
            before=os.fstat(fd)
            require(stat.S_ISREG(before.st_mode) and before.st_uid==0 and before.st_nlink==1
                    and not before.st_mode & 0o022 and before.st_size<=1024**2)
            raw=os.read(fd,1024**2+1)
            require(len(raw)==before.st_size and files.identity(os.fstat(fd))==files.identity(before)
                    and files.identity(os.stat(Path(path).name,dir_fd=parent,follow_symlinks=False))==files.identity(before))
            return {'path':path,'sha256':hashlib.sha256(raw).hexdigest(),'bytes':len(raw)}
        finally:os.close(fd)


def scheduled_files():
    result=[]
    for name in CRON_PATHS:
        path=Path(name)
        try:info=path.lstat()
        except FileNotFoundError:continue
        if stat.S_ISDIR(info.st_mode):
            with files.directory(name) as fd:
                require(info.st_uid==0)
                if name=='/var/spool/cron/crontabs':
                    require(stat.S_IMODE(info.st_mode)==0o1730
                            and info.st_gid==grp.getgrnam('crontab').gr_gid)
                else:require(not info.st_mode & 0o022)
                require(files.identity(os.fstat(fd))==files.identity(info))
                entries=sorted(os.listdir(fd));require(len(entries)<=128)
                for entry in entries:result.append(file_row(str(path/entry)))
                require(sorted(os.listdir(fd))==entries
                        and files.identity(os.fstat(fd))==files.identity(info)
                        and files.identity(path.lstat())==files.identity(info))
        else:result.append(file_row(name))
    return sorted(result,key=lambda row:row['path'])


def unit_names(kind,state,*,user=False):
    raw=execute(['systemctl',*(['--user'] if user else []),'list-units','--type='+kind,*state,'--plain','--no-legend','--no-pager'])
    values=[line.split()[0] for line in raw.splitlines() if line.strip()]
    require(len(values)==len(set(values)) and len(values)<=256)
    return set(values)


def unit_row(name,*,user=False):
    raw=execute(['systemctl',*(['--user'] if user else []),'show',name,'--property='+','.join(PROPERTIES)])
    row={}
    for line in raw.splitlines():
        key,separator,value=line.partition('=')
        require(separator and key in PROPERTIES and key not in row);row[key]=value
    require(set(row)==set(PROPERTIES) and row['Id']==name and row['LoadState']=='loaded'
            and row['NeedDaemonReload']=='no' and row['Transient']=='no')
    drops=row['DropInPaths'].split();require(len(drops)<=32 and len(drops)==len(set(drops)))
    return {'unit':name,'fragment':file_row(row['FragmentPath']),
            'dropIns':[file_row(path) for path in drops]}


def observe(policy):
    """Policy is separately reviewed source, never learned from this observation."""
    require(os.geteuid()==0 and isinstance(policy,dict) and policy.get('schemaVersion')==1)
    require(set(policy)=={'schemaVersion','provenance','scheduledFiles','units','runningServices','timers','userManagers'})
    require(set(policy['userManagers'])<={'user@0.service'})
    services=unit_names('service',['--state=running'])
    timers=unit_names('timer',['--all'])
    require(services<=set(policy['runningServices']) and timers==set(policy['timers'])|{TDF_TIMER})
    require({name for name in services if name.startswith('user@')}<=set(policy['userManagers']))
    # No active backup may overlap capture. Its configuration/lifecycle is checked
    # independently by WriterFence, not admitted as an OS worker here.
    require(TDF_SERVICE not in services and not execute(['atq']).strip())
    observed=scheduled_files()
    require(observed==policy['scheduledFiles'])
    units=[unit_row(name) for name in sorted(policy['units'])]
    require(units==[policy['units'][name] for name in sorted(policy['units'])])
    users={}
    for manager in sorted(set(policy['userManagers']) & services):
        expected=policy['userManagers'][manager]
        names=unit_names('timer',['--all'],user=True)
        require(names==set(expected['timers']))
        user_rows=[unit_row(name,user=True) for name in sorted(expected['units'])]
        require(user_rows==[expected['units'][name] for name in sorted(expected['units'])])
        users[manager]=(names,user_rows)
    require(scheduled_files()==observed and unit_names('service',['--state=running'])==services
            and unit_names('timer',['--all'])==timers and not execute(['atq']).strip())
    require([unit_row(name) for name in sorted(policy['units'])]==units)
    for manager,(names,user_rows) in users.items():
        expected=policy['userManagers'][manager]
        require(unit_names('timer',['--all'],user=True)==names
                and [unit_row(name,user=True) for name in sorted(expected['units'])]==user_rows)
    raw=(json.dumps(policy,sort_keys=True,separators=(',',':'))+'\n').encode()
    return {'schemaVersion':1,'policySha256':hashlib.sha256(raw).hexdigest(),
            'scheduledFiles':len(observed),'unitConfigurations':len(units),
            'userManagersObserved':len(users),'atQueueEmpty':True,
            'readOnly':True,'processInventoryVerified':False,'continuousWriterExclusion':False,
            'deploymentAuthorized':False}
