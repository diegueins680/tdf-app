#!/usr/bin/env python3
"""Sample explicit host process classes; never infer continuous writer exclusion.

The reviewed OS, kernel and caller remain trusted. This is not a sandbox or an
inspection of process memory, libraries, open descriptors or all OS configuration.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import stat

PROC=Path('/proc')
MAX_PROCESS=4096
MAX_EXECUTABLE=100*1024**2
UNATTENDED='/usr/share/unattended-upgrades/unattended-upgrade-shutdown'


def require(value):
    if not value:raise ValueError('Host process admission rejected')


def read(fd,name,maximum=65536):
    child=os.open(name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=fd)
    try:
        data=os.read(child,maximum+1);require(len(data)<=maximum);return data
    finally:os.close(child)


def process_stat(data):
    require(isinstance(data,bytes) and len(data)<=65536)
    lead,separator,tail=data.rpartition(b') ')
    require(separator and b' (' in lead)
    pid=lead.split(b' (',1)[0];fields=tail.split()
    require(pid.isdigit() and len(fields)>=20 and fields[1].isdigit() and fields[19].isdigit())
    return {'pid':int(pid),'parent':int(fields[1]),'state':fields[0].decode('ascii'),
            'startTicks':int(fields[19])}


def cgroup(data):
    require(isinstance(data,bytes))
    text=data.decode('ascii');rows=text.splitlines()
    require(len(rows)==1 and rows[0].startswith('0::/'))
    path=rows[0][3:]
    require('//' not in path and '..' not in path.split('/') and len(path)<=4096)
    return path


def invocation(executable,units,args,comm):
    # Raw argv is examined only for these known OS interpreters/helpers, never
    # emitted. No arbitrary interpreter is classified as native OS code.
    if units==['unattended-upgrades.service'] and executable in ('/usr/bin/python3.12','/usr/bin/python3.12 (deleted)'):
        require(len(args)==3 and args[0] in (b'/usr/bin/python3',b'/usr/bin/python3.12')
                and args[1]==UNATTENDED.encode() and args[2]==b'--wait-for-signal')
        return 'unattended-shutdown-waiter'
    if units==['user@0.service']:
        if executable=='/usr/lib/systemd/systemd':
            require(len(args)==2 and args[0] in (b'/lib/systemd/systemd',b'/usr/lib/systemd/systemd') and args[1]==b'--user')
            return 'root-user-manager'
        if executable=='/usr/lib/systemd/systemd-executor':
            require(args==[b'(sd-pam)'] and comm==b'(sd-pam)\n')
            return 'root-user-pam-helper'
    return 'native-os'


def executable_hash(fd,cache):
    child=os.open('exe',os.O_RDONLY,dir_fd=fd)  # Deliberate procfs magic-link open.
    try:
        info=os.fstat(child)
        require(stat.S_ISREG(info.st_mode) and 0<info.st_size<=MAX_EXECUTABLE)
        key=(info.st_dev,info.st_ino,info.st_size,info.st_mtime_ns,info.st_ctime_ns)
        if key not in cache:
            value=hashlib.sha256();count=0
            while True:
                data=os.read(child,65536)
                if not data:break
                count+=len(data);require(count<=info.st_size);value.update(data)
            require(count==info.st_size);cache[key]=value.hexdigest()
        after=os.stat('exe',dir_fd=fd)
        require((after.st_dev,after.st_ino,after.st_size,after.st_mtime_ns,after.st_ctime_ns)==key)
        return cache[key]
    finally:os.close(child)


def processes():
    require(os.geteuid()==0)
    names=sorted(x for x in os.listdir(PROC) if x.isdigit());require(len(names)<=MAX_PROCESS)
    result=[];cache={}
    for name in names:
        try:
            fd=os.open(PROC/name,os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW)
            try:
                before=process_stat(read(fd,'stat'))
                if before['state']=='Z':continue  # A zombie cannot execute or spawn.
                status=read(fd,'status')
                kernel=[line for line in status.splitlines() if line.startswith(b'Kthread:')]
                require(len(kernel)==1)
                if kernel[0].split()==[b'Kthread:',b'1']:continue
                require(kernel[0].split()==[b'Kthread:',b'0'])
                group=cgroup(read(fd,'cgroup'))
                docker=re.fullmatch(r'/system.slice/docker-([a-f0-9]{64})\.scope',group)
                row={**before,'cgroup':group}
                row.pop('state')
                if docker:
                    row.update({'containerId':docker[1]})
                elif before['pid']==os.getpid():
                    row.update({'observer':True})
                else:
                    executable=os.readlink('exe',dir_fd=fd)
                    units=re.findall(r'(?:^|/)([a-zA-Z0-9@_.-]+\.service)(?=/|$)',group)
                    args=read(fd,'cmdline').split(b'\0');args=args[:-1] if args[-1]==b'' else args
                    row.update({'units':units,'executable':executable,'executableSha256':executable_hash(fd,cache),
                                'invocation':invocation(executable,units,args,read(fd,'comm'))})
                    require(os.readlink('exe',dir_fd=fd)==executable)
                    if row['invocation']=='unattended-shutdown-waiter':
                        with open(UNATTENDED,'rb') as source:
                            content=source.read(1024**2+1);require(len(content)<=1024**2)
                            row['scriptSha256']=hashlib.sha256(content).hexdigest()
                after=process_stat(read(fd,'stat'))
                require(all(after[key]==before[key] for key in ('pid','parent','startTicks'))
                        and cgroup(read(fd,'cgroup'))==group)
                result.append(row)
            finally:os.close(fd)
        except (FileNotFoundError,ProcessLookupError):
            # A vanished task contributes no live class. Reused PIDs cannot be
            # resolved through the already-open proc directory; resample later.
            require(not os.path.exists(PROC/name))
            continue
    return sorted(result,key=lambda row:row['pid'])


def admit(rows,policy,containers,observer_pid):
    require(isinstance(policy,dict) and set(policy)=={'schemaVersion','provenance','classes'}
            and type(policy['schemaVersion']) is int and policy['schemaVersion']==1)
    require(isinstance(containers,frozenset) and all(re.fullmatch('[a-f0-9]{64}',x) for x in containers))
    by_pid={r['pid']:r for r in rows};require(len(by_pid)==len(rows) and observer_pid in by_pid)
    ancestry=set();pid=observer_pid
    while pid:
        require(pid in by_pid and pid not in ancestry)
        ancestry.add(pid);pid=by_pid[pid]['parent']
    counts={'observer':0,'docker':0,'trustedOs':0,'observerAncestry':0}
    for row in rows:
        if 'containerId' in row:
            require(row['containerId'] in containers);counts['docker']+=1;continue
        if row.get('observer') is True:
            require(row['pid']==observer_pid);counts['observer']+=1;continue
        keys=('units','executable','executableSha256','invocation')
        match={k:row[k] for k in keys}
        if 'scriptSha256' in row:match['scriptSha256']=row['scriptSha256']
        require(match in policy['classes'])
        if not row['units']:
            require(row['pid'] in ancestry)
            # The root manager has PID1; the only other admitted unscoped class
            # is an exact SSH executable on this observer's ancestry chain.
            require((row['pid']==1 and row['executable']=='/usr/lib/systemd/systemd')
                    or row['executable']=='/usr/sbin/sshd')
            counts['observerAncestry']+=1
        else:counts['trustedOs']+=1
    require(counts['observer']==1)
    return counts


def observe(policy,containers):
    first=processes();counts=admit(first,policy,containers,os.getpid())
    second=processes();admit(second,policy,containers,os.getpid());require(second==first)
    digest=hashlib.sha256((json.dumps(policy,sort_keys=True,separators=(',',':'))+'\n').encode()).hexdigest()
    return {'schemaVersion':1,'readOnly':True,'policySha256':digest,'sampledClasses':counts,
            'continuousWriterExclusion':False,'openFileInventoryVerified':False,'deploymentAuthorized':False}
