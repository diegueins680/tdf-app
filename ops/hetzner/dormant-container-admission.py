#!/usr/bin/env python3
"""Read-only qualification of Docker's persisted manual-stop boundary.

This component never starts, stops, updates or deletes a container. The caller
retains release ownership and supplies independently reviewed daemon/boot evidence.
It does not establish network isolation or authorize a production reboot.
"""
import hashlib
import http.client
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import socket
import stat
import struct
import subprocess

spec=importlib.util.spec_from_file_location('dormant_files',Path(__file__).with_name('recovery-files.py'))
files=importlib.util.module_from_spec(spec);spec.loader.exec_module(files)
MAX_BYTES=2*1024**2


def require(value):
    if not value:raise ValueError('Dormant container admission rejected')


def identifier(value):
    require(isinstance(value,str) and re.fullmatch('[a-f0-9]{64}',value))
    return value


def read_metadata(root,cid):
    """Never return credentials or the raw Docker configuration."""
    identifier(cid)
    with files.directory(str(Path(root)/'containers'/cid)) as parent:
        fd=os.open('config.v2.json',os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
        try:
            before=os.fstat(fd)
            require(stat.S_ISREG(before.st_mode) and before.st_uid==0 and before.st_nlink==1
                    and not before.st_mode & 0o022 and 0<before.st_size<=MAX_BYTES)
            raw=b''
            while len(raw)<=before.st_size:
                part=os.read(fd,65536)
                if not part:break
                raw+=part
            require(len(raw)==before.st_size and files.identity(os.fstat(fd))==files.identity(before)
                    and files.identity(os.stat('config.v2.json',dir_fd=parent,follow_symlinks=False))==files.identity(before))
            value=json.loads(raw);require(value['ID']==cid)
            return {'id':cid,'sha256':hashlib.sha256(raw).hexdigest(),
                    'manuallyStopped':value.get('HasBeenManuallyStopped'),
                    'startedBefore':value.get('HasBeenStartedBefore')}
        finally:os.close(fd)


def configuration_fingerprint(value):
    """Bind complete inspect configuration; runtime state remains separate."""
    identifier(value['Id'])
    require(isinstance(value['Image'],str) and re.fullmatch('sha256:[a-f0-9]{64}',value['Image'])
            and isinstance(value['Config'],dict) and isinstance(value['HostConfig'],dict)
            and isinstance(value['Mounts'],list))
    mounts=value['Mounts'];destinations=[row['Destination'] for row in mounts]
    require(all(isinstance(name,str) and name.startswith('/') for name in destinations)
            and len(destinations)==len(set(destinations)))
    stable={key:value[key] for key in ('Id','Image','Config','HostConfig')}
    stable['Mounts']=sorted(mounts,key=lambda row:row['Destination'])
    return hashlib.sha256((json.dumps(stable,sort_keys=True,separators=(',',':'))+'\n').encode()).hexdigest()


def container_row(value,metadata=None):
    cid=identifier(value['Id']);state=value['State'];host=value['HostConfig']
    require(all(type(state[key]) is bool for key in ('Running','Paused','Restarting','Dead'))
            and type(state['Pid']) is int and state['Pid']>=0)
    row={'id':cid,'configurationSha256':configuration_fingerprint(value),'running':state['Running'],'pid':state['Pid'],'paused':state['Paused'],
         'restarting':state['Restarting'],'dead':state['Dead'],'status':state['Status'],
         'restartPolicy':host['RestartPolicy'],'networkMode':host['NetworkMode'],
         'networkIds':sorted(identifier(n['NetworkID']) for n in value['NetworkSettings']['Networks'].values()),
         'metadata':metadata}
    if not row['running']:
        require(row['pid']==0 and not any(row[k] for k in ('paused','restarting','dead'))
                and row['status'] in ('created','exited') and isinstance(metadata,dict) and metadata['id']==cid)
    return row


def check_dormant(row):
    require(row['running'] is False and row['pid']==0
            and not any(row[k] for k in ('paused','restarting','dead'))
            and row['status'] in ('created','exited'))
    policy=row['restartPolicy']
    require(isinstance(policy,dict) and set(policy)=={'Name','MaximumRetryCount'}
            and type(policy['MaximumRetryCount']) is int and policy['MaximumRetryCount']==0)
    require(policy['Name'] in ('no','unless-stopped'))
    if policy['Name']=='unless-stopped':
        require(row['status']=='exited' and row['metadata']['manuallyStopped'] is True
                and row['metadata']['startedBefore'] is True)


def proc_start(pid):
    raw=Path('/proc',str(pid),'stat').read_text()
    # The parenthesized comm can contain spaces and ')'; fields follow its last ')'.
    fields=raw[raw.rfind(')')+2:].split();require(len(fields)>19)
    return int(fields[19])


def executable(pid):
    fd=os.open('/proc/'+str(pid)+'/exe',os.O_RDONLY)
    try:
        before=os.fstat(fd)
        require(stat.S_ISREG(before.st_mode) and before.st_uid==0
                and not before.st_mode & 0o022 and 0<before.st_size<=128*1024**2)
        digest=hashlib.sha256();size=0
        while True:
            block=os.read(fd,1024**2)
            if not block:break
            size+=len(block);require(size<=before.st_size);digest.update(block)
        require(size==before.st_size and files.identity(os.fstat(fd))==files.identity(before))
        return {'sha256':digest.hexdigest(),'bytes':size}
    finally:os.close(fd)


def serving_pid(peer_pid,path):
    if peer_pid!=1:return peer_pid
    # socket activation retains PID1's peer credentials. Bind the inherited
    # listening socket to the actual canonical docker.service process instead.
    result=subprocess.run(['systemctl','show','docker.service','--property=MainPID','--value'],
                          capture_output=True,text=True,timeout=10,
                          env={'PATH':'/usr/sbin:/usr/bin:/sbin:/bin','LANG':'C.UTF-8'})
    require(result.returncode==0 and re.fullmatch('[0-9]+\n?',result.stdout))
    pid=int(result.stdout);require(pid>1)
    matches=[]
    for line in Path('/proc/net/unix').read_text().splitlines()[1:]:
        fields=line.split(maxsplit=7)
        if (len(fields)==8 and fields[3:6]==['00010000','0001','01']
                and fields[7].startswith('/') and str(Path(fields[7]).resolve())==path):
            matches.append(fields[6])
    require(len(matches)==1)
    descriptors=list(Path('/proc',str(pid),'fd').iterdir());require(len(descriptors)<=65536)
    links=[]
    for descriptor in descriptors:
        try:links.append(os.readlink(descriptor))
        except FileNotFoundError:pass  # unrelated short-lived descriptor
    require('socket:['+matches[0]+']' in links)
    return pid


class Docker(http.client.HTTPConnection):
    def __init__(self,path):
        super().__init__('localhost',timeout=20)
        self.path=str(Path(path).resolve(strict=True));self.peer=None

    def connect(self):
        self.sock=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM);self.sock.settimeout(20)
        self.sock.connect(self.path)
        pid,uid,gid=struct.unpack('3i',self.sock.getsockopt(socket.SOL_SOCKET,socket.SO_PEERCRED,12))
        require(uid==0 and pid>0)
        pid=serving_pid(pid,self.path)
        identity=(pid,proc_start(pid))
        require(self.peer is None or self.peer==identity);self.peer=identity

    def get(self,path):
        require(path in ('/info','/containers/json?all=1')
                or re.fullmatch('/containers/[a-f0-9]{64}/json',path))
        self.request('GET',path)
        response=self.getresponse();raw=response.read(MAX_BYTES+1)
        require(response.status==200 and len(raw)<=MAX_BYTES)
        require(proc_start(self.peer[0])==self.peer[1])
        return json.loads(raw)


def observe(socket_path='/run/docker.sock'):
    require(os.geteuid()==0)
    client=Docker(socket_path);pidfd=None
    try:
        info=client.get('/info');pid=client.peer[0];pidfd=os.pidfd_open(pid)
        poll=select.poll();poll.register(pidfd,select.POLLIN)
        require(not poll.poll(0))
        require(info['ServerVersion']=='29.1.3')
        daemon=executable(pid)
        root=info['DockerRootDir'];require(isinstance(root,str) and root.startswith('/'))
        listing=client.get('/containers/json?all=1')
        require(isinstance(listing,list) and len(listing)<=128)
        ids=sorted(identifier(row['Id']) for row in listing);require(len(set(ids))==len(ids))
        rows=[]
        for cid in ids:
            value=client.get('/containers/'+cid+'/json')
            metadata=None if value['State']['Running'] else read_metadata(root,cid)
            rows.append(container_row(value,metadata))
        require(sorted(row['Id'] for row in client.get('/containers/json?all=1'))==ids
                and client.get('/info')['DockerRootDir']==root and executable(pid)==daemon
                and proc_start(pid)==client.peer[1] and not poll.poll(0))
        return {'schemaVersion':1,'version':info['ServerVersion'],'socket':client.path,
                'dataRoot':root,'daemon':daemon,'daemonProcess':{'pid':pid,'startTicks':client.peer[1]},'containers':rows}
    finally:
        client.close()
        if pidfd is not None:os.close(pidfd)


def admit(snapshot,policy):
    """Matching caller policy is necessary, not self-issued release approval."""
    require(isinstance(policy,dict) and set(policy)=={'schemaVersion','qualification','snapshot'}
            and type(policy['schemaVersion']) is int and policy['schemaVersion']==1)
    evidence=policy['qualification']
    require(isinstance(evidence,dict) and set(evidence)=={'sourceRevision','daemonRestartEvidenceSha256','rebootEvidenceSha256'})
    for key,value in evidence.items():
        require(isinstance(value,str) and re.fullmatch('[a-f0-9]{'+('40' if key=='sourceRevision' else '64')+'}',value))
    require(isinstance(snapshot,dict) and set(snapshot)=={'schemaVersion','version','socket','dataRoot','daemon','daemonProcess','containers'}
            and type(snapshot['schemaVersion']) is int and snapshot['schemaVersion']==1
            and snapshot['version']=='29.1.3' and snapshot==policy['snapshot'])
    incarnation=snapshot['daemonProcess']
    require(isinstance(incarnation,dict) and set(incarnation)=={'pid','startTicks'}
            and type(incarnation['pid']) is int and incarnation['pid']>1
            and type(incarnation['startTicks']) is int and incarnation['startTicks']>0)
    ids=[]
    for row in snapshot['containers']:
        ids.append(identifier(row['id']))
        require(isinstance(row.get('configurationSha256'),str) and re.fullmatch('[a-f0-9]{64}',row['configurationSha256']))
        if row['running']:
            require(row['pid']>0 and row['status']=='running' and row['metadata'] is None
                    and not any(row[k] for k in ('paused','restarting','dead')))
        else:check_dormant(row)
    require(len(ids)==len(set(ids)))
    return {'schemaVersion':1,'dormantContainersMatchReviewedQualification':True,
            'qualification':evidence,'hostBypassAdmissionVerified':False,
            'snapshotSha256':hashlib.sha256(json.dumps(snapshot,sort_keys=True,separators=(',',':')).encode()).hexdigest()}


def observe_qualified(policy,socket_path='/run/docker.sock'):
    before=observe(socket_path);result=admit(before,policy)
    require(observe(socket_path)==before)
    return result
