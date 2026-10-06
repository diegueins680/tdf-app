#!/usr/bin/env python3
"""Original API recovery within an already admitted abort epoch.

Fixed read-only HTTP probes run inside the exact original network namespace using
host Python. They emit only version/health metadata and bounded validity flags;
public room names and unauthorized response bodies are never returned or logged.
"""
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import subprocess
import time

_spec=importlib.util.spec_from_file_location('application_original_db',Path(__file__).with_name('original-database-recovery.py'))
d=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(d)
require,canonical,sha=d.require,d.canonical,d.sha
PATHS=('/health','/version','/rooms/public','/bookings')
PROBE=r'''
import http.client,json,sys
path=sys.argv[1]
assert path in ('/health','/version','/rooms/public','/bookings')
connection=http.client.HTTPConnection('127.0.0.1',8080,timeout=5)
code=None
try:
    connection.request('GET',path,headers={'Accept':'application/json','Cache-Control':'no-cache'})
    response=connection.getresponse();code=response.status
    data=response.read(65537);assert len(data)<=65536
    result={'code':code,'valid':False,'metadata':{},'transportUnavailable':False}
    if path=='/bookings':
        result['valid']=code==401
    else:
        body=json.loads(data)
        if path=='/rooms/public':
            result['valid']=code==200 and isinstance(body,list) and len(body)<=1024 and all(
                isinstance(row,dict) and set(row)=={'roomId','rName','rBookable'} and
                isinstance(row['roomId'],str) and isinstance(row['rName'],str) and row['rBookable'] is True for row in body)
        elif path=='/health':
            result['valid']=code==200 and body=={'status':'ok','db':'ok'}
            if result['valid']:result['metadata']=body
        else:
            result['valid']=code==200 and isinstance(body,dict) and set(body)=={'name','version','commit','buildTime'} and all(isinstance(v,str) and len(v)<=256 for v in body.values())
            if result['valid']:result['metadata']=body
    print(json.dumps(result))
except Exception as error:
    print(json.dumps({'code':code,'valid':False,'metadata':{},'transportUnavailable':code is None and isinstance(error,(TimeoutError,ConnectionRefusedError,ConnectionResetError))}))
finally:connection.close()
'''


# Fixed SNI/Host with a loopback transport in the held original edge namespace.
# Default certificate/hostname verification stays enabled; TLS failures are not
# classified as retryable absence. No DNS, proxy, redirect or arbitrary endpoint.
EDGE_CONNECTION = r'''import http.client,json,sys,ssl,socket
class LoopbackHTTPS(http.client.HTTPSConnection):
    def connect(self):
        raw=socket.create_connection(('127.0.0.1',443),timeout=self.timeout)
        try:self.sock=self._context.wrap_socket(raw,server_hostname='api.tdfrecords.net')
        except BaseException:
            raw.close()
            raise
'''
require(PROBE.count('import http.client,json,sys')==1 and PROBE.count("http.client.HTTPConnection('127.0.0.1',8080,timeout=5)")==1)
EDGE_PROBE=PROBE.replace('import http.client,json,sys',EDGE_CONNECTION).replace(
    "http.client.HTTPConnection('127.0.0.1',8080,timeout=5)",
    "LoopbackHTTPS('api.tdfrecords.net',443,timeout=5,context=ssl.create_default_context())")


def revision(saved):
    values={}
    for raw in saved['containers']['api']['Config']['Env']:
        key,sep,value=raw.partition('=');require(sep and key not in values);values[key]=value
    expected=values.get('SOURCE_COMMIT')
    require(d.o.abort.j.hash_value(expected,40) and values.get('GIT_SHA')==expected
            and values.get('APP_PORT')=='8080')
    for key in ('GIT_COMMIT','GIT_COMMIT_SHA','COMMIT_SHA','SOURCE_VERSION','SOURCE_SHA','GITHUB_SHA',
                'RENDER_GIT_COMMIT','RENDER_GIT_COMMIT_SHA','VERCEL_GIT_COMMIT_SHA','FLY_GIT_SHA'):
        require(not values.get(key) or values[key]==expected)
    return expected


def target(saved,service='api'):
    require(service in ('api','edge'))
    expected=saved['expected'][service]['containerId']
    rows=json.loads(d.o.sources.inspector.capture(d.o.sources.inspector.DOCKER+['inspect',expected]))
    require(len(rows)==1 and rows[0]['Id']==expected and rows[0]['State']['Running'] is True)
    row=rows[0];require(type(row['State']['Pid']) is int and row['State']['Pid']>0)
    stable={key:(sorted(row[key],key=lambda m:m['Destination']) if key=='Mounts' else row[key]) for key in saved['containers'][service]}
    require(stable==saved['containers'][service])
    return row


def probe(saved,path,service='api'):
    require(path in PATHS and service in ('api','edge'))
    program=PROBE if service=='api' else EDGE_PROBE
    row=target(saved,service);pid=row['State']['Pid'];cid=row['Id']
    proc=os.open('/proc/'+str(pid),os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW)
    pidfd=namespace=None
    try:
        pidfd=os.pidfd_open(pid)
        group=os.open('cgroup',os.O_RDONLY|os.O_NOFOLLOW,dir_fd=proc)
        try:groups=os.read(group,65537).decode().splitlines()
        finally:os.close(group)
        require(any(line.split(':',2)[-1].endswith('/docker-'+cid+'.scope') or
                    line.split(':',2)[-1].endswith('/docker/'+cid) for line in groups))
        namespace=os.open('ns/net',os.O_RDONLY,dir_fd=proc)
        require(not select.select([pidfd],[],[],0)[0])
        result=subprocess.run(['nsenter','--net=/proc/self/fd/'+str(namespace),'python3','-c',program,path],
            env=d.o.fence.ENV,pass_fds=(namespace,),text=True,capture_output=True,timeout=8)
        require(result.returncode==0 and len(result.stdout)<=4096 and not select.select([pidfd],[],[],0)[0])
        closing=target(saved,service)
        require(closing['State']['Pid']==pid and closing['State']['StartedAt']==row['State']['StartedAt'])
        value=json.loads(result.stdout)
        require(set(value)=={'code','valid','metadata','transportUnavailable'} and type(value['valid']) is bool
                and type(value['transportUnavailable']) is bool and isinstance(value['metadata'],dict))
        return value
    finally:
        for fd in (namespace,pidfd,proc):
            if fd is not None:os.close(fd)


class OriginalApplication(d.OriginalDatabase):
    def recover(self):
        self.guard();expected_revision=revision(self.saved)
        def effect(context):
            self.guard();before=d.observe(self.saved);self.guard()
            require(before['running']['db'] and d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            cid=self.saved['expected']['api']['containerId'];submitted=not before['running']['api']
            if submitted:require(d.o.fence.execute(d.o.sources.inspector.DOCKER+['start',cid]).strip()==cid)
            deadline=time.monotonic()+60
            while True:
                self.guard();require(d.observe(self.saved)['running']['api'])
                health=probe(self.saved,'/health')
                if health['code']==200:
                    require(health['valid']);break
                require((health['code'] is None and health['transportUnavailable']) or health['code']==503)
                require(time.monotonic()<deadline);time.sleep(0.25)
            version=probe(self.saved,'/version')
            require(version['valid'] and version['metadata'].get('name')=='tdf-hq'
                    and version['metadata'].get('commit')==expected_revision
                    and re.fullmatch(r'[0-9]+(?:\.[0-9]+){2,3}',version['metadata'].get('version','')))
            rooms=probe(self.saved,'/rooms/public');denial=probe(self.saved,'/bookings')
            require(rooms['valid'] and rooms['code']==200 and denial['valid'] and denial['code']==401)
            self.guard();after=d.observe(self.saved)
            require(after['running']['api'] and after['running']['db']
                    and d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            evidence={'revision':expected_revision,'startSubmitted':submitted,'publicDatabaseReadVerified':True,
                      'anonymousBookingDenied':True,'databaseHash':sha(canonical(self.saved['database'])),
                      'healthAloneEstablishesDatabaseReadiness':False,'originalDeploymentRecovered':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('recover-api',self.targets_hash,effect)
