#!/usr/bin/env python3
"""Real retained namespace across stop, using only a new 64MiB synthetic container."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import sys

ROOT=Path(__file__).resolve().parent.parent
def load(name,path):
    spec=importlib.util.spec_from_file_location(name,ROOT/path)
    value=importlib.util.module_from_spec(spec);spec.loader.exec_module(value);return value

f=load('physical_fixture','scripts/test-physical-postgres-docker.py')
s=load('stopped_storage','ops/hetzner/stopped-application-storage.py')
r,p,require=f.r,f.p,f.require
LABEL='net.tdf.stopped-storage-test'
MEMORY=64*1024**2
SCRIPT="mkdir -p /app/uploads && printf 'synthetic-private-upload' > /app/uploads/sentinel && chmod 600 /app/uploads/sentinel && touch /tmp/ready; trap 'exit 0' TERM; while :; do sleep 1; done"
COMMAND=['-c',SCRIPT]


class Fixture:
    def __init__(self,image,image_id,nonce):
        self.image,self.image_id,self.nonce=image,image_id,nonce
        self.name='tdf-stopped-storage-'+nonce
        self.target=None;self.creation_attempted=False

    def command(self):
        return r.DOCKER+['create','--pull=never','--name',self.name,'--label',LABEL+'='+self.nonce,
            '--network=none','--user=0:0','--entrypoint=/bin/sh','--stop-signal=SIGTERM',
            '--memory='+str(MEMORY),'--memory-swap='+str(MEMORY),'--cpus=0.5','--pids-limit=16',
            '--cap-drop=ALL','--security-opt=no-new-privileges:true',
            '--tmpfs','/var/lib/postgresql/data:rw,nosuid,nodev,size=1048576',self.image,*COMMAND]

    def inspect(self):
        rows=json.loads(r.execute(r.DOCKER+['inspect',self.target or self.name]))
        require(len(rows)==1);value=rows[0];cfg=value['Config'];host=value['HostConfig']
        require(re.fullmatch('[a-f0-9]{64}',value['Id']) and (self.target is None or self.target==value['Id']))
        require(value['Image']==self.image_id and cfg['Image']==self.image and cfg['Labels'].get(LABEL)==self.nonce
                and cfg['User']=='0:0' and cfg['Entrypoint']==['/bin/sh'] and cfg['Cmd']==COMMAND
                and cfg['StopSignal']=='SIGTERM')
        require(host['NetworkMode']=='none' and set(value['NetworkSettings']['Networks'])=={'none'})
        require(host['Memory']==MEMORY and host['MemorySwap']==MEMORY and host['NanoCpus']==500000000
                and host['PidsLimit']==16 and host['CapDrop']==['ALL'] and not host['ReadonlyRootfs'])
        require('no-new-privileges:true' in host['SecurityOpt'] and host['IpcMode']=='private')
        require(not any(host.get(k) for k in ('Privileged','PortBindings','Devices','CapAdd','VolumesFrom',
                                            'Binds','PidMode','UTSMode')))
        require(host['Tmpfs']=={'/var/lib/postgresql/data':'rw,nosuid,nodev,size=1048576'})
        require(len(value['Mounts'])<=1 and all(m['Type']=='tmpfs' and m['Destination']=='/var/lib/postgresql/data'
                                             for m in value['Mounts']))
        self.target=value['Id'];return value

    def start(self):
        self.inspect();r.execute(r.DOCKER+['start',self.target]);self.inspect()
        r.execute(r.DOCKER+['exec',self.target,'sh','-c','for i in 1 2 3 4 5; do test -f /tmp/ready && exit 0; sleep 1; done; exit 1'])

    def stop(self):
        self.inspect();r.execute(r.DOCKER+['stop','--timeout=10',self.target],timeout=20)
        require(self.inspect()['State']['Running'] is False)

    def cleanup(self):
        if self.creation_attempted:
            self.inspect();r.execute(r.DOCKER+['rm','--force',self.target],timeout=15)
            self.target=None;self.creation_attempted=False


def main():
    require(sys.platform=='linux' and os.geteuid()==0 and hasattr(os,'setns') and hasattr(os,'pidfd_open'))
    image=os.environ.get('TDF_PHYSICAL_TEST_IMAGE','')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}',image))
    rows=json.loads(r.execute(r.DOCKER+['image','inspect',image]))
    require(len(rows)==1 and image in rows[0]['RepoDigests'])
    nonce,directory=f.new_directory();fixture=Fixture(image,rows[0]['Id'],nonce)
    with r.rehearsal_lock(p.HOST_ROOT):
        require(not os.path.lexists(p.HOST_ROOT/r.PENDING_NAME))
        for label in (r.LABEL,'net.tdf.application-canary',LABEL):
            require(not r.execute(r.DOCKER+['ps','--all','--quiet','--filter','label='+label]).strip())
        memory=next(int(line.split()[1])*1024 for line in Path('/proc/meminfo').read_text().splitlines()
                    if line.startswith('MemAvailable:'))
        require(memory>=512*1024**2)
        r.reserve_creation(p.HOST_ROOT,nonce,image)
        try:
            fixture.creation_attempted=True
            fixture.target=r.execute(fixture.command()).strip();fixture.inspect();fixture.start()
            source=s.RetainedRoot(fixture.target,image,rows[0]['Id'])
            with source.pinned():
                try: source.capture_uploads(str(directory/'running.tar'))
                except ValueError: pass
                else: raise ValueError('Running source accepted')
                require(not (directory/'running.tar').exists())
                with s.files.relative_directory(source.root_fd,['app','uploads']) as upload_fd:
                    os.setxattr(upload_fd,'user.tdf-synthetic-test',b'synthetic')
                    fixture.stop()
                    require(source.pid not in (0,os.getpid()))
                    try: source.capture_uploads(str(directory/'unsupported-metadata.tar'))
                    except ValueError: pass
                    else: raise ValueError('Actual stopped source xattr was discarded')
                    os.removexattr(upload_fd,'user.tdf-synthetic-test')
                    with s.files.relative_directory(source.root_fd,['app']) as app_fd:
                        os.mkdir('contracts',0o700,dir_fd=app_fd)
                    with s.files.relative_directory(source.root_fd,['app','contracts']) as contracts_fd:
                        os.mkdir('store',0o700,dir_fd=contracts_fd)
                    with s.files.relative_directory(source.root_fd,['app','contracts','store']) as contract_fd:
                        sentinel=os.open('sentinel.json',os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW,0o600,dir_fd=contract_fd)
                        try: require(os.write(sentinel,b'synthetic-retained-contract')==27)
                        finally: os.close(sentinel)
                    try: source.capture_uploads(str(directory/'uncaptured-contract.tar'))
                    except ValueError: pass
                    else: raise ValueError('Uncaptured legacy contract accepted')
                    require(not (directory/'uncaptured-contract.tar').exists())
                    # Only this nonce-owned synthetic fixture file is removed.
                    with s.files.relative_directory(source.root_fd,['app','contracts','store']) as contract_fd:
                        sentinel=os.open('sentinel.json',os.O_RDONLY|os.O_NOFOLLOW,dir_fd=contract_fd)
                        try: require(os.read(sentinel,100)==b'synthetic-retained-contract')
                        finally: os.close(sentinel)
                        os.unlink('sentinel.json',dir_fd=contract_fd)
                    captured=source.capture_uploads(str(directory/'uploads.tar'))
                    require(captured['presence']=='present')
                    result=s.files.restore(str(directory/'uploads.tar'),captured['manifest'],str(directory/'restored'))
                    require((directory/'restored'/'sentinel').read_bytes()==b'synthetic-private-upload')
                    with s.files.directory(str(directory/'restored')) as fd:
                        require(s.files.walk(fd)==captured['manifest'])
                fixture.start();fixture.stop()
                try: source.capture_uploads(str(directory/'restarted.tar'))
                except ValueError: pass
                else: raise ValueError('Restarted source accepted through stale retained root')
                require(not (directory/'restarted.tar').exists())
            try: source.guard()
            except ValueError: pass
            else: raise ValueError('Closed descriptor owner accepted')
        finally:
            # Unknown create or failed full-identity cleanup deliberately retains
            # the durable reservation. Never remove an unrelated container.
            fixture.cleanup()
            require(fixture.target is None and not fixture.creation_attempted)
            r.release_creation(p.HOST_ROOT,nonce,image)
    print(json.dumps({'status':'passed','scope':'synthetic stopped-container writable-layer uploads',
        'retainedNamespaceAfterStop':True,'fullContentAndMetadataRestored':True,
        'unsupportedActualXattrRejected':True,'uncapturedLegacyContractRejected':True,'runningAndRestartedSourcesRejected':True,
        'ownedContainerRemoved':True,'productionDataAccessed':False,'bytes':result['bytes'],
        'manifestSha256':hashlib.sha256(p.canonical(captured['manifest'])).hexdigest()}))


if __name__=='__main__':main()
