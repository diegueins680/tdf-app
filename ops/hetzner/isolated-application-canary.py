#!/usr/bin/env python3
"""Run one candidate only inside the admitted disposable restore network namespace.

No production mounts, credentials, published ports, image pulls or provider routes.
The restore owner must retain its durable pending reservation until cleanup succeeds.
"""
import copy
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import time

LABEL = 'net.tdf.application-canary'
MEMORY = 512 * 1024 * 1024
ENVIRONMENT = {
    'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8',
    'APP_ENV': 'production', 'APP_PORT': '8080', 'DB_HOST': '127.0.0.1', 'DB_PORT': '5432',
    'DB_USER': 'postgres', 'DB_PASS': 'synthetic-isolated-canary', 'DB_NAME': 'tdf_hq',
    'DB_SSLMODE': 'disable', 'RUN_MIGRATIONS': 'false', 'AUTO_APPLY_PRODUCTION_MIGRATIONS': 'false',
    'RESET_DB': 'false', 'SEED_DB': 'false', 'ALLOW_ALL_ORIGINS': 'false',
    'CORS_DISABLE_DEFAULTS': 'true', 'ALLOWED_ORIGINS': 'https://www.tdfrecords.net,https://tdfrecords.net',
    'HQ_ASSETS_DIR': '/data/assets', 'TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT': '/data/assets/.internal-feedback',
    'EVENT_DISCOVERY_ENABLED': 'false', 'EVENT_DISCOVERY_AUTO_PUBLISH': 'false',
    'EVENT_LOGISTICS_RECHECK_ENABLED': 'false', 'ARTIST_ENRICHMENT_ENABLED': 'false',
    'REPUTATION_AGGREGATION_WORKER_ENABLED': 'false', 'TICKET_CONFIRMATION_WORKER_ENABLED': 'false',
}
COMMAND = ['env', '-i', *[key+'='+value for key,value in ENVIRONMENT.items()], '/app/production-entrypoint.sh']
HOST_ENV = {'PATH': '/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8'}
# Executed in a pinned Linux namespace fd, using host Python and no proxy/redirect.
# Only fixed metadata endpoints are reachable from this probe interface.
PROBE = r'''
import json, sys, urllib.request, urllib.error
class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs): return None
path = sys.argv[1]
assert path in ('/health','/version')
code=None
try:
    opener=urllib.request.build_opener(urllib.request.ProxyHandler({}), NoRedirect())
    try: response=opener.open('http://127.0.0.1:8080'+path, timeout=5)
    except urllib.error.HTTPError as error: response=error
    with response:
        code=response.code; data=response.read(65537)
        assert len(data)<=65536
        value=json.loads(data)
        assert isinstance(value,dict)
        keys={'status','db','message'} if path=='/health' else {'name','version','commit','buildTime'}
        assert set(value)<=keys and all(isinstance(v,str) and len(v)<=256 for v in value.values())
        print(json.dumps({'code':code,'body':value,'cacheControl':response.headers.get('Cache-Control'),'valid':True,'transportUnavailable':False}))
except Exception as error:
    reason=error.reason if isinstance(error,urllib.error.URLError) else error
    unavailable=code is None and isinstance(reason,(TimeoutError,ConnectionRefusedError,ConnectionResetError))
    print(json.dumps({'code':code,'body':{},'cacheControl':None,'valid':False,'transportUnavailable':unavailable}))
'''


def require(condition):
    if not condition: raise ValueError('Isolated application canary boundary rejected')


class Canary:
    def __init__(self, restore, target, directory, image, revision, *, restored_content=None):
        require(re.fullmatch(r'diegueins680/tdf-hq@sha256:[a-f0-9]{64}', image))
        require(re.fullmatch(r'[a-f0-9]{40}', revision))
        require(target.target is not None and target.target != target.source)
        require(re.fullmatch(r'[a-f0-9]{32}', target.nonce))
        self.restore, self.database, self.directory = restore, target, Path(directory)
        self.image, self.revision, self.nonce = image, revision, target.nonce
        self.target, self.creation_attempted, self.paused = None, False, False
        self.image_id = None
        self.restored_content = copy.deepcopy(restored_content)
        self.content_evidence = None
        self.name = 'tdf-audit-canary-'+self.nonce
        require(self.directory == Path('/opt/tdf/backups')/('rehearsal-'+self.nonce))

    def execute(self, args, *, timeout=30):
        result = subprocess.run(self.restore.DOCKER+args, env=HOST_ENV, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, timeout=timeout)
        require(result.returncode == 0 and len(result.stdout) <= 4*1024*1024)
        return result.stdout

    def inspect_database(self):
        self.database.inspect()
        rows = json.loads(self.execute(['inspect', self.database.target]))
        require(len(rows)==1)
        data=rows[0]
        self.database.admit(data)
        require(set(data['NetworkSettings']['Networks']) == {'none'})
        require(data['State']['Running'] and type(data['State']['Pid']) is int and data['State']['Pid'] > 0)
        return data

    def prepare(self):
        owner_guard = getattr(self.database, 'require_application_owner', None)
        if owner_guard is not None:
            owner_guard(self)  # physical copies require registered cleanup order
        self.inspect_database()
        require(self.execute(['ps','--all','--quiet','--filter','label='+LABEL]).strip() == '')
        rows=json.loads(self.execute(['image','inspect', self.image]))
        require(len(rows)==1 and self.image in rows[0]['RepoDigests'])
        require(re.fullmatch(r'sha256:[a-f0-9]{64}', rows[0]['Id']))
        require(not rows[0]['Config'].get('Entrypoint'))
        self.image_id=rows[0]['Id']
        info=self.directory.lstat()
        require(stat.S_ISDIR(info.st_mode) and info.st_uid==0 and info.st_mode & 0o077==0)
        if self.restored_content is None:
            for name in ('canary-assets','canary-uploads'):
                path=self.directory/name
                path.mkdir(mode=0o700)  # exclusive; never reuse pre-existing content
                os.chown(path,1000,1000)
            self.content_evidence = {'mode': 'new-empty-directories'}
        else:
            verifier = getattr(self.database, 'admit_application_content', None)
            require(callable(verifier))
            self.content_evidence = {'mode': 'restored-copy-verified',
                                    'copies': verifier(self, self.restored_content)}

    def command(self):
        require(self.image_id is not None)
        args=['create','--pull=never','--name',self.name,'--label',LABEL+'='+self.nonce,
              '--network=container:'+self.database.target,'--read-only','--user','1000:1000',
              '--workdir','/app','--memory='+str(MEMORY),'--memory-swap='+str(MEMORY),
              '--cpus=0.5','--pids-limit=128','--cap-drop=ALL','--security-opt=no-new-privileges:true',
              '--tmpfs','/tmp:rw,nosuid,nodev,size=16777216']
        for name,destination in [('canary-assets','/data/assets'),('canary-uploads','/app/uploads')]:
            args += ['--mount','type=bind,src='+str(self.directory/name)+',dst='+destination]
        return args+[self.image,*COMMAND]

    def admit(self,data):
        target=data['Id']
        require(re.fullmatch(r'[a-f0-9]{64}',target) and target not in (self.database.source,self.database.target))
        require(self.target is None or target==self.target)
        cfg=data['Config'];host=data['HostConfig']
        require(cfg['Labels'].get(LABEL)==self.nonce and cfg['Image']==self.image and data['Image']==self.image_id)
        require(cfg['Cmd']==COMMAND and not cfg.get('Entrypoint') and cfg['User']=='1000:1000' and cfg['WorkingDir']=='/app')
        require(host['NetworkMode']=='container:'+self.database.target and not data['NetworkSettings']['Networks'])
        require(host['ReadonlyRootfs'] and host['Memory']==MEMORY and host['MemorySwap']==MEMORY)
        require(host['NanoCpus']==500000000 and host['PidsLimit']==128 and host['CapDrop']==['ALL'])
        require(not any(host.get(k) for k in ('Privileged','PortBindings','Devices','CapAdd','VolumesFrom','Binds','PidMode','UTSMode')))
        require(host['IpcMode']=='private' and 'no-new-privileges:true' in host['SecurityOpt'])
        require(host['Tmpfs']=={'/tmp':'rw,nosuid,nodev,size=16777216'})
        mounts={m['Destination']:m for m in data['Mounts']}
        # Docker can omit tmpfs from Mounts; its exact declaration is required
        # independently in HostConfig.Tmpfs. Bind mounts remain mandatory.
        require(len(mounts)==len(data['Mounts']) and set(mounts) in
                ({'/data/assets','/app/uploads'}, {'/data/assets','/app/uploads','/tmp'}))
        for name,destination in [('canary-assets','/data/assets'),('canary-uploads','/app/uploads')]:
            m=mounts[destination]
            require(m['Type']=='bind' and m['Source']==str(self.directory/name) and m['RW'] is True)
        require('/tmp' not in mounts or mounts['/tmp']['Type']=='tmpfs')
        self.target=target
        return data

    def inspect(self):
        require(isinstance(self.target,str) and re.fullmatch(r'[a-f0-9]{64}',self.target))
        rows=json.loads(self.execute(['inspect',self.target]))
        require(len(rows)==1)
        return self.admit(rows[0])

    def probe(self,path):
        require(path in ('/health','/version'))
        db=self.inspect_database();app=self.inspect()
        require(app['State']['Running'])
        # Open namespace handles before probing; never follow a reusable PID path
        # from the child. Also establish application and disposable DB share it.
        fd=os.open('/proc/'+str(db['State']['Pid'])+'/ns/net',os.O_RDONLY)
        try:
            app_fd=os.open('/proc/'+str(app['State']['Pid'])+'/ns/net',os.O_RDONLY)
            try:
                require((os.fstat(fd).st_dev,os.fstat(fd).st_ino)==(os.fstat(app_fd).st_dev,os.fstat(app_fd).st_ino))
            finally: os.close(app_fd)
            result=subprocess.run(['nsenter','--net=/proc/self/fd/'+str(fd),'python3','-c',PROBE,path],
                env=HOST_ENV,pass_fds=(fd,),capture_output=True,text=True,timeout=8)
            require(result.returncode==0 and len(result.stdout)<=4096)
            value=json.loads(result.stdout)
        finally: os.close(fd)
        self.inspect_database(); self.inspect()
        return value

    def await_ready(self):
        for _ in range(45):
            value=self.probe('/health')
            if value['code']==200:
                require(value['valid'] is True and value['body']=={'status':'ok','db':'ok'} and value['cacheControl']=='no-store')
                return value
            require((value['code'] is None and value['transportUnavailable'] is True) or
                    (value['code']==503 and value['valid'] is True))
            time.sleep(1)
        raise ValueError('Isolated application did not become ready')

    def run(self):
        self.prepare()
        self.creation_attempted=True
        self.target=self.execute(self.command()).strip()
        self.inspect()
        self.execute(['start',self.target])
        self.await_ready()
        file_commit=self.execute(['exec',self.target,'cat','/app/COMMIT']).strip()
        require(file_commit==self.revision)
        version=self.probe('/version')
        require(version['code']==200 and version['valid'] is True and version['body'].get('commit')==self.revision)
        binary_hash=self.execute(['exec',self.target,'sha256sum','/app/tdf-hq-exe']).split()[0]
        require(re.fullmatch(r'[a-f0-9]{64}',binary_hash))
        # This mutates only the admitted nonce-owned disposable database process.
        self.inspect_database()
        self.paused=True  # uncertainty set before Docker request
        self.execute(['pause',self.database.target])
        try:
            unavailable=self.probe('/health')
            require((unavailable['code'] is None and unavailable['transportUnavailable'] is True) or
                    (unavailable['code']==503 and unavailable['valid'] is True))
            if unavailable['code']==503:
                require(unavailable['body']=={'status':'degraded','db':'unavailable'} and unavailable['cacheControl']=='no-store')
        finally:
            self.inspect_database()
            self.execute(['unpause',self.database.target])
            self.paused=False
        self.await_ready()
        return {'image':self.image,'imageId':self.image_id,'sourceRevision':self.revision,'binarySha256':binary_hash,
                'content':self.content_evidence,
                'databasePauseProbe':unavailable['code'],'databaseRecovery':'passed',
                'network':'disposable-database-only','productionCredentialsProvided':False,
                'providerConnectivity':False,'scope':'Startup, version and readiness failure/recovery only; not full API conformance.'}

    def cleanup(self):
        # Uncertain creation must retain the enclosing restore reservation until
        # this exact container is admitted and removed. No broad label deletion.
        if self.paused:
            self.inspect_database()
            self.execute(['unpause',self.database.target]);self.paused=False
        if self.target is None and self.creation_attempted:
            rows=json.loads(self.execute(['inspect',self.name],timeout=10))
            require(len(rows)==1);self.admit(rows[0])
        if self.target is not None:
            self.inspect()
            self.execute(['rm','--force',self.target],timeout=10)
            self.target=None;self.creation_attempted=False
