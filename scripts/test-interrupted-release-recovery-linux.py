#!/usr/bin/env python3
"""Two-step real reboot fixture on an explicitly acknowledged, empty owned VM.

prepare creates only nonce-labelled synthetic resources and requests a host reboot.
verify re-admits the same IDs, recovers current synthetic data, then removes only
those owned resources. Never use on production. API/edge behavior is not qualified.
"""
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('abort_linux',ROOT/'ops/hetzner/interrupted-release-recovery.py')
r=importlib.util.module_from_spec(spec);spec.loader.exec_module(r)
D=['docker','--host','unix:///var/run/docker.sock']
LABEL='net.tdf.synthetic-abort'
require=r.require


def run(command,timeout=90):
    p=subprocess.run(command,capture_output=True,text=True,timeout=timeout)
    require(p.returncode==0);return p.stdout.strip()


def inspect(cid):
    rows=json.loads(run(D+['inspect',cid]));require(len(rows)==1);return rows[0]


def stable(row):
    return {k:(sorted(row[k],key=lambda m:m['Destination']) if k=='Mounts' else row[k])
            for k in ('Id','Image','Config','HostConfig','Mounts')}


def ready(cid):
    for _ in range(60):
        p=subprocess.run(D+['exec',cid,'pg_isready','-h','127.0.0.1','-U','postgres','-d','postgres'],capture_output=True,timeout=10)
        if p.returncode==0:return
        time.sleep(0.5)
    require(False)


def sql(cid,query):return run(D+['exec',cid,'psql','-X','-qAt','-U','postgres','-d','postgres','-c',query])


def main():
    require(sys.platform=='linux' and os.geteuid()==0)
    host=r.boot_identity()
    require(os.environ.get('TDF_SYNTHETIC_REBOOT_HOST')==host['machineId'])
    require(len(sys.argv)==3 and sys.argv[1] in ('prepare','verify','cleanup'))
    directory=Path(sys.argv[2])
    require(directory.parent==Path('/opt/tdf') and re.fullmatch('synthetic-abort-[a-f0-9]{32}',directory.name))
    nonce=directory.name.removeprefix('synthetic-abort-')
    if sys.argv[1]=='prepare':
        require(not directory.exists() and not Path('/opt/tdf/production').exists() and not run(D+['ps','-aq']))
        image=os.environ.get('TDF_PHYSICAL_TEST_IMAGE','')
        require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}',image))
        known=json.loads(run(D+['image','inspect',image]));require(len(known)==1 and image in known[0]['RepoDigests'])
        directory.mkdir(mode=0o700);(directory/'journal').mkdir(mode=0o700)
        volume='tdf-synthetic-abort-'+nonce
        require(volume not in run(D+['volume','ls','--format','{{.Name}}']).split())
        run(D+['volume','create','--label',LABEL+'='+nonce,volume])
        targets={}
        for service in ('db','api'):
            cmd=D+['create','--name','tdf-abort-'+service+'-'+nonce,'--label',LABEL+'='+nonce,
                '--restart=unless-stopped','--network=none','--memory=268435456','--memory-swap=268435456',
                '--pids-limit=64','--cpus=0.5']
            if service=='db':cmd+=['--mount','type=volume,source='+volume+',target=/var/lib/postgresql/data',
                '--env','POSTGRES_HOST_AUTH_METHOD=trust','--env','POSTGRES_INITDB_ARGS=--encoding=UTF8',image]
            else:cmd+=['--stop-signal=SIGTERM','--entrypoint=/bin/sh',image,'-c',
                "trap 'touch /tmp/stop-received; sleep 60; exit 0' TERM; touch /tmp/ready; while :; do sleep 1; done"]
            cid=run(cmd);targets[service]=cid;run(D+['start',cid])
        ready(targets['db'])
        sql(targets['db'],"CREATE TABLE recovery_history(id integer primary key, value text); INSERT INTO recovery_history VALUES(159,'current-original-data');")
        run(D+['exec',targets['api'],'sh','-c','mkdir -p /app/uploads; printf preserved-upload > /app/uploads/sentinel'])
        original={s:stable(inspect(cid)) for s,cid in targets.items()}
        timer='tdf-synthetic-abort-'+nonce+'.timer';service=timer.replace('.timer','.service')
        units={service:'[Service]\nType=oneshot\nExecStart=/usr/bin/true\n',timer:
            '[Timer]\nOnBootSec=1h\nUnit='+service+'\n[Install]\nWantedBy=timers.target\n'}
        for name,content in units.items():
            path=Path('/etc/systemd/system')/name;require(not path.exists());path.write_text(content);path.chmod(0o644)
        run(['systemctl','daemon-reload']);run(['systemctl','enable','--now',timer])
        plan={k:('sha256:'+'a'*64 if k.endswith('Image') else 'a'*(40 if k.endswith('Revision') else 64)) for k in r.j.PLAN_KEYS}
        admission={'schemaVersion':1,'releaseNonce':nonce,'planHash':r.sha(r.canonical(plan)),
            'host':host,'originalDeployment':{'containers':original,'volume':volume,'units':units}}
        with r.j.files.directory(str(directory),private=True) as fd:r.publish(fd,'admission.json',admission)
        with r.j.open_journal(str(directory/'journal')) as q:
            q.initialize(plan,nonce)
            def lost(c):
                # The client is killed after actual container signal delivery; the
                # daemon request remains uncancelled. Reboot removes that daemon.
                run(['systemctl','stop',timer])
                child=subprocess.Popen(D+['stop','--timeout','300',targets['api']],stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
                try:
                    for _ in range(40):
                        signal=subprocess.run(D+['exec',targets['api'],'test','-f','/tmp/stop-received'],capture_output=True,timeout=5)
                        if signal.returncode==0:break
                        time.sleep(0.1)
                    require(signal.returncode==0 and child.poll() is None)
                finally:
                    child.kill();child.wait(timeout=10)
                raise RuntimeError('synthetic lost stop response')
            try:q.perform('maintenance','c'*64,lost)
            except RuntimeError:pass
        with r.open_abort(str(directory/'journal')) as q:
            q.latch(admission,r.sha(r.canonical(admission)))
            q.request_reboot(lambda:run(['systemctl','reboot']))
        return
    with r.j.files.directory(str(directory),private=True) as fd:
        raw,_=r.read_file(fd,'admission.json',maximum=r.MAX_ADMISSION)
    admission=json.loads(raw);require(admission['releaseNonce']==nonce)
    original=admission['originalDeployment'];targets={s:row['Id'] for s,row in original['containers'].items()}
    result=None
    if sys.argv[1]=='verify':
        with r.open_abort(str(directory/'journal')) as q:
            result=q.observe_new_boot()
            require(set(run(D+['ps','-aq','--no-trunc']).split())==set(targets.values()))
            for s,cid in targets.items():
                row=inspect(cid);require(row['Config']['Labels'][LABEL]==nonce and stable(row)==original['containers'][s])
            timer=next(n for n in original['units'] if n.endswith('.timer'))
            require(run(['systemctl','is-active',timer])=='active')
            # Preserve originals in place. Starting this synthetic original DB can write;
            # the abort intent already records that possibility before reboot.
            for s in ('db','api'):
                if not inspect(targets[s])['State']['Running']:run(D+['start',targets[s]])
                if s=='db':ready(targets[s])
            require(sql(targets['db'],'SELECT id::text || value FROM recovery_history ORDER BY id;')=='159current-original-data')
            require(run(D+['exec',targets['api'],'cat','/app/uploads/sentinel'])=='preserved-upload')
            result.update({'syntheticCurrentDatabasePreserved':True,'syntheticWritableLayerPreserved':True,
                'originalContainerIdsPreserved':True,'enabledTimerResumedAtBoot':True,
                'limitations':['Synthetic PG17/inert API; no production effect or actual backend/edge qualification.',
                    'This fixture performs identity-checked synthetic restart; production restart adapter remains unimplemented.']})
            with r.j.files.directory(str(directory),private=True) as fd:r.publish(fd,'result.json',result)
    # Identity-bound cleanup only after the fixture assertions pass.
    for name,content in original['units'].items():
        path=Path('/etc/systemd/system')/name;require(path.read_text()==content)
        if name.endswith('.timer'):run(['systemctl','disable','--now',name])
        path.unlink()
    run(['systemctl','daemon-reload'])
    for cid in reversed(list(targets.values())):
        require(inspect(cid)['Config']['Labels'][LABEL]==nonce);run(D+['rm','--force','--volumes',cid])
    volume=original['volume'];rows=json.loads(run(D+['volume','inspect',volume]))
    require(len(rows)==1 and rows[0]['Labels'][LABEL]==nonce);run(D+['volume','rm',volume])
    require(not run(D+['ps','-aq']))
    print(json.dumps(result if result is not None else {'ownedSyntheticResourcesRemoved':True,'rebootVerified':False},sort_keys=True))


if __name__=='__main__':main()
