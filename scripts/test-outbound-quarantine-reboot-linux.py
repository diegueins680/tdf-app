#!/usr/bin/env python3
"""Three-phase real-reboot fixture, exclusively on an acknowledged empty VM.

prepare leaves labelled inert Docker work and a synthetic host receiver installed.
The harness must reboot that same VM, then run verify, then cleanup. No production
entrypoint is exposed, and no provider, credential or customer record is used.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('quarantine_boot',ROOT/'ops/hetzner/outbound-quarantine.py')
q = importlib.util.module_from_spec(spec);spec.loader.exec_module(q)
require = q.require
DOCKER = ['docker','--host','unix:///var/run/docker.sock']
FIXTURE_DIR = Path('/etc/tdf-quarantine-fixture')
RECEIVER = 'tdf-quarantine-fixture.service'
RECEIVER_PATH = Path('/etc/systemd/system')/RECEIVER
LABEL = 'net.tdf.synthetic-quarantine-reboot'
PORT = 31875
SERVER = '''import json,os,socket,threading,time
from pathlib import Path
root=Path('/etc/tdf-quarantine-fixture')
boot=Path('/proc/sys/kernel/random/boot_id').read_text().strip()
s=socket.socket(socket.AF_INET,socket.SOCK_STREAM)
s.setsockopt(socket.SOL_SOCKET,socket.SO_REUSEADDR,1);s.bind(('0.0.0.0',31875));s.listen(16)
with (root/'receiver-boots.jsonl').open('a') as f:
 f.write(json.dumps({'boot':boot,'monotonic':time.monotonic()})+'\\n');f.flush();os.fsync(f.fileno())
notify=socket.socket(socket.AF_UNIX,socket.SOCK_DGRAM)
address=os.environ['NOTIFY_SOCKET']
notify.sendto(b'READY=1', '\\0'+address[1:] if address.startswith('@') else address)
notify.close()
while True:
 c,_=s.accept()
 with (root/'seen.jsonl').open('a') as f:
  f.write(json.dumps({'boot':boot})+'\\n');f.flush();os.fsync(f.fileno())
 c.close()
'''
SERVICE = '''[Unit]
Description=TDF synthetic receiver for owned quarantine reboot test
Before=docker.service tdf-outbound-quarantine.service
[Service]
Type=notify
NotifyAccess=main
ExecStart=/usr/bin/python3 -I /etc/tdf-quarantine-fixture/receiver.py
[Install]
WantedBy=multi-user.target
'''


def run(args): return q.run(args).decode().strip()


def write(path, value):
    fd = os.open(path,os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW,0o600)
    try:
        with os.fdopen(fd,'wb',closefd=False) as f:f.write(value);f.flush();os.fsync(fd)
    finally:os.close(fd)
    parent=os.open(path.parent,os.O_RDONLY|os.O_DIRECTORY)
    try:os.fsync(parent)
    finally:os.close(parent)


def lines(name):
    path=FIXTURE_DIR/name
    return [json.loads(line) for line in path.read_text().splitlines()] if path.exists() else []


def boot():return Path('/proc/sys/kernel/random/boot_id').read_text().strip()


def rejected_start():
    """Restart=always may leave activating/auto-restart after ExecStartPre fails.

    The real daemon must never enter ExecStart. Its monotonic start timestamp
    cannot advance; MainPID must remain zero. Stop the owned retry job afterward.
    """
    before=int(q.properties('docker.service',('ExecMainStartTimestampMonotonic',))['ExecMainStartTimestampMonotonic'])
    result=subprocess.run(['systemctl','start','docker.service'],env=q.ENV,capture_output=True,timeout=45)
    require(result.returncode!=0)
    after=q.properties('docker.service',('MainPID','ExecMainStartTimestampMonotonic'))
    require(after['MainPID']=='0' and int(after['ExecMainStartTimestampMonotonic'])<=before)
    run(['systemctl','stop','docker.service','docker.socket'])


def saved():
    data=json.loads(q.read_private(FIXTURE_DIR/'fixture.json'))
    require(data['machine']==Path('/etc/machine-id').read_text().strip())
    return data


def prepare(machine):
    require(not run(DOCKER+['ps','--all','--quiet']) and not Path('/opt/tdf/production').exists())
    for path in (FIXTURE_DIR,q.DIRECTORY,q.UNIT_PATH,q.DROPIN_PATH,RECEIVER_PATH):require(not path.exists())
    require(not any(row.get('table',{}).get('name')==q.TABLE
                    for row in json.loads(run(['nft','--json','list','tables']))['nftables']))
    image=os.environ.get('TDF_QUARANTINE_TEST_IMAGE','')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}',image))
    run(DOCKER+['image','inspect',image])
    initial_volumes=sorted(run(DOCKER+['volume','ls','--quiet']).split())
    FIXTURE_DIR.mkdir(mode=0o700);q.DIRECTORY.mkdir(mode=0o700)
    nonce=os.urandom(6).hex();name='tdf-quarantine-reboot-'+nonce
    network=run(DOCKER+['network','create','--subnet','172.30.250.0/24','--label',LABEL+'='+nonce,name])
    require(re.fullmatch('[a-f0-9]{64}',network));bridges=['br-'+network[:12]]
    # Publish ownership as soon as each effect is observed. Failed prepare leaves
    # evidence for explicit fixture recovery, never a blanket host cleanup.
    write(FIXTURE_DIR/'network.json',q.canonical({'id':network,'nonce':nonce}))
    write(FIXTURE_DIR/'receiver.py',SERVER.encode());write(RECEIVER_PATH,SERVICE.encode())
    run(['systemctl','daemon-reload']);run(['systemctl','enable','--now',RECEIVER])
    for _ in range(30):
        if lines('receiver-boots.jsonl'):break
        time.sleep(.1)
    require(lines('receiver-boots.jsonl'))
    shell='while :; do printf "attempt\\n"; printf synthetic > /dev/tcp/172.30.250.1/31875 2>/dev/null; sleep 0.2; done'
    container=run(DOCKER+['run','--detach','--pull','never','--restart','unless-stopped','--name',name,
                '--label',LABEL+'='+nonce,'--network',network,'--entrypoint','bash',image,'-c',shell])
    write(FIXTURE_DIR/'container.json',q.canonical({'id':container,'nonce':nonce}))
    time.sleep(2);require(len(lines('seen.jsonl'))>=3)
    guard=(ROOT/'ops/hetzner/outbound-quarantine.py').read_bytes()
    write(q.DIRECTORY/'guard.py',guard)
    write(q.DIRECTORY/'policy.json',q.canonical({'schemaVersion':1,'bridges':bridges,
                'guardSha256':hashlib.sha256(guard).hexdigest()}))
    write(q.UNIT_PATH,q.SERVICE.encode())
    q.DROPIN_PATH.parent.mkdir(exist_ok=True)
    write(q.DROPIN_PATH,q.DROPIN.encode())
    run(['systemctl','daemon-reload']);run(['systemctl','start',q.UNIT])
    evidence=q.observe_persistent()
    time.sleep(1);baseline=len(lines('seen.jsonl'));time.sleep(3)
    require(len(lines('seen.jsonl'))==baseline)
    record={'machine':machine,'boot':boot(),'network':network,'container':container,'nonce':nonce,
            'bridges':bridges,'baselineAccepted':baseline,'policy':evidence,'initialVolumes':initial_volumes}
    write(FIXTURE_DIR/'fixture.json',q.canonical(record))
    print(json.dumps({'prepared':True,'boot':record['boot'],'rebootRequired':True}))


def verify():
    data=saved();require(boot()!=data['boot'])
    current=q.observe_persistent();require(current==data['policy'])
    row=json.loads(run(DOCKER+['inspect',data['container']]))[0]
    require(row['Id']==data['container'] and row['State']['Running']
            and row['Config']['Labels'].get(LABEL)==data['nonce'])
    require(any(item['boot']==boot() for item in lines('receiver-boots.jsonl')))
    time.sleep(4)
    require(len(lines('seen.jsonl'))==data['baselineAccepted'])
    logs=run(DOCKER+['logs','--since',row['State']['StartedAt'],data['container']])
    require(logs.count('attempt')>=1)
    # A corrupted live policy prevents Docker restart. No table repair is done
    # by enforce, and systemd must fail the dependency rather than start dockerd.
    run(['systemctl','stop','docker.service','docker.socket'])
    require(q.properties(q.UNIT,('ActiveState',))['ActiveState']=='active')
    run(['nft','flush','chain','inet',q.TABLE,'host_input'])
    rejected_start()
    require(q.properties(q.UNIT,('ActiveState',))['ActiveState']=='active')
    run(['systemctl','stop',q.UNIT])
    run(['nft','delete','table','inet',q.TABLE])
    run(['systemctl','reset-failed',q.UNIT,'docker.service'])
    run(['systemctl','start',q.UNIT]);run(['systemctl','start','docker.service'])
    require(q.observe_persistent()==data['policy'])
    # Missing persistent configuration also denies the boot/start entrypoint.
    run(['systemctl','stop','docker.service','docker.socket']);run(['systemctl','stop',q.UNIT])
    (q.DIRECTORY/'policy.json').rename(q.DIRECTORY/'policy.saved')
    try:
        rejected_start()
    finally:(q.DIRECTORY/'policy.saved').rename(q.DIRECTORY/'policy.json')
    run(['systemctl','reset-failed',q.UNIT,'docker.service'])
    run(['systemctl','start',q.UNIT]);run(['systemctl','start','docker.service'])
    require(q.observe_persistent()==data['policy'])
    time.sleep(3);require(len(lines('seen.jsonl'))==data['baselineAccepted'])
    print(json.dumps({'realBootChanged':True,'automaticContainerStartupRestricted':True,
        'changedPolicyDeniedDockerStart':True,'missingConfigurationDeniedDockerStart':True,
        'acceptedAfterRestriction':0,'syntheticReceiverOnly':True}))


def cleanup():
    data=saved()
    row=json.loads(run(DOCKER+['inspect',data['container']]))[0]
    require(row['Id']==data['container'] and row['Config']['Labels'].get(LABEL)==data['nonce'])
    run(DOCKER+['rm','--force','--volumes',data['container']])
    row=json.loads(run(DOCKER+['network','inspect',data['network']]))[0]
    require(row['Id']==data['network'] and row['Labels'].get(LABEL)==data['nonce'] and not row['Containers'])
    run(DOCKER+['network','rm',data['network']])
    require(not run(DOCKER+['ps','--all','--quiet']))
    require(sorted(run(DOCKER+['volume','ls','--quiet']).split())==data['initialVolumes'])
    run(['systemctl','disable','--now',RECEIVER])
    # Remove our Docker dependency before stopping the synthetic guard; keep
    # Docker available to subsequent owned fixtures. No production removal API.
    require(q.read_private(q.DROPIN_PATH).decode()==q.DROPIN)
    q.DROPIN_PATH.unlink();run(['systemctl','daemon-reload'])
    run(['systemctl','stop',q.UNIT]);run(['nft','delete','table','inet',q.TABLE])
    for path,content in ((q.UNIT_PATH,q.SERVICE),(RECEIVER_PATH,SERVICE)):
        require(q.read_private(path).decode()==content);path.unlink()
    run(['systemctl','daemon-reload'])
    # Preserve private fixture/guard/config/receiver evidence, outside active paths.
    q.DIRECTORY.rename(FIXTURE_DIR/'retired-policy')
    FIXTURE_DIR.rename(Path('/opt/tdf')/('quarantine-reboot-evidence-'+data['nonce']))
    print(json.dumps({'fixtureRemoved':True,'evidenceRetained':True}))


if __name__=='__main__':
    machine=Path('/etc/machine-id').read_text().strip()
    require(os.geteuid()==0 and re.fullmatch('[a-f0-9]{32}',machine)
            and os.environ.get('TDF_SYNTHETIC_QUARANTINE_HOST')==machine)
    require(len(sys.argv)==2 and sys.argv[1] in ('prepare','verify','cleanup'))
    if sys.argv[1]=='prepare':prepare(machine)
    elif sys.argv[1]=='verify':verify()
    else:cleanup()
