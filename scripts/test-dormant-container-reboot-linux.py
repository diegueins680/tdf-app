#!/usr/bin/env python3
"""Owned-VM Docker29 manual-stop qualification; never use on production.

Caller stages reviewed Docker/runtime binaries and identity.json in ROOT. This
creates a second daemon with an independent data root, socket and containerd.
The primary daemon is only inspected. prepare includes a daemon-restart control;
verify requires an actual host reboot. cleanup removes only fixture containers
and unit; its private runtime/data/evidence directory is retained.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import tempfile
from unittest.mock import patch

ROOT=Path('/opt/tdf/dormant-docker-29-fixture')
UNIT='tdf-dormant-docker-fixture.service'
UNIT_PATH=Path('/etc/systemd/system')/UNIT
SOCKET=ROOT/'docker.sock'
STATE=ROOT/'fixture.json'
DAEMON_CONFIG=ROOT/'daemon-fixture.json'
DAEMON_CONFIG_TEXT='{"features":{"containerd-snapshotter":false}}\n'
REPO=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('dormant',REPO/'ops/hetzner/dormant-container-admission.py')
d=importlib.util.module_from_spec(spec);spec.loader.exec_module(d)
ENV={'PATH':str(ROOT)+':/usr/sbin:/usr/bin:/sbin:/bin','LANG':'C.UTF-8'}
COMMANDS=['--host','unix://'+str(SOCKET)]
UNIT_TEXT='''[Unit]
Description=TDF owned dormant-container qualification fixture
After=network-online.target
Wants=network-online.target
[Service]
Type=notify
Environment=PATH=/opt/tdf/dormant-docker-29-fixture:/usr/sbin:/usr/bin:/sbin:/bin
ExecStart=/opt/tdf/dormant-docker-29-fixture/dockerd --config-file /opt/tdf/dormant-docker-29-fixture/daemon-fixture.json --host unix:///opt/tdf/dormant-docker-29-fixture/docker.sock --data-root /opt/tdf/dormant-docker-29-fixture/data --exec-root /opt/tdf/dormant-docker-29-fixture/exec --pidfile /opt/tdf/dormant-docker-29-fixture/dockerd.pid --containerd-namespace tdf-dormant-fixture --containerd-plugins-namespace tdf-dormant-fixture-plugins --bridge none --iptables=false --ip6tables=false --ip-forward=false --ip-masq=false --userland-proxy=false --storage-driver vfs
TimeoutStartSec=90
TimeoutStopSec=45
KillMode=process
Restart=no
[Install]
WantedBy=multi-user.target
'''


def require(value):
    if not value:raise ValueError('Owned dormant fixture rejected')


def run(args,timeout=60):
    result=subprocess.run(args,capture_output=True,text=True,env=ENV,timeout=timeout)
    if result.returncode:raise ValueError('Owned fixture command failed: '+result.stderr[-2000:])
    return result.stdout.strip()


def docker(*args):return run(['docker',*COMMANDS,*args],timeout=120)


def write(path,value):
    fd=os.open(path,os.O_WRONLY|os.O_CREAT|os.O_EXCL|os.O_NOFOLLOW,0o600)
    with os.fdopen(fd,'w') as handle:
        handle.write(value);handle.flush();os.fsync(handle.fileno())
    fd=os.open(path.parent,os.O_RDONLY|os.O_DIRECTORY)
    try:os.fsync(fd)
    finally:os.close(fd)


def boot():return Path('/proc/sys/kernel/random/boot_id').read_text().strip()


def ownership():
    require(os.geteuid()==0 and sys.platform=='linux')
    machine=Path('/etc/machine-id').read_text().strip()
    require(re.fullmatch('[a-f0-9]{32}',machine) and os.environ.get('TDF_DORMANT_FIXTURE_MACHINE')==machine)
    require(ROOT.is_dir() and not ROOT.is_symlink() and ROOT.stat().st_uid==0 and ROOT.stat().st_mode & 0o077==0)
    identity=json.loads((ROOT/'identity.json').read_text())
    require(set(identity)=={'dockerd','containerd','containerd-shim-runc-v2','runc'})
    for name,digest in identity.items():
        path=ROOT/name
        require(path.is_file() and not path.is_symlink() and path.stat().st_uid==0
                and not path.stat().st_mode & 0o022
                and hashlib.sha256(path.read_bytes()).hexdigest()==digest)
    require('29.1.3' in run([str(ROOT/'dockerd'),'--version']))
    return machine,identity


def inspect(cid):
    require(re.fullmatch('[a-f0-9]{64}',cid))
    rows=json.loads(docker('inspect',cid));require(len(rows)==1 and rows[0]['Id']==cid)
    require(rows[0]['Config']['Labels']=={'tdf.dormant-fixture':'manual-stop-v1'})
    return rows[0]


def dormant(cid):
    value=inspect(cid);metadata=d.read_metadata(ROOT/'data',cid)
    row=d.container_row(value,metadata);d.check_dormant(row)
    require(metadata['manuallyStopped'] is True and metadata['startedBefore'] is True)
    return row


def all_ids():return sorted(docker('ps','--all','--quiet','--no-trunc').split())


def metadata_controls():
    with tempfile.TemporaryDirectory(prefix='metadata-controls-',dir=ROOT) as name:
        root=Path(name);cid='a'*64;parent=root/'containers'/cid;parent.mkdir(parents=True)
        path=parent/'config.v2.json'
        raw=json.dumps({'ID':cid,'HasBeenManuallyStopped':True,'HasBeenStartedBefore':True,
                        'Config':{'Env':['PRIVATE_FIXTURE=never-in-receipt']}})
        path.write_text(raw);os.chmod(path,0o600)
        require('never-in-receipt' not in json.dumps(d.read_metadata(root,cid)))
        original_read=os.read
        changed=False
        def replace_after_read(fd,size):
            nonlocal changed
            data=original_read(fd,size)
            if not changed:
                changed=True;replacement=parent/'replacement';replacement.write_text(raw)
                os.chmod(replacement,0o600);os.replace(replacement,path)
            return data
        try:
            with patch.object(d.os,'read',side_effect=replace_after_read):d.read_metadata(root,cid)
        except ValueError:pass
        else:raise ValueError('Metadata substitution negative control survived')
        path.unlink();path.symlink_to(parent/'missing')
        try:d.read_metadata(root,cid)
        except OSError:pass
        else:raise ValueError('Symlink negative control survived')
        path.unlink();path.write_text(raw);os.chmod(path,0o666)
        try:d.read_metadata(root,cid)
        except ValueError:pass
        else:raise ValueError('Writable metadata negative control survived')


def fresh_roots():
    # Check before starting dockerd: checking ps afterwards is too late if an
    # abandoned data root contains restartable containers from a failed fixture.
    for name in ('data','exec','docker.sock','dockerd.pid'):
        require(not os.path.lexists(ROOT/name))


def restart_control(state):
    require(all_ids()==sorted(state['ids']))
    for cid in state['ids']:dormant(cid)
    run(['systemctl','stop',UNIT])
    require(not SOCKET.exists())
    # Intentionally invalid persisted state on one owned stopped container.
    # This is a known-invalid variant, never a production repair operation.
    path=ROOT/'data/containers'/state['ids'][1]/'config.v2.json'
    original=path.read_bytes();value=json.loads(original)
    require(value['ID']==state['ids'][1] and value['HasBeenManuallyStopped'] is True)
    write(ROOT/'negative-original-config.json',original.decode())
    value['HasBeenManuallyStopped']=False
    with path.open('w') as handle:
        json.dump(value,handle);handle.flush();os.fsync(handle.fileno())
    run(['systemctl','start',UNIT])
    deadline=time.monotonic()+20
    while not inspect(state['ids'][1])['State']['Running'] and time.monotonic()<deadline:time.sleep(.1)
    require(inspect(state['ids'][1])['State']['Running'])
    dormant(state['ids'][0])
    docker('stop','--time','5',state['ids'][1]);dormant(state['ids'][1])
    run(['systemctl','restart',UNIT])
    for cid in state['ids']:dormant(cid)
    snapshot=d.observe(str(SOCKET));require(snapshot['daemon']['sha256']==state['runtime']['dockerd'])
    return {'manualStopSurvivedDaemonRestart':True,'clearedFlagRestartedNegativeControl':True,
            'snapshot':snapshot}


def main():
    machine,identity=ownership();mode=sys.argv[1]
    if mode=='prepare':
        require(not UNIT_PATH.exists() and not STATE.exists())
        fresh_roots()
        metadata_controls()
        require(run(['docker','--host','unix:///var/run/docker.sock','ps','--all','--quiet'])=='')
        write(DAEMON_CONFIG,DAEMON_CONFIG_TEXT)
        write(UNIT_PATH,UNIT_TEXT);os.chmod(UNIT_PATH,0o644)
        run(['systemctl','daemon-reload']);run(['systemctl','enable','--now',UNIT])
        require(docker('info','--format','{{.ServerVersion}}')=='29.1.3' and not all_ids())
        docker('pull','busybox:1.37.0')
        image=docker('image','inspect','busybox:1.37.0','--format','{{.Id}}')
        require(re.fullmatch('sha256:[a-f0-9]{64}',image))
        ids=[]
        for name in ('stays-stopped','negative-restart'):
            cid=docker('run','--detach','--name','tdf-dormant-'+name,'--label','tdf.dormant-fixture=manual-stop-v1',
                       '--network','none','--read-only','--cap-drop','ALL','--security-opt','no-new-privileges',
                       '--restart','unless-stopped',image,'sh','-c','trap "exit 0" TERM; while :; do sleep 1 & wait $!; done')
            require(inspect(cid)['State']['Running']);ids.append(cid)
        state={'machine':machine,'runtime':identity,'boot':boot(),'image':image,'ids':ids,
               'unitSha256':hashlib.sha256(UNIT_TEXT.encode()).hexdigest()}
        write(STATE,json.dumps(state,indent=2)+'\n')
        for cid in ids:docker('stop','--time','5',cid)
        receipt=restart_control(state);write(ROOT/'restart-result.json',json.dumps(receipt,indent=2)+'\n')
        print(json.dumps({'daemonRestart':'PASS','negativeControl':'PASS','actualReboot':'PENDING'}))
    else:
        state=json.loads(STATE.read_text())
        require(state['machine']==machine and state['runtime']==identity and UNIT_PATH.read_text()==UNIT_TEXT
                and DAEMON_CONFIG.read_text()==DAEMON_CONFIG_TEXT)
        require(all_ids()==sorted(state['ids']))
        if mode=='verify':
            require(boot()!=state['boot'] and run(['systemctl','is-active',UNIT])=='active')
            rows=[dormant(cid) for cid in state['ids']]
            snapshot=d.observe(str(SOCKET));require(snapshot['daemon']['sha256']==identity['dockerd'])
            write(ROOT/'reboot-result.json',json.dumps({'actualReboot':'PASS','boot':boot(),
                  'dormantRows':rows,'snapshot':snapshot},indent=2)+'\n')
            print(json.dumps({'actualReboot':'PASS','dormantContainers':len(rows)}))
        elif mode=='cleanup':
            require((ROOT/'reboot-result.json').is_file())
            for cid in state['ids']:dormant(cid);docker('rm',cid)
            require(not all_ids())
            run(['systemctl','disable','--now',UNIT]);require(not SOCKET.exists())
            require(UNIT_PATH.read_text()==UNIT_TEXT);UNIT_PATH.unlink();run(['systemctl','daemon-reload'])
            print(json.dumps({'cleanup':'PASS','privateEvidenceRetained':True}))
        else:raise ValueError('Unknown fixture phase')


if __name__=='__main__':main()
