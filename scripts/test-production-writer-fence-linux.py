#!/usr/bin/env python3
"""Real systemd/Docker shutdown of an exclusively owned synthetic Linux host.

Requires an explicit machine-id acknowledgement, empty Docker inventory and no
canonical production directory, networks, volumes or TDF units. Never invoke on
production. API and edge use inert shell workloads; PostgreSQL is real PG17.
"""
import hashlib
from contextlib import nullcontext
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import stat
import sys
import time
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, ROOT/'ops/hetzner'/filename)
    value = importlib.util.module_from_spec(spec); spec.loader.exec_module(value)
    return value


w = load('linux_writer_fence', 'production-writer-fence.py')
j = load('linux_fence_journal', 'release-journal.py')
a = load('linux_original_admission', 'original-deployment-admission.py')
s = load('linux_fence_storage', 'stopped-application-storage.py')
p = load('linux_fence_physical', 'physical-postgres-recovery.py')
require = w.require
DOCKER = w.sources.inspector.DOCKER
LABEL = 'net.tdf.synthetic-writer-fence'
DIRECTORY = Path('/opt/tdf/production')
SERVICE = '[Unit]\nDescription=TDF synthetic fence fixture only\n[Service]\nType=oneshot\nExecStart=/usr/bin/true\n'
TIMER = '[Unit]\nDescription=TDF synthetic fence fixture only\n[Timer]\nOnActiveSec=1d\nUnit=tdf-postgres-backup.service\n[Install]\nWantedBy=timers.target\n'
SHELL = "mkdir -p /app/uploads; printf synthetic-upload > /app/uploads/sentinel; chmod 600 /app/uploads/sentinel; touch /tmp/ready; trap 'exit 0' TERM; while :; do sleep 1; done"


def run(command, *, timeout=90):
    result = subprocess.run(command, env=w.ENV, text=True, capture_output=True, timeout=timeout)
    require(result.returncode == 0 and len(result.stdout) <= 4*1024**2)
    return result.stdout.strip()


def inspect(target):
    values = json.loads(run(DOCKER+['inspect', target]))
    require(len(values) == 1)
    return values[0]


def exclusive_file(path, content):
    with path.open('x') as output:
        output.write(content)
    path.chmod(0o644)


def preserve_owned_directory(directory, destination, owned):
    if owned is None: return
    current = directory.lstat()
    require(stat.S_ISDIR(current.st_mode) and not directory.is_symlink()
            and (current.st_dev, current.st_ino) == owned
            and current.st_uid == os.geteuid() and stat.S_IMODE(current.st_mode) == 0o700
            and not os.path.lexists(destination))
    directory.rename(destination)


class SyntheticTLS:
    """Nonce-owned CA only on the acknowledged empty fixture host, never production."""
    def __init__(self,archive,volume,nonce):
        self.directory=archive/'synthetic-tls';self.directory.mkdir(mode=0o700)
        self.trust=Path('/usr/local/share/ca-certificates')/('tdf-synthetic-'+nonce+'.crt')
        require(not os.path.lexists(self.trust));self.trust_identity=None
        row=json.loads(run(DOCKER+['volume','inspect',volume]))
        require(len(row)==1 and row[0]['Labels'].get(LABEL)==nonce)
        self.destination=Path(row[0]['Mountpoint'])/'synthetic-tls'
        self.destination.mkdir(mode=0o700)
        directory=self.directory
        run(['openssl','req','-x509','-newkey','rsa:2048','-nodes','-days','1',
             '-keyout',str(directory/'ca.key'),'-out',str(directory/'ca.crt'),
             '-subj','/CN=TDF synthetic '+nonce,'-addext','basicConstraints=critical,CA:TRUE',
             '-addext','keyUsage=critical,keyCertSign,cRLSign'])
        for prefix,hostname in (('valid','api.tdfrecords.net'),('wrong','wrong.invalid')):
            run(['openssl','req','-newkey','rsa:2048','-nodes','-keyout',str(directory/(prefix+'.key')),
                 '-out',str(directory/(prefix+'.csr')),'-subj','/CN='+hostname])
            (directory/(prefix+'.ext')).write_text('subjectAltName=DNS:'+hostname+'\nbasicConstraints=CA:FALSE\nkeyUsage=digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\n')
            run(['openssl','x509','-req','-in',str(directory/(prefix+'.csr')),'-CA',str(directory/'ca.crt'),
                 '-CAkey',str(directory/'ca.key'),'-CAcreateserial','-days','1',
                 '-out',str(directory/(prefix+'.crt')),'-extfile',str(directory/(prefix+'.ext'))])
        for key in directory.glob('*.key'):key.chmod(0o600)
        self.select('valid')

    def select(self,prefix):
        require(prefix in ('valid','wrong'))
        for suffix in ('crt','key'):
            source=self.directory/(prefix+'.'+suffix);destination=self.destination/('server.'+suffix)
            shutil.copyfile(source,destination);destination.chmod(0o600 if suffix=='key' else 0o644)

    def verify(self,cid):
        application=load('linux_edge_tls_probe','original-application-recovery.py')
        def sample():
            row=inspect(cid)
            saved={'expected':{'edge':{'containerId':cid}},'containers':{'edge':{
                key:(sorted(row[key],key=lambda m:m['Destination']) if key=='Mounts' else row[key])
                for key in ('Id','Image','Config','HostConfig','Mounts')}}}
            for _ in range(60):
                value=application.probe(saved,'/health','edge')
                if not value['transportUnavailable']:return value
                time.sleep(0.25)
            require(False)
        invalid=sample()
        require(invalid=={'code':None,'valid':False,'metadata':{},'transportUnavailable':False})
        content=(self.directory/'ca.crt').read_bytes()
        with self.trust.open('xb') as stream:stream.write(content)
        self.trust.chmod(0o644)
        st=self.trust.lstat();self.trust_identity=(st.st_dev,st.st_ino,hashlib.sha256(content).hexdigest())
        run(['update-ca-certificates'])
        run(DOCKER+['stop','--time','10',cid]);self.select('wrong');run(DOCKER+['start',cid])
        invalid=sample()
        require(invalid=={'code':None,'valid':False,'metadata':{},'transportUnavailable':False})
        run(DOCKER+['stop','--time','10',cid]);self.select('valid');run(DOCKER+['start',cid])
        valid=sample();require(valid['valid'] and valid['code']==200)

    def cleanup(self):
        if self.trust_identity is not None:
            st=self.trust.lstat()
            require(stat.S_ISREG(st.st_mode) and not self.trust.is_symlink() and st.st_uid==os.geteuid()
                    and (st.st_dev,st.st_ino,hashlib.sha256(self.trust.read_bytes()).hexdigest())==self.trust_identity)
            self.trust.unlink();run(['update-ca-certificates'])
            require(not os.path.lexists(self.trust))


def cleanup_preserving_tls(resource_cleanup,tls):
    try:resource_cleanup()
    finally:
        if tls is not None:tls.cleanup()


def exercise_database_recovery(archive, saved, db, actual_application=False, actual_edge=False):
    terminal_verified=False
    """Real PostgreSQL/Docker/files; synthetic boot epoch, explicitly no reboot."""
    recovery=load('linux_original_db_recovery','original-database-recovery.py')
    service=load('linux_original_db_journal','abort-service-journal.py')
    # Fixture-only crash setup after the already-qualified shutdown/capture.
    run(DOCKER+['start',db])
    for _ in range(60):
        probe=subprocess.run(DOCKER+['exec',db,'pg_isready','-U','postgres','-d','tdf_hq'],capture_output=True,timeout=10)
        if probe.returncode==0:break
        time.sleep(0.25)
    require(probe.returncode==0)
    run(DOCKER+['exec',db,'psql','-X','-qAt','-U','postgres','-d','tdf_hq','-c',
        "CREATE TABLE abort_committed_data(value text NOT NULL); INSERT INTO abort_committed_data VALUES ('preserved-after-kill');"])
    run(DOCKER+['kill','--signal=KILL',db])
    require(inspect(db)['State']['ExitCode']==137 and not inspect(db)['State']['Running'])
    root=Path(saved['originalDeployment']['roots']['database']['path'])
    version=root/'PG_VERSION';held=root/'PG_VERSION.synthetic-held'
    require(not held.exists());version.rename(held)
    rejected=[]
    try:
        for kind in ('missing','empty','symlink','wrong-major'):
            if kind=='empty':version.touch();os.chown(version,999,999)
            if kind=='symlink':version.symlink_to(held)
            if kind=='wrong-major':version.write_bytes(b'16\n');os.chown(version,999,999)
            try:recovery.cluster_present(root)
            except (ValueError,OSError):rejected.append(kind)
            else:require(False)
            if os.path.lexists(version):version.unlink()
    finally:
        if os.path.lexists(version):version.unlink()
        held.rename(version)
    require(len(rejected)==4)
    require(recovery.cluster_present(root)['existingClusterRequired'])
    with service.a.open_abort(archive/'journal') as abort:
        abort.latch(saved,service.a.sha(service.a.canonical(saved)))
        abort.request_reboot(lambda:None)  # Deliberate synthetic boot protocol only.
        host=service.a.boot_identity();synthetic={**host,'bootId':'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa'}
        require(synthetic['bootId']!=host['bootId'])
        original_execute=recovery.o.fence.execute
        start_commands=[];allow_start=False
        def traced_execute(command):
            if command[:len(DOCKER)+1]==DOCKER+['start']:
                start_commands.append(list(command))
                require(allow_start)  # Trace/reject before dispatch in negative case.
            return original_execute(command)
        with patch.object(service.a,'boot_identity',return_value=synthetic),patch.object(recovery.o.fence,'execute',side_effect=traced_execute),recovery.recovery_reservation() as reservation:
            journal=service.ServiceJournal(abort);journal.begin_epoch()
            absence=load('linux_original_absence','original-disposable-absence.py')
            absence_reservation=absence.d.Reservation(reservation.directory,reservation.descriptor)
            absence_adapter=absence.OriginalDisposableAbsence(journal,archive,absence_reservation)
            absence_state=absence_adapter.recover()
            require(absence_state['completedStages']==['remove-disposables'])
            adapter=recovery.OriginalDatabase(journal,archive,reservation)
            version.rename(held)
            try:
                refused=False
                try:adapter.recover()
                except (ValueError,OSError):refused=True
                require(refused and not start_commands and not inspect(db)['State']['Running'])
            finally:held.rename(version)
            # Repairing a fixture file never erases an uncertain stage. A second
            # recorded synthetic boot epoch is required before any new attempt.
            replay_refused=False
            try:adapter.recover()
            except ValueError:replay_refused=True
            require(replay_refused and not start_commands and not inspect(db)['State']['Running'])
            journal.request_next_reboot(lambda:None)
            synthetic['bootId']='bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb'
            journal.begin_epoch()
            require(absence_adapter.recover()['completedStages']==['remove-disposables'])
            allow_start=True
            status=adapter.recover()
            require(start_commands==[DOCKER+['start',db]])
            require(status['completedStages']==['remove-disposables','recover-db'])
            denied=False
            try:adapter.recover()
            except ValueError:denied=True
            require(denied)
            if actual_application:
                application=load('linux_original_application','original-application-recovery.py')
                # Same real lock descriptor; each independently loaded library
                # owns its own typed wrapper, never a separate lock acquisition.
                app_reservation=application.d.Reservation(reservation.directory,reservation.descriptor)
                app_adapter=application.OriginalApplication(journal,archive,app_reservation)
                app_status=app_adapter.recover()
                require(app_status['completedStages']==['remove-disposables','recover-db','recover-api'])
                require(application.probe(saved['originalDeployment'],'/rooms/public')['valid'])
                if actual_edge:
                    edge=load('linux_original_edge','original-edge-recovery.py')
                    edge_reservation=edge.a.d.Reservation(reservation.directory,reservation.descriptor)
                    edge_status=edge.OriginalEdge(journal,archive,edge_reservation).recover()
                    require(edge_status['completedStages']==['remove-disposables','recover-db','recover-api','recover-edge'])
                    require(edge.a.probe(saved['originalDeployment'],'/version','edge')['valid'])
                    if os.environ.get('TDF_TEST_ORIGINAL_TIMER_RECOVERY')=='1':
                        timer=load('linux_original_timer','original-timer-recovery.py')
                        timer_reservation=timer.d.Reservation(reservation.directory,reservation.descriptor)
                        timer_status=timer.OriginalTimer(journal,archive,timer_reservation).recover()
                        require(timer_status['completedStages']==['remove-disposables','recover-db','recover-api','recover-edge','restore-timer'])
                        require(timer.d.observe(saved['originalDeployment'])['units']['timerStopped'] is False)
                        require(not timer_status['originalDeploymentRecoverySequenceComplete'])
                        if os.environ.get('TDF_TEST_ORIGINAL_RECOVERY_COMPLETION')=='1':
                            completion=load('linux_original_completion','original-recovery-completion.py')
                            final_reservation=completion.a.d.Reservation(reservation.directory,reservation.descriptor)
                            final_status=completion.OriginalRecoveryCompletion(journal,archive,final_reservation).recover()
                            require(final_status['originalDeploymentRecoverySequenceComplete']
                                    and final_status['releaseContinuationAllowed'] is False)
                            terminal_verified=final_status['originalDeploymentRecoverySequenceComplete']
    require(run(DOCKER+['exec',db,'psql','-X','-qAt','-U','postgres','-d','tdf_hq','-c',
                'SELECT value FROM abort_committed_data;'])=='preserved-after-kill')
    require(recovery.o.database_identity(db)==saved['originalDeployment']['database'])
    return {'actualPostgresExit137Recovered':True,'committedDataPreserved':True,
            'emptyDisposableSetAdmitted':True,'disposableRemovalExercised':False,
            'terminalRecoverySequenceComplete':terminal_verified,
            'missingClusterControls':rejected,'sameEpochDuplicateDenied':True,'missingClusterDeniedBeforeActualStart':True,
            'secondRecordedSyntheticEpochRequired':True,
            'bootEpoch':'synthetic; no actual reboot in this component fixture',
            'actualServiceAdapter':'original database, backend and TLS edge' if actual_edge else 'original database and real backend' if actual_application else 'original database only; API/edge remain inert and stopped'}


def validate_recovery_modes():
    stages=('DB','APPLICATION','EDGE','TIMER','COMPLETION')
    flags=[os.environ.get('TDF_TEST_ORIGINAL_'+name+'_RECOVERY')=='1' for name in stages[:-1]]
    flags.append(os.environ.get('TDF_TEST_ORIGINAL_RECOVERY_COMPLETION')=='1')
    require(all(not flags[i] or flags[i-1] for i in range(1,len(flags))))


def main():
    validate_recovery_modes()  # Reject skipped requested stages before any fixture effect.
    require(sys.platform == 'linux' and os.geteuid() == 0)
    machine = Path('/etc/machine-id').read_text().strip()
    require(re.fullmatch('[a-f0-9]{32}', machine)
            and os.environ.get('TDF_SYNTHETIC_WRITER_FENCE_HOST') == machine)
    image = os.environ.get('TDF_PHYSICAL_TEST_IMAGE', '')
    inert = os.environ.get('TDF_CANARY_TEST_IMAGE', '')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}', image)
            and re.fullmatch(r'diegueins680/tdf-hq@sha256:[a-f0-9]{64}', inert))
    for reference in (image, inert):
        row = json.loads(run(DOCKER+['image', 'inspect', reference]))
        require(len(row) == 1 and reference in row[0]['RepoDigests'])
    # These independent absence checks run before any fixture mutation.
    require(not os.path.lexists(DIRECTORY) and not run(DOCKER+['ps', '--all', '--quiet']))
    networks = ('tdf-production_database', 'tdf-production_outbound')
    volumes = tuple(w.sources.VOLUMES.values())
    require(not set(run(DOCKER+['network', 'ls', '--format', '{{.Name}}']).split()) & set(networks))
    require(not set(run(DOCKER+['volume', 'ls', '--format', '{{.Name}}']).split()) & set(volumes))
    for operation in ('list-units', 'list-unit-files'):
        empty = subprocess.run(['systemctl', operation, '--all', '--plain', '--no-legend', '--no-pager', 'tdf*'],
                               env=w.ENV, text=True, capture_output=True, timeout=30)
        # systemd list-unit-files returns1 for an empty pattern match.
        require(empty.returncode in (0, 1) and not empty.stdout.strip() and not empty.stderr.strip())
    for name in w.UNITS:
        require(not os.path.lexists(w.DIRECTORY/name))
    require(Path('/opt/tdf').is_dir())
    actual_application=os.environ.get('TDF_TEST_ORIGINAL_APPLICATION_RECOVERY')=='1'
    if actual_application:require(os.environ.get('TDF_TEST_ORIGINAL_DB_RECOVERY')=='1')
    actual_edge=os.environ.get('TDF_TEST_ORIGINAL_EDGE_RECOVERY')=='1'
    if actual_edge:require(actual_application)
    if os.environ.get('TDF_TEST_ORIGINAL_TIMER_RECOVERY')=='1':require(actual_edge)
    edge_image='caddy@sha256:6aeddd44c3078b0f9a35206472a11420648a79c184603ef95957d0a20044cb2b'
    if actual_edge:require(edge_image in json.loads(run(DOCKER+['image','inspect',edge_image]))[0]['RepoDigests'])
    app_environment={}
    nonce = os.urandom(16).hex()
    archive = Path('/opt/tdf')/('synthetic-fence-'+nonce)
    archive.mkdir(mode=0o700)
    (archive/'journal').mkdir(mode=0o700)
    created_containers, created_networks, created_volumes, created_units = {}, [], [], []
    hashes = {w.SERVICE: hashlib.sha256(SERVICE.encode()).hexdigest(),
              w.TIMER: hashlib.sha256(TIMER.encode()).hexdigest()}
    evidence = None
    before = {}
    owned_production = None
    tls=None
    try:
        DIRECTORY.mkdir(mode=0o700)
        created = DIRECTORY.lstat()
        owned_production = (created.st_dev, created.st_ino)
        (DIRECTORY/'assets').mkdir(mode=0o700)
        if actual_application:
            os.chown(DIRECTORY/'assets',1000,1000)
            (DIRECTORY/'uploads').mkdir(mode=0o700);os.chown(DIRECTORY/'uploads',1000,1000)
        exclusive_file(DIRECTORY/'postgres_password', 'synthetic-fixture-only\n')
        exclusive_file(DIRECTORY/'Caddyfile', ('{\n auto_https off\n}\nhttps://api.tdfrecords.net {\n tls /data/synthetic-tls/server.crt /data/synthetic-tls/server.key\n reverse_proxy tdf-synthetic-fence-api-'+nonce+':8080\n}\n') if actual_edge else '# Inert fixture; not an actual edge configuration\n')
        exclusive_file(DIRECTORY/'compose.yaml', '# Synthetic Docker inventory only\n')
        for network in networks:
            run(DOCKER+['network', 'create', '--internal', '--label', LABEL+'='+nonce, network])
            created_networks.append(network)
        for volume in volumes:
            run(DOCKER+['volume', 'create', '--label', LABEL+'='+nonce, volume])
            created_volumes.append(volume)
        if actual_edge:tls=SyntheticTLS(archive,volumes[1],nonce)
        for service in ('db', 'api', 'edge'):
            reference = image if service == 'db' else edge_image if actual_edge and service=='edge' else inert
            command = DOCKER+['create', '--pull=never', '--name', 'tdf-synthetic-fence-'+service+'-'+nonce,
                '--label', LABEL+'='+nonce, '--label', 'com.docker.compose.project=tdf-production',
                '--label', 'com.docker.compose.service='+service,
                '--label', 'com.docker.compose.project.working_dir='+str(DIRECTORY),
                '--label', 'com.docker.compose.project.config_files='+str(DIRECTORY/'compose.yaml'),
                '--restart=unless-stopped', '--network', networks[1 if service == 'edge' else 0],
                '--memory=268435456', '--memory-swap=268435456', '--cpus=0.5', '--pids-limit=64']
            if actual_application and service=='api':
                command[command.index('--memory=268435456')]='--memory=536870912'
                command[command.index('--memory-swap=268435456')]='--memory-swap=536870912'
                command[command.index('--pids-limit=64')]='--pids-limit=128'
            if service == 'db':
                command += ['--mount', 'type=volume,source='+volumes[0]+',target=/var/lib/postgresql/data',
                    '--mount', 'type=bind,source='+str(DIRECTORY/'postgres_password')+',target=/run/secrets/postgres_password,readonly',
                    '--env', 'POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password', '--env', 'POSTGRES_DB=tdf_hq',
                    '--env', 'POSTGRES_INITDB_ARGS=--encoding=UTF8', reference]
            elif actual_application and service=='api':
                command += ['--user=1000:1000','--mount','type=bind,source='+str(DIRECTORY/'assets')+',target=/data/assets',
                            '--mount','type=bind,source='+str(DIRECTORY/'uploads')+',target=/app/uploads']
                for key,value in app_environment.items():command += ['--env',key+'='+value]
                command += [reference]
            elif actual_edge and service=='edge':
                command += ['--mount','type=bind,source='+str(DIRECTORY/'Caddyfile')+',target=/etc/caddy/Caddyfile,readonly',
                            '--mount','type=volume,source='+volumes[1]+',target=/data',
                            '--mount','type=volume,source='+volumes[2]+',target=/config',reference]
            else:
                command += ['--user=0:0', '--entrypoint=/bin/sh', '--stop-signal=SIGTERM']
                if service == 'api':
                    command += ['--mount', 'type=bind,source='+str(DIRECTORY/'assets')+',target=/data/assets']
                else:
                    command += ['--mount', 'type=bind,source='+str(DIRECTORY/'Caddyfile')+',target=/etc/caddy/Caddyfile,readonly',
                        '--mount', 'type=volume,source='+volumes[1]+',target=/data',
                        '--mount', 'type=volume,source='+volumes[2]+',target=/config']
                command += [reference, '-c', SHELL]
            # Record nonce name before creation so a lost response retains evidence.
            name = command[command.index('--name')+1]
            created_containers[service] = name
            target = run(command)
            value = inspect(target)
            require(value['Config']['Labels'][LABEL] == nonce and value['Name'] == '/'+name)
            created_containers[service] = target
            if service == 'api': run(DOCKER+['network', 'connect', networks[1], target])
            run(DOCKER+['start', target])
            if actual_application and service=='db':
                run(DOCKER+['exec',target,'sh','-c',
                    'for n in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do pg_isready -h 127.0.0.1 -U postgres -d tdf_hq && exit 0; sleep 1; done; exit 1'])
                def fixture_sql(text):
                    out=subprocess.run(DOCKER+['exec','-i',target,'psql','-X','-qAt','-v','ON_ERROR_STOP=1','-U','postgres','-d','tdf_hq'],input=text,text=True,capture_output=True,timeout=180)
                    if out.returncode:print('Owned synthetic SQL failure: '+out.stderr[-4096:],file=sys.stderr)
                    require(out.returncode==0);return out.stdout.strip()
                for name in ('production-schema-20260814.sql','catalog-production-source-fixture.sql'):
                    fixture_sql((ROOT/'scripts/__tests__/fixtures'/name).read_text())
                fixture_sql((ROOT/'synthetic-fixture-migrations.sql').read_text())
                canary=load('linux_fixture_canary','isolated-application-canary.py')
                app_environment={**canary.ENVIRONMENT,'DB_HOST':inspect(target)['Name'].removeprefix('/'),'DB_PASS':'synthetic-fixture-only','OPERATIONS_WORKER_ENABLED':'false',
                    **canary.regional_environment(json.loads(fixture_sql(canary.REGIONAL_SQL)))}
        db = created_containers['db']
        for _ in range(60):
            probe = subprocess.run(DOCKER+['exec', db, 'pg_isready', '-U', 'postgres', '-d', 'tdf_hq'],
                                   env=w.ENV, capture_output=True, timeout=10)
            if probe.returncode == 0: break
            time.sleep(0.5)
        require(probe.returncode == 0)
        # Initializing entrypoint's temporary server can answer pg_isready; require
        # the final entrypoint process and its actual TCP listener as well.
        run(DOCKER+['exec', db, 'sh', '-c',
            'for n in 1 2 3 4 5 6 7 8 9 10; do pg_isready -h 127.0.0.1 -U postgres -d tdf_hq && exit 0; sleep 1; done; exit 1'])
        system_id = run(DOCKER+['exec', db, 'psql', '-X', '-qAt', '-U', 'postgres', '-d', 'tdf_hq',
                                  '-c', 'SELECT system_identifier FROM pg_control_system();'])
        run(DOCKER+['exec', db, 'psql', '-X', '-qAt', '-U', 'postgres', '-d', 'tdf_hq', '-c',
            'CREATE TABLE IF NOT EXISTS public.tdf_schema_migration (migration_id text PRIMARY KEY, checksum text NOT NULL, source_commit text NOT NULL)'])
        for service in ('api', 'edge'):
            if actual_application and service=='api':
                application=load('linux_application_probe','original-application-recovery.py')
                row=inspect(created_containers['api'])
                probe_saved={'expected':{'api':{'containerId':row['Id']}},'containers':{'api':{key:(sorted(row[key],key=lambda m:m['Destination']) if key=='Mounts' else row[key]) for key in ('Id','Image','Config','HostConfig','Mounts')}}}
                for _ in range(90):
                    try:ready=application.probe(probe_saved,'/health')['valid']
                    except ValueError:ready=False
                    if ready:break
                    time.sleep(0.5)
                if not ready:print(run(DOCKER+['logs','--tail','35',row['Id']]),file=sys.stderr)
                require(ready)
                run(DOCKER+['exec',row['Id'],'sh','-c','mkdir -p /app/uploads; printf synthetic-upload > /app/uploads/sentinel; chmod 600 /app/uploads/sentinel'])
                continue
            if actual_edge and service=='edge':
                tls.verify(created_containers['edge'])
                continue
            run(DOCKER+['exec', created_containers[service], 'sh', '-c',
                'for n in 1 2 3 4 5; do test -f /tmp/ready && exit 0; sleep 1; done; exit 1'])
        for name, content in ((w.SERVICE, SERVICE), (w.TIMER, TIMER)):
            exclusive_file(w.DIRECTORY/name, content); created_units.append(name)
        run(['systemctl', 'daemon-reload'])
        run(['systemctl', 'enable', '--now', w.TIMER])
        expected = {}
        for service, target in created_containers.items():
            value = inspect(target)
            expected[service] = {'containerId': target, 'image': value['Config']['Image'], 'imageId': value['Image']}
        admitted = w.sources.observe(expected)
        before = {service: {key: inspect(target)[key] for key in ('Config','HostConfig','Mounts')}
                  for service, target in created_containers.items()}
        plan = {key: ('sha256:'+'1'*64 if key.endswith('Image') else '1'*(40 if key.endswith('Revision') else 64))
                for key in j.PLAN_KEYS}
        plan['runtimeHash'] = admitted['runtimeConfigurationSha256']
        source = None if actual_application else s.RetainedRoot(**dict(target=expected['api']['containerId'],
            image=expected['api']['image'], image_id=expected['api']['imageId']))
        with j.open_journal(str(archive/'journal')) as journal, (source.pinned() if source else nullcontext()):
            journal.initialize(plan, nonce)
            # Docker's source string stays unchanged when its host path is
            # replaced. The live mount still references the original directory.
            assets = DIRECTORY/'assets'; held_assets = DIRECTORY/'assets-held'
            assets.rename(held_assets); assets.mkdir(mode=0o700)
            rejected_replacement = False
            try:
                try: a.bind_directory_identities(inspect(expected['api']['containerId']))
                except ValueError: rejected_replacement = True
            finally:
                assets.rmdir(); held_assets.rename(assets)
            require(rejected_replacement)
            original_admission = a.prepare(journal, archive, expected, hashes)
            saved = a.read_prepared(archive, nonce, journal.records()[0]['planHash'])
            require(saved['originalDeployment']['database']['systemIdentifier'] == system_id
                    and (actual_application or saved['originalDeployment']['database']['migrations'] == [])
                    and set(saved['originalDeployment']['containers']) == {'api', 'db', 'edge'})
            fence = w.WriterFence(journal, expected, admitted['runtimeConfigurationSha256'], hashes, source)
            fence.maintenance()
            unclean_api_rejected=False
            try:fence.stop_writers()
            except ValueError:
                state=inspect(expected['api']['containerId'])['State']
                require(actual_application and state['Running'] is False and state['ExitCode']==137
                        and state['OOMKilled'] is False and journal.status()['pendingStage']=='stop-writers')
                unclean_api_rejected=True
            if os.environ.get("TDF_TEST_REQUIRE_CLEAN_APPLICATION_STOP") == "1":
                require(actual_application and not unclean_api_rejected
                        and inspect(expected["api"]["containerId"])["State"]["ExitCode"] == 0)
            if not unclean_api_rejected:
                fence.stop_database()
                require(fence.observe()['sources']['dockerWritersStopped'])
                require(journal.status()['completedStages'] == list(j.STAGES[:3]))
            else:
                require(journal.status()['completedStages']==['maintenance'] and inspect(db)['State']['Running'])
            captured = source.capture_uploads(str(archive/'uploads.tar')) if source else {
                'presence':'present','manifest':s.files.capture(str(DIRECTORY/'uploads'),str(archive/'uploads.tar'))}
            require(captured['presence'] == 'present')
            s.files.restore(str(archive/'uploads.tar'), captured['manifest'], str(archive/'restored-uploads'))
            require((archive/'restored-uploads/sentinel').read_bytes() == b'synthetic-upload')
            evidence = {'schemaVersion': 1, 'status': 'synthetic-original-db-api-edge-recovery-passed' if actual_edge else 'synthetic-original-db-api-recovery-passed' if actual_application else 'synthetic-real-daemon-fence-passed',
                'completedStages': journal.status()['completedStages'],
                'dockerWritersStopped':not unclean_api_rejected,'uncleanApiExitRejected':unclean_api_rejected, 'registeredTimerStopped': True,
                'originalDeploymentAdmissionVerified': original_admission['preparedBeforeShutdown'],
                'liveBindDirectoryReplacementRejected': rejected_replacement,
                'legacyUploadReplay': not actual_application,'persistentUploadReplay':actual_application, 'databaseSystemIdentifier': system_id,
                'limitations': ['Disposable empty Linux host; no production effect.',
                    'API and edge are inert shell processes, not backend/Caddy behavior.',
                    'No complete host-worker exclusion, clean-control or database recovery proof.',
                    'No production key custody, migration, rollout or restart recovery.']}
        if os.environ.get('TDF_TEST_ORIGINAL_DB_RECOVERY')=='1':
            evidence['originalDatabaseRecovery']=exercise_database_recovery(archive,saved,db,actual_application,actual_edge)
            evidence['fenceFlagsScope']='Historical successful or rejected fence observation before fixture-only recovery'
            evidence['limitations']=['Disposable PG17 component fixture; no production effect.',
                'Boot epochs are synthetic; API and TLS edge are real only when explicitly enabled.',
                'No complete host-worker, clean-shutdown, key-custody, migration or rollout proof.']
    finally:
        def cleanup_owned_resources():
            if evidence is None and before:
                def paths(left, right, prefix):
                    if type(left) is not type(right): return [prefix]
                    if isinstance(left, dict):
                        return [p for key in set(left) | set(right)
                                for p in paths(left.get(key), right.get(key), prefix+'/'+key)]
                    if isinstance(left, list):
                        if len(left) != len(right): return [prefix]
                        return [p for index, (a,b) in enumerate(zip(left,right))
                                for p in paths(a,b,prefix+'/'+str(index))]
                    return [] if left == right else [prefix]
                changed = []
                for service, baseline in before.items():
                    actual = inspect(created_containers[service])
                    changed += paths(baseline, {key: actual[key] for key in baseline}, service)
                print('Synthetic changed configuration fields: '+json.dumps(sorted(changed)), file=sys.stderr)
            # Verify nonce ownership before any destructive fixture cleanup. Unknown
            # resources or cleanup failures stop here and retain the remaining state.
            for name in reversed(created_units):
                require(hashlib.sha256((w.DIRECTORY/name).read_bytes()).hexdigest() == hashes[name])
                if name == w.TIMER: run(['systemctl', 'disable', '--now', name])
                (w.DIRECTORY/name).unlink()
            if created_units: run(['systemctl', 'daemon-reload'])
            for target in reversed(list(created_containers.values())):
                value = inspect(target)
                require(value['Config']['Labels'].get(LABEL) == nonce)
                run(DOCKER+['rm', '--force', value['Id']])
            for volume in reversed(created_volumes):
                row = json.loads(run(DOCKER+['volume', 'inspect', volume]))
                require(len(row) == 1 and row[0]['Labels'].get(LABEL) == nonce)
                run(DOCKER+['volume', 'rm', volume])
            for network in reversed(created_networks):
                row = json.loads(run(DOCKER+['network', 'inspect', network]))
                require(len(row) == 1 and row[0]['Labels'].get(LABEL) == nonce)
                run(DOCKER+['network', 'rm', network])
            # A failed mkdir or replaced canonical name must never authorize moving
            # an unowned tree that appeared after the initial absence observation.
            preserve_owned_directory(DIRECTORY, archive/'synthetic-production', owned_production)
        cleanup_preserving_tls(cleanup_owned_resources,tls)
    require(evidence is not None and not run(DOCKER+['ps', '--all', '--quiet']))
    evidence['ownedContainersRemoved'] = True
    if os.environ.get('TDF_TEST_ORIGINAL_TIMER_RECOVERY')=='1':evidence['originalTimerSchedulingRestored']=True
    if actual_edge:evidence['edgeTLSControls']={'untrustedCertificateRejected':True,'wrongHostnameRejected':True,'trustedOriginalEdgeVerified':True,'temporaryTrustRemoved':True}
    print(json.dumps(evidence, sort_keys=True))


if __name__ == '__main__': main()
