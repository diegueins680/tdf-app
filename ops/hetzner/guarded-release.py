#!/usr/bin/env python3
"""Guarded routine release of the TDF backend on the Hetzner production host.

Runs ON the production host as root, one phase per invocation, driven by
scripts/guarded-release.mjs from the operator's machine. Every production
effect is a stage of the fixed release journal (release-journal.py), so stages
run once, in order, and an interrupted stage blocks further progress until a
reviewed recovery.

Phases (each needs a separate operator confirmation of the release revision):
  backup   maintenance -> stop-writers -> stop-database -> capture -> encrypt
           -> retrieve-off-host -> restore-isolate
  migrate  start-database -> migrate
  cutover  start-candidate -> verify-candidate -> reopen

Public traffic sees a maintenance response from the first stage until reopen.
While it does, nothing but this release writes to the database, so the cold
capture taken at `capture` is an exact copy of production. `rollback` relies on
that: before reopen it may put the captured database back without losing any
user write. After reopen it refuses; restoring then would discard real writes.

Not covered (see ops/hetzner/README.md): writer exclusion against privileged
host processes outside this tool, coordinated assets/edge certificate recovery,
and an `abandon` journal record. After a rollback the journal stays interrupted
and a new release needs a reviewed journal extension first.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
import tarfile
import time
import urllib.error
import urllib.request
import uuid

HERE = Path(__file__).resolve().parent


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, HERE / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


journal_module = load('guarded_release_journal', 'release-journal.py')
STAGES = journal_module.STAGES
PHASES = {
    'backup': ('maintenance', 'stop-writers', 'stop-database', 'capture', 'encrypt',
               'retrieve-off-host', 'restore-isolate'),
    'migrate': ('start-database', 'migrate'),
    'cutover': ('start-candidate', 'verify-candidate', 'reopen'),
}
assert sum(PHASES.values(), ()) == STAGES, 'phases must cover the journal stages in order'

PROJECT = 'tdf-production'
DIRECTORY = Path('/opt/tdf/production')
CONTROL = Path('/opt/tdf/release-control')
BACKUPS = Path('/opt/tdf/backups')
TOOLS_ROOT = Path('/opt/tdf/release-tools')
VOLUME = 'tdf_production_postgres_data'
VOLUME_DATA = Path('/var/lib/docker/volumes') / VOLUME / '_data'
PUBLIC = 'https://api.tdfrecords.net'
BACKUP_TIMER = 'tdf-postgres-backup.timer'
MAINTENANCE_MARKER = 'tdf-release-maintenance'
MAINTENANCE_CADDYFILE = (
    'api.tdfrecords.net {\n'
    '\theader Retry-After 1800\n'
    '\theader X-TDF-Maintenance ' + MAINTENANCE_MARKER + '\n'
    '\trespond "TDF is being updated. Please try again in a few minutes." 503\n'
    '}\n').encode()
IMAGE_REF = re.compile(r'(?P<repo>[a-z0-9./_-]+)@(?P<digest>sha256:[a-f0-9]{64})')
REVISION = re.compile(r'[a-f0-9]{40}')
MIGRATION_ID = re.compile(r'[0-9A-Za-z_.-]{1,128}')
SMOKE_PATH = re.compile(r'/[A-Za-z0-9/_.-]{0,200}')
PARAM_KEYS = {'sourceRevision', 'mobileRevision', 'candidateImage', 'manifestHash',
              'migrationIds', 'recipient', 'storage', 'smokePaths', 'toolRevision'}
STORAGE_KEYS = {'host', 'port', 'user', 'directory'}


class Rejected(Exception):
    """A boundary check failed. Messages never contain secrets or raw output."""


def require(condition, message):
    if not condition:
        raise Rejected(message)


def sha(value):
    return hashlib.sha256(value).hexdigest()


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'), ensure_ascii=True) + '\n').encode()


def file_sha(path):
    value = hashlib.sha256()
    with open(path, 'rb') as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b''):
            value.update(block)
    return value.hexdigest()


def validate_params(params):
    require(isinstance(params, dict) and set(params) == PARAM_KEYS, 'release parameters have unexpected keys')
    for key in ('sourceRevision', 'mobileRevision', 'toolRevision'):
        require(isinstance(params[key], str) and REVISION.fullmatch(params[key]), key + ' must be a full revision')
    require(isinstance(params['candidateImage'], str) and IMAGE_REF.fullmatch(params['candidateImage']),
            'candidate image must be an immutable repo@sha256 reference')
    require(isinstance(params['manifestHash'], str) and re.fullmatch('[a-f0-9]{64}', params['manifestHash']),
            'manifest hash is invalid')
    ids = params['migrationIds']
    require(isinstance(ids, list) and 0 < len(ids) <= 2000 and len(set(ids)) == len(ids)
            and all(isinstance(i, str) and MIGRATION_ID.fullmatch(i) for i in ids), 'migration ids are invalid')
    require(isinstance(params['recipient'], str) and re.fullmatch(r'age1[0-9a-z]{58}', params['recipient']),
            'recipient must be a native age X25519 recipient')
    storage = params['storage']
    require(isinstance(storage, dict) and set(storage) == STORAGE_KEYS, 'storage target has unexpected keys')
    require(isinstance(storage['host'], str) and re.fullmatch(r'[a-z0-9][a-z0-9.-]{1,252}', storage['host']),
            'storage host is invalid')
    require(type(storage['port']) is int and 0 < storage['port'] < 65536, 'storage port is invalid')
    require(isinstance(storage['user'], str) and re.fullmatch(r'[a-z0-9][a-z0-9_-]{0,63}', storage['user']),
            'storage user is invalid')
    require(isinstance(storage['directory'], str) and re.fullmatch(r'[A-Za-z0-9_-]{1,64}', storage['directory']),
            'storage directory must be one plain path segment')
    paths = params['smokePaths']
    require(isinstance(paths, list) and len(paths) <= 20
            and all(isinstance(p, str) and SMOKE_PATH.fullmatch(p) for p in paths), 'smoke paths are invalid')
    return params


def journal_plan(params, runtime):
    """The journal's fixed plan keys, binding parameters and observed runtime."""
    recovery = IMAGE_REF.fullmatch(runtime['apiImage'])
    require(recovery is not None, 'running API image is not an immutable reference')
    return {
        'sourceRevision': params['sourceRevision'],
        'mobileRevision': params['mobileRevision'],
        'runtimeHash': sha(canonical({'runtime': runtime, 'params': params})),
        'manifestHash': params['manifestHash'],
        'candidateImage': IMAGE_REF.fullmatch(params['candidateImage'])['digest'],
        'recoveryImage': recovery['digest'],
        'recipientHash': sha(params['recipient'].encode()),
    }


def replace_env_value(text, key, expected, new):
    """Replace exactly one KEY=expected line, preserving every other byte."""
    lines = text.split('\n')
    matches = [i for i, line in enumerate(lines) if line.startswith(key + '=')]
    require(len(matches) == 1, key + ' must appear exactly once in the deployment .env')
    require(lines[matches[0]] == key + '=' + expected, key + ' does not hold the expected value')
    lines[matches[0]] = key + '=' + new
    return '\n'.join(lines)


class Host:
    """Every production side effect. Tests replace this with a fake."""

    DOCKER = ['env', '-u', 'DOCKER_HOST', '-u', 'DOCKER_CONTEXT', '-u', 'DOCKER_TLS_VERIFY',
              '-u', 'DOCKER_CERT_PATH', 'docker', '--host', 'unix:///var/run/docker.sock']
    ENV = {'PATH': '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8'}

    def __init__(self, tools=HERE):
        self.tools = Path(tools)
        self._runtime = load('guarded_release_runtime', 'inspect-runtime.py')
        self._files = load('guarded_release_files', 'recovery-files.py')
        self._envelope = load('guarded_release_envelope', 'recovery-envelope.py')
        self._physical = load('guarded_release_physical', 'physical-postgres-recovery.py')

    # -- primitives -------------------------------------------------------
    def run(self, command, *, input=None, timeout=120, capture=True):
        result = subprocess.run(command, input=input, text=True, timeout=timeout, env=self.ENV,
                                stdout=subprocess.PIPE if capture else subprocess.DEVNULL,
                                stderr=subprocess.DEVNULL)
        require(result.returncode == 0, 'host command failed: ' + command[command.index('docker') + 1
                if 'docker' in command else 0])
        return result.stdout if capture else ''

    def docker(self, *args, **kwargs):
        return self.run(self.DOCKER + list(args), **kwargs)

    def compose(self, *args, **kwargs):
        return self.docker('compose', '--project-name', PROJECT, '--project-directory', str(DIRECTORY),
                           '-f', str(DIRECTORY / 'compose.yaml'), '--profile', 'serve', *args, **kwargs)

    def container(self, service):
        ids = self.docker('ps', '--all', '--quiet', '--no-trunc',
                          '--filter', 'label=com.docker.compose.project=' + PROJECT,
                          '--filter', 'label=com.docker.compose.service=' + service).split()
        require(len(ids) == 1, 'expected exactly one ' + service + ' container')
        return json.loads(self.docker('inspect', ids[0]))[0]

    def psql(self, sql):
        db = self.container('db')
        require(db['State']['Running'], 'database is not running')
        return self.docker('exec', '-i', db['Id'], 'env', '-i', 'PATH=/usr/local/bin:/usr/bin:/bin',
                           'PGCONNECT_TIMEOUT=10', 'psql', '-X', '-qAt', '-v', 'ON_ERROR_STOP=1',
                           '-h', '/var/run/postgresql', '-p', '5432', '-U', 'postgres', '-d', 'tdf_hq',
                           input=sql, timeout=60).strip()

    def http(self, url, *, method='GET', body=None, timeout=15):
        request = urllib.request.Request(url, method=method, data=body,
                                         headers={'Content-Type': 'application/json'} if body else {})
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                return response.status, dict(response.headers), response.read(65536)
        except urllib.error.HTTPError as error:
            return error.code, dict(error.headers), error.read(65536)
        except (urllib.error.URLError, OSError, TimeoutError):
            return 0, {}, b''

    def read_bytes(self, path):
        return Path(path).read_bytes()

    def write_in_place(self, path, content):
        # Compose bind-mounts single files: a rename would leave the container on
        # the old inode, so overwrite the same inode and fsync it.
        fd = os.open(path, os.O_WRONLY | os.O_TRUNC | os.O_NOFOLLOW)
        try:
            view = memoryview(content)
            while view:
                written = os.write(fd, view)
                require(written > 0, 'short write')
                view = view[written:]
            os.fsync(fd)
        finally:
            os.close(fd)

    def write_new(self, path, content):
        fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
        try:
            os.write(fd, content)
            os.fsync(fd)
        finally:
            os.close(fd)

    def systemctl(self, *args):
        return subprocess.run(['systemctl', *args], env=self.ENV, stdout=subprocess.PIPE,
                              stderr=subprocess.DEVNULL, text=True, timeout=60)

    def sleep(self, seconds):
        time.sleep(seconds)

    # -- observations -----------------------------------------------------
    def observe_runtime(self):
        api, db = self.container('api'), self.container('db')
        api_summary = self._runtime.summarize_container('api', api)
        db_summary = self._runtime.summarize_container('db', db)
        require(api_summary['running'] and db_summary['running'], 'api and database must be running')
        env = dict(line.split('=', 1) for line in self.read_bytes(DIRECTORY / '.env').decode().splitlines()
                   if '=' in line and not line.lstrip().startswith('#'))
        require(env.get('TDF_IMAGE') == api_summary['image'], 'running API differs from configured TDF_IMAGE')
        require(env.get('POSTGRES_IMAGE') == db_summary['image'], 'running database differs from POSTGRES_IMAGE')
        status, _, body = self.http(PUBLIC + '/version')
        require(status == 200, 'public version endpoint unavailable')
        public = json.loads(body)
        return {'apiContainer': api['Id'], 'apiImage': api_summary['image'],
                'databaseContainer': db['Id'], 'databaseImage': db_summary['image'],
                'databaseImageId': db['Image'], 'publicCommit': public['commit'],
                'caddyfileSha256': sha(self.read_bytes(DIRECTORY / 'Caddyfile'))}

    def ledger(self):
        rows = self.psql('SELECT migration_id FROM public.tdf_schema_migration ORDER BY migration_id;')
        return [row for row in rows.splitlines() if row]

    def system_identifier(self):
        value = self.psql('SELECT system_identifier::text FROM pg_control_system();')
        require(re.fullmatch('[1-9][0-9]{0,19}', value) is not None, 'invalid system identifier')
        return value

    def other_database_clients(self):
        return int(self.psql("SELECT count(*) FROM pg_stat_activity WHERE backend_type='client backend' "
                             "AND pid <> pg_backend_pid();"))

    def database_network_peers(self):
        network = json.loads(self.docker('network', 'inspect', PROJECT + '_database'))[0]
        return sorted(network.get('Containers') or {})

    def cold_control(self, image):
        return self.docker('run', '--rm', '--pull=never', '--network=none', '--read-only',
                           '--user', '999:999', '--cap-drop=ALL', '--security-opt=no-new-privileges:true',
                           '--mount', 'type=volume,src=' + VOLUME + ',dst=/var/lib/postgresql/data,readonly',
                           '--entrypoint', 'pg_controldata', image, '/var/lib/postgresql/data')

    # -- heavy effects delegated to reviewed primitives --------------------
    def capture_tree(self, source, destination):
        return self._files.capture(str(source), str(destination))

    def admit_cold_manifest(self, manifest):
        self._physical.admit_cold_manifest(manifest)

    def control_identity(self, text, system_id):
        return self._physical.control_identity(text, system_id)

    def restore_tree(self, archive, manifest, destination):
        return self._files.restore(str(archive), manifest, str(destination))

    def encrypt(self, binary, source, destination, recipient):
        return self._envelope.encrypt(str(binary), str(source), str(destination), recipient)

    def sftp(self, storage, batch):
        key = CONTROL / 'storage-transfer-key'
        known = CONTROL / 'storage-known-hosts'
        self.run(['sftp', '-q', '-b', '-', '-P', str(storage['port']), '-i', str(key),
                  '-o', 'IdentitiesOnly=yes', '-o', 'BatchMode=yes', '-o', 'StrictHostKeyChecking=yes',
                  '-o', 'UserKnownHostsFile=' + str(known), '-o', 'ConnectTimeout=15',
                  storage['user'] + '@' + storage['host']], input=batch, timeout=1800, capture=False)

    def isolated_start(self, release_dir, nonce, runtime, manifest, system_id):
        """Start the captured cold copy in the reviewed isolate; return its ledger."""
        directory = self._physical.HOST_ROOT / ('rehearsal-' + nonce)
        directory.mkdir(mode=0o700)
        self.restore_tree(release_dir / 'database.tar', manifest, directory / 'physical-data')
        clone = self._physical.PhysicalClone(runtime['databaseContainer'], runtime['databaseImage'],
                                             runtime['databaseImageId'], nonce, str(directory), system_id)
        with clone.reserved():
            clone.prepare(manifest)
            evidence = clone.start()
            rows = self._physical.restore.execute(clone.write_command('psql', [
                '-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq']),
                input='SELECT migration_id FROM public.tdf_schema_migration ORDER BY migration_id;')
        return evidence, [row for row in rows.splitlines() if row]


class Release:
    def __init__(self, host, journal, release_dir):
        self.host, self.journal, self.dir = host, journal, Path(release_dir)

    # -- small helpers ----------------------------------------------------
    def record(self, name, value):
        raw = canonical(value)
        self.host.write_new(self.dir / name, raw)
        return sha(raw)

    def load(self, name):
        return json.loads((self.dir / name).read_bytes())

    def params(self):
        return validate_params(self.load('params.json'))

    def runtime(self):
        return self.load('runtime.json')

    def confirm_bound(self):
        """params.json and runtime.json must still hash to the journal's plan."""
        if HERE.parent == TOOLS_ROOT:
            require(HERE.name == self.params()['toolRevision'], 'tool code differs from the planned tool revision')
        status = self.journal.status()
        plan = journal_plan(self.params(), self.runtime())
        require(sha(canonical(plan)) == status['planHash'], 'release files no longer match the journal plan')
        return status

    def wait_for(self, predicate, message, attempts=60, delay=2):
        for _ in range(attempts):
            if predicate():
                return
            self.host.sleep(delay)
        raise Rejected(message)

    def edge_sees(self, content):
        edge = self.host.container('edge')
        return self.host.docker('exec', edge['Id'], 'cat', '/etc/caddy/Caddyfile').encode() == content

    def reload_edge(self):
        edge = self.host.container('edge')
        self.host.docker('exec', edge['Id'], 'caddy', 'reload', '--config', '/etc/caddy/Caddyfile',
                         '--adapter', 'caddyfile')

    def set_caddyfile(self, content):
        self.host.write_in_place(DIRECTORY / 'Caddyfile', content)
        require(self.edge_sees(content), 'edge container does not see the written Caddyfile')
        self.reload_edge()

    # -- stages -----------------------------------------------------------
    def stage_maintenance(self):
        runtime = self.runtime()
        original = self.host.read_bytes(DIRECTORY / 'Caddyfile')
        require(sha(original) == runtime['caddyfileSha256'], 'Caddyfile changed since the plan was observed')
        require(MAINTENANCE_MARKER.encode() not in original, 'Caddyfile is already a maintenance file')
        self.host.write_new(self.dir / 'Caddyfile.original', original)
        self.set_caddyfile(MAINTENANCE_CADDYFILE)
        status, headers, _ = self.host.http(PUBLIC + '/health')
        lowered = {key.lower(): value for key, value in headers.items()}
        require(status == 503 and lowered.get('x-tdf-maintenance') == MAINTENANCE_MARKER,
                'public endpoint is not serving the maintenance response')
        return {'originalCaddyfileSha256': sha(original), 'publicStatus': status}

    def stage_stop_writers(self):
        runtime = self.runtime()
        api = self.host.container('api')
        require(api['Id'] == runtime['apiContainer'], 'API container changed since the plan')
        timer = self.host.systemctl('is-active', BACKUP_TIMER).stdout.strip()
        self.host.systemctl('stop', BACKUP_TIMER)
        service = self.host.systemctl('is-active', 'tdf-postgres-backup.service').stdout.strip()
        require(service != 'active', 'a scheduled backup is running; retry after it completes')
        self.host.compose('stop', '--timeout', '60', 'api', timeout=180)
        require(not self.host.container('api')['State']['Running'], 'API did not stop')
        peers = self.host.database_network_peers()
        require(peers == [runtime['databaseContainer']], 'other containers are attached to the database network')
        require(self.host.other_database_clients() == 0, 'database still has client connections')
        ledger = self.host.ledger()
        return {'backupTimerWasActive': timer == 'active', 'ledgerCount': len(ledger),
                'ledgerSha256': sha(canonical(ledger)), 'ledger': ledger,
                'systemIdentifier': self.host.system_identifier()}

    def stage_stop_database(self):
        runtime = self.runtime()
        writers = self.load('stop-writers.json')
        self.host.compose('stop', '--timeout', '120', 'db', timeout=240)
        require(not self.host.container('db')['State']['Running'], 'database did not stop')
        control = self.host.control_identity(self.host.cold_control(runtime['databaseImage']),
                                             writers['systemIdentifier'])
        return {'control': control}

    def stage_capture(self):
        writers = self.load('stop-writers.json')
        require(not self.host.container('db')['State']['Running'], 'database must be stopped for a cold capture')
        require(not self.host.container('api')['State']['Running'], 'API must be stopped for capture')
        database = self.host.capture_tree(VOLUME_DATA, self.dir / 'database.tar')
        self.host.admit_cold_manifest(database)
        production = self.host.capture_tree(DIRECTORY, self.dir / 'production.tar')
        manifests = {'database': database, 'production': production}
        self.host.write_new(self.dir / 'manifests.json', canonical(manifests))
        bundle = self.dir / 'bundle.tar'
        fd = os.open(bundle, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
        with os.fdopen(fd, 'wb') as handle:
            with tarfile.open(fileobj=handle, mode='w', format=tarfile.PAX_FORMAT) as archive:
                for name in ('database.tar', 'production.tar', 'manifests.json', 'Caddyfile.original'):
                    archive.add(str(self.dir / name), arcname=name, recursive=False)
            handle.flush()
            os.fsync(handle.fileno())
        return {'systemIdentifier': writers['systemIdentifier'],
                'databaseManifestSha256': sha(canonical(database)),
                'productionManifestSha256': sha(canonical(production)),
                'bundleSha256': file_sha(bundle), 'bundleBytes': bundle.stat().st_size}

    def stage_encrypt(self):
        params, capture = self.params(), self.load('capture.json')
        bundle = self.dir / 'bundle.tar'
        require(file_sha(bundle) == capture['bundleSha256'], 'bundle changed after capture')
        result = self.host.encrypt(self.host.tools / 'age', bundle, self.dir / 'bundle.tar.age', params['recipient'])
        require(result['plaintext']['sha256'] == capture['bundleSha256'], 'encrypted plaintext differs')
        # The trusted record that binds the off-host object to this release.
        status = self.journal.status()
        record = {'schemaVersion': 1, 'releaseNonce': status['releaseNonce'], 'planHash': status['planHash'],
                  'sourceRevision': params['sourceRevision'], 'capture': capture,
                  'plaintext': result['plaintext'], 'ciphertext': result['ciphertext'],
                  'recipientSha256': result['recipientSha256'], 'ageSha256': result['toolSha256']}
        self.host.write_new(self.dir / 'bundle-record.json', canonical(record))
        return {'ciphertext': result['ciphertext'], 'recordSha256': sha(canonical(record))}

    def stage_retrieve_off_host(self):
        params, encrypted = self.params(), self.load('encrypt.json')
        storage = params['storage']
        nonce = self.journal.status()['releaseNonce']
        remote = storage['directory'] + '/' + nonce
        self.host.sftp(storage, '-mkdir {d}\nmkdir {r}\nput {a} {r}/bundle.tar.age\nput {b} {r}/bundle-record.json\n'
                       .format(d=storage['directory'], r=remote, a=self.dir / 'bundle.tar.age',
                               b=self.dir / 'bundle-record.json'))
        # A fresh connection must return the exact bytes; an upload ack is not enough.
        retrieved = self.dir / 'retrieved.tar.age'
        self.host.sftp(storage, 'get {r}/bundle.tar.age {t}\n'.format(r=remote, t=retrieved))
        size, digest = retrieved.stat().st_size, file_sha(retrieved)
        require(size == encrypted['ciphertext']['bytes'] and digest == encrypted['ciphertext']['sha256'],
                'off-host copy does not match the encrypted bundle')
        return {'remotePath': remote + '/bundle.tar.age', 'retrievedSha256': digest, 'retrievedBytes': size}

    def stage_restore_isolate(self):
        writers, capture = self.load('stop-writers.json'), self.load('capture.json')
        manifests = json.loads((self.dir / 'manifests.json').read_bytes())
        require(sha(canonical(manifests['database'])) == capture['databaseManifestSha256'],
                'database manifest changed after capture')
        nonce = self.journal.status()['releaseNonce']
        evidence, ledger = self.host.isolated_start(self.dir, nonce, self.runtime(), manifests['database'],
                                                    capture['systemIdentifier'])
        require(ledger == writers['ledger'], 'restored copy has a different migration ledger')
        return {'isolate': evidence, 'ledgerSha256': sha(canonical(ledger))}

    def stage_start_database(self):
        writers = self.load('stop-writers.json')
        self.host.compose('start', 'db', timeout=180)

        def healthy():
            state = self.host.container('db')['State']
            return state['Running'] and state.get('Health', {}).get('Status') == 'healthy'
        self.wait_for(healthy, 'database did not become healthy')
        require(self.host.system_identifier() == writers['systemIdentifier'], 'database identity changed')
        require(self.host.ledger() == writers['ledger'], 'migration ledger changed while stopped')
        require(not self.host.container('api')['State']['Running'], 'API started unexpectedly')
        return {'systemIdentifier': writers['systemIdentifier']}

    def stage_migrate(self):
        params = self.params()
        require(not self.host.container('api')['State']['Running'], 'API must stay stopped for migrations')
        self.host.docker('run', '--rm', '--pull=never', '--name', 'tdf-release-migrate-' + self.journal.status()['releaseNonce'],
                         '--network', PROJECT + '_database', '--env-file', str(DIRECTORY / 'api.env'),
                         '--env', 'APP_ENV=production', '--env', 'AUTO_APPLY_PRODUCTION_MIGRATIONS=true',
                         '--env', 'TDF_MIGRATION_PRECHECK_ONLY=true', '--env', 'RUN_MIGRATIONS=false',
                         '--cap-drop=ALL', '--security-opt=no-new-privileges:true', '--memory=512m',
                         params['candidateImage'], timeout=900, capture=False)
        ledger = self.host.ledger()
        missing = sorted(set(params['migrationIds']) - set(ledger))
        require(not missing, 'migrations missing after apply: ' + ','.join(missing[:5]))
        return {'ledgerCount': len(ledger), 'ledgerSha256': sha(canonical(ledger))}

    def stage_start_candidate(self):
        params, runtime = self.params(), self.runtime()
        original = self.host.read_bytes(DIRECTORY / '.env')
        self.host.write_new(self.dir / 'env.original', original)
        updated = replace_env_value(original.decode(), 'TDF_IMAGE', runtime['apiImage'], params['candidateImage'])
        self.host.write_in_place(DIRECTORY / '.env', updated.encode())
        self.host.compose('up', '--detach', '--no-deps', 'api', timeout=300)

        def running():
            api = self.host.container('api')
            return (api['State']['Running'] and not api['State'].get('Restarting')
                    and api['Config']['Image'] == params['candidateImage'])
        self.wait_for(running, 'candidate API did not start')
        return {'apiContainer': self.host.container('api')['Id'], 'image': params['candidateImage']}

    def api_base(self):
        api = self.host.container('api')
        address = api['NetworkSettings']['Networks'][PROJECT + '_outbound']['IPAddress']
        require(re.fullmatch(r'[0-9.]{7,15}', address or '') is not None, 'API has no outbound address')
        return 'http://' + address + ':8080'

    def stage_verify_candidate(self):
        params = self.params()
        base = self.api_base()

        def healthy():
            status, _, body = self.host.http(base + '/health')
            return status == 200 and json.loads(body or b'{}') == {'db': 'ok', 'status': 'ok'}
        self.wait_for(healthy, 'candidate health check failed')
        status, _, body = self.host.http(base + '/version')
        require(status == 200 and json.loads(body)['commit'] == params['sourceRevision'],
                'candidate reports a different revision')
        smoke = {}
        for path in params['smokePaths']:
            code, _, _ = self.host.http(base + path)
            require(code == 200, 'smoke check failed for ' + path)
            smoke[path] = code
        login = CONTROL / 'smoke-login.json'
        login_verified = False
        if login.exists():
            spec = json.loads(self.host.read_bytes(login))
            code, _, body = self.host.http(base + spec['path'], method='POST',
                                           body=json.dumps(spec['body']).encode())
            require(code == 200 and body, 'smoke login failed')
            login_verified = True
        return {'version': params['sourceRevision'], 'smoke': smoke, 'loginVerified': login_verified}

    def stage_reopen(self):
        params, writers = self.params(), self.load('stop-writers.json')
        original = self.host.read_bytes(self.dir / 'Caddyfile.original')
        self.set_caddyfile(original)

        def public_ok():
            status, _, body = self.host.http(PUBLIC + '/version')
            return status == 200 and json.loads(body).get('commit') == params['sourceRevision']
        self.wait_for(public_ok, 'public endpoint does not serve the candidate', attempts=30)
        if writers['backupTimerWasActive']:
            self.host.systemctl('start', BACKUP_TIMER)
        return {'publicCommit': params['sourceRevision']}

    # -- driving ----------------------------------------------------------
    def run_phase(self, phase, confirm):
        params = self.params()
        require(phase in PHASES, 'unknown phase')
        require(confirm == params['sourceRevision'][:12], 'confirmation does not match the release revision')
        status = self.confirm_bound()
        require(status['pendingStage'] is None, 'stage ' + str(status.get('pendingStage')) +
                ' was interrupted; run rollback or a reviewed recovery')
        done = status['completedStages']
        for stage in PHASES[phase]:
            if stage in done:
                continue
            require(STAGES.index(stage) == len(done), 'phases must run in order; next is ' + STAGES[len(done)])
            effect_fn = getattr(self, 'stage_' + stage.replace('-', '_'))

            def effect(context, effect_fn=effect_fn, stage=stage):
                evidence = effect_fn()
                evidence_hash = self.record(stage + '.json', evidence)
                return {**context, 'evidenceHash': evidence_hash}
            status = self.journal.perform(stage, sha(canonical({'stage': stage, 'dir': str(self.dir)})), effect)
            done = status['completedStages']
            print('completed ' + stage, flush=True)
        return self.journal.status()

    def rollback(self, confirm):
        """Return to the recovery image before traffic reopens. Never after."""
        params, runtime = self.params(), self.runtime()
        require(confirm == 'rollback-' + params['sourceRevision'][:12], 'rollback confirmation does not match')
        status = self.confirm_bound()
        done, pending = status['completedStages'], status['pendingStage']
        touched = set(done) | ({pending} if pending else set())
        require('reopen' not in touched, 'traffic reopened; restoring now would discard real writes')
        require(touched, 'nothing to roll back')
        restore_database = bool(touched & {'migrate', 'start-candidate', 'verify-candidate'})
        steps = []
        if self.host.container('api')['State']['Running']:
            self.host.compose('stop', '--timeout', '60', 'api', timeout=180)
            steps.append('stopped-api')
        if (self.dir / 'env.original').exists():
            self.host.write_in_place(DIRECTORY / '.env', self.host.read_bytes(self.dir / 'env.original'))
            steps.append('restored-env')
        configured = self.host.read_bytes(DIRECTORY / '.env').decode()
        require(('TDF_IMAGE=' + runtime['apiImage']) in configured.split('\n'), '.env does not name the recovery image')
        if restore_database:
            require(os.path.exists(self.dir / 'capture.json'), 'no verified capture to restore')
            capture, writers = self.load('capture.json'), self.load('stop-writers.json')
            manifests = json.loads((self.dir / 'manifests.json').read_bytes())
            require(sha(canonical(manifests['database'])) == capture['databaseManifestSha256'],
                    'database manifest changed')
            if self.host.container('db')['State']['Running']:
                self.host.compose('stop', '--timeout', '120', 'db', timeout=240)
            require(not self.host.container('db')['State']['Running'], 'database did not stop')
            staged = self.dir / 'restored-data'
            self.host.restore_tree(self.dir / 'database.tar', manifests['database'], staged)
            # Same filesystem renames: keep the post-migration data for review.
            os.rename(VOLUME_DATA, self.dir / 'failed-data')
            os.rename(staged, VOLUME_DATA)
            steps.append('restored-database')
        if not self.host.container('db')['State']['Running']:
            self.host.compose('start', 'db', timeout=180)
            self.wait_for(lambda: self.host.container('db')['State'].get('Health', {}).get('Status') == 'healthy',
                          'database did not become healthy')
            steps.append('started-database')
        if (self.dir / 'stop-writers.json').exists():
            writers = self.load('stop-writers.json')
            require(self.host.ledger() == writers['ledger'], 'ledger does not match the pre-release capture')
        self.host.compose('up', '--detach', '--no-deps', 'api', timeout=300)
        self.wait_for(lambda: self.host.container('api')['State']['Running'], 'recovery API did not start')
        base = self.api_base()
        self.wait_for(lambda: self.host.http(base + '/health')[0] == 200, 'recovery API is unhealthy')
        code, _, body = self.host.http(base + '/version')
        require(code == 200 and json.loads(body)['commit'] == runtime['publicCommit'],
                'recovery API reports an unexpected revision')
        steps.append('started-recovery-api')
        if (self.dir / 'Caddyfile.original').exists():
            self.set_caddyfile(self.host.read_bytes(self.dir / 'Caddyfile.original'))
            steps.append('reopened')
        if os.path.exists(self.dir / 'stop-writers.json') and self.load('stop-writers.json')['backupTimerWasActive']:
            self.host.systemctl('start', BACKUP_TIMER)
        receipt = {'schemaVersion': 1, 'status': 'rolled-back', 'steps': steps,
                   'journalStatus': status, 'databaseRestored': restore_database,
                   'note': 'Journal remains interrupted; a reviewed abandon record is needed before the next release.'}
        self.record('rollback.json', receipt)
        return receipt


def plan(host, journal, params):
    """Observe production read-only, pull the candidate and record the journal plan."""
    validate_params(params)
    runtime = host.observe_runtime()
    require(runtime['apiImage'] != params['candidateImage'], 'candidate is already running')
    host.docker('pull', params['candidateImage'], timeout=900, capture=False)
    ids = host.docker('image', 'inspect', '--format', '{{json .RepoDigests}}', params['candidateImage'])
    require(params['candidateImage'] in json.loads(ids), 'pulled image does not carry the candidate digest')
    plan_value = journal_plan(params, runtime)
    nonce = uuid.uuid4().hex
    release_dir = BACKUPS / ('release-' + nonce)
    release_dir.mkdir(mode=0o700)
    host.write_new(release_dir / 'params.json', canonical(params))
    host.write_new(release_dir / 'runtime.json', canonical(runtime))
    records = journal.records()
    if records:
        journal.initialize_next(plan_value, nonce, sha(canonical(records[-1])))
    else:
        journal.initialize(plan_value, nonce)
    return {'releaseNonce': nonce, 'releaseDirectory': str(release_dir), 'plan': plan_value,
            'runtime': {k: runtime[k] for k in ('apiImage', 'publicCommit')}}


def current_release_dir(journal):
    status = journal.status()
    directory = BACKUPS / ('release-' + status['releaseNonce'])
    info = directory.lstat()
    require(stat.S_ISDIR(info.st_mode) and info.st_uid == os.geteuid() and info.st_mode & 0o077 == 0,
            'release directory is missing or not private')
    return directory


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    sub = parser.add_subparsers(dest='command', required=True)
    sub.add_parser('plan', help='read release parameters JSON from stdin')
    run = sub.add_parser('run')
    run.add_argument('phase', choices=sorted(PHASES))
    run.add_argument('--confirm', required=True)
    sub.add_parser('status')
    back = sub.add_parser('rollback')
    back.add_argument('--confirm', required=True)
    args = parser.parse_args(argv)
    os.umask(0o077)
    require(os.geteuid() == 0, 'run as root on the production host')
    host = Host()
    for directory in (CONTROL, CONTROL / 'journal', BACKUPS):
        directory.mkdir(mode=0o700, exist_ok=True)
        info = directory.lstat()
        require(stat.S_ISDIR(info.st_mode) and info.st_uid == 0 and info.st_mode & 0o077 == 0,
                str(directory) + ' must be a root-private directory')
    with journal_module.open_journal(str(CONTROL / 'journal')) as journal:
        if args.command == 'plan':
            result = plan(host, journal, json.loads(sys.stdin.read(1024 * 1024)))
        else:
            release = Release(host, journal, current_release_dir(journal))
            if args.command == 'status':
                result = {**journal.status(), 'releaseDirectory': str(release.dir)}
            elif args.command == 'run':
                result = release.run_phase(args.phase, args.confirm)
            else:
                result = release.rollback(args.confirm)
    print(json.dumps(result, sort_keys=True))


if __name__ == '__main__':
    try:
        main(sys.argv[1:])
    except Rejected as error:
        print('Release stopped: ' + str(error), file=sys.stderr)
        sys.exit(2)
    except Exception as error:  # Never print raw output, SQL, env or credentials.
        print('Release stopped by a ' + type(error).__name__ + '; inspect the journal before any retry.',
              file=sys.stderr)
        sys.exit(1)
