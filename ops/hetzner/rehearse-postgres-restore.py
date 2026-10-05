#!/usr/bin/env python3
"""Restore an online production snapshot only into a bounded, isolated container.

Source database operations are read-only. Archives stay on the host under0700;
only hashes, table counts and migration identities may enter the receipt.
This is not an asset-coordinated release backup or a deployment executor.
"""
from contextlib import contextmanager
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import selectors
import stat
import subprocess
import time
import uuid

DOCKER = ['env', '-u', 'DOCKER_HOST', '-u', 'DOCKER_CONTEXT', '-u', 'DOCKER_TLS_VERIFY',
          '-u', 'DOCKER_CERT_PATH', 'docker', '--host', 'unix:///var/run/docker.sock']

LABEL = 'net.tdf.restore-rehearsal'
DATA = '/var/lib/postgresql/data'
MAX_DATABASE = 128 * 1024 * 1024
MAX_ARCHIVE = 256 * 1024 * 1024
MEMORY_LIMIT = 384 * 1024 * 1024
CAPACITY_SQL = 'SELECT pg_database_size(current_database());'
# query_to_xml exposes only counts, never table contents, even in subprocess output.
COUNTS_SQL = """SELECT coalesce(jsonb_object_agg(format('%I.%I',n.nspname,c.relname),
 ((xpath('/row/count/text()',query_to_xml(format('SELECT count(*) AS count FROM %I.%I',
 n.nspname,c.relname),false,true,'')))[1]::text)::bigint),'{}'::jsonb)
 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace
 WHERE c.relkind IN ('r','p','m') AND n.nspname <> 'information_schema'
 AND n.nspname !~ '^pg_';"""


def require(condition):
    if not condition:
        raise ValueError('Restore rehearsal boundary rejected')


def digest_file(path):
    value = hashlib.sha256()
    with open(path, 'rb') as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b''):
            value.update(block)
    return value.hexdigest()


def execute(command, *, input=None, timeout=60):
    result = subprocess.run(command, input=input, text=True, stdout=subprocess.PIPE,
                            stderr=subprocess.DEVNULL, timeout=timeout)
    require(result.returncode == 0 and len(result.stdout) <= 4 * 1024 * 1024)
    return result.stdout


def local_database(container, program, arguments, *, read_only):
    require(re.fullmatch(r'[a-f0-9]{64}', container))
    require(program in ('psql', 'pg_dump', 'pg_dumpall', 'pg_restore'))
    options = '-c statement_timeout=60000 -c lock_timeout=5000'
    if read_only:
        require(program in ('psql', 'pg_dump', 'pg_dumpall'))
        options += ' -c default_transaction_read_only=on -c idle_in_transaction_session_timeout=300000'
    return DOCKER + ['exec', '-i', container, 'env', '-u', 'PGHOSTADDR', '-u', 'PGSERVICE',
            '-u', 'PGSERVICEFILE', 'PGOPTIONS=' + options,
            'timeout', '--signal=TERM', '--kill-after=5', '240' if program == 'psql' else '150', program,
            '-h', '/var/run/postgresql', '-p', '5432', '-U', 'postgres', *arguments]


def psql(container, *, read_only):
    return local_database(container, 'psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq'], read_only=read_only)


def validate_counts(value):
    require(isinstance(value, dict) and 0 < len(value) <= 2000)
    require(all(isinstance(key, str) and len(key) <= 256 and
                type(count) is int and count >= 0 for key, count in value.items()))
    return value


class HeldSnapshot:
    def __init__(self, source):
        self.process = subprocess.Popen(psql(source, read_only=True), stdin=subprocess.PIPE,
                                        stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)

    def query(self, sql):
        self.process.stdin.write(sql + '\n')
        self.process.stdin.flush()
        with selectors.DefaultSelector() as selector:
            selector.register(self.process.stdout, selectors.EVENT_READ)
            require(bool(selector.select(70)))
        line = self.process.stdout.readline(512 * 1024)
        require(bool(line) and line.endswith('\n'))
        return line.strip()

    def close(self):
        # Disconnect rolls back the read-only exporting transaction. No COMMIT
        # or SQL mutation is sent to production during this rehearsal.
        self.process.stdin.close()
        try:
            self.process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            self.process.terminate()
            self.process.wait(timeout=10)


class IsolatedRestore:
    def __init__(self, source, image, image_id, nonce):
        require(re.fullmatch(r'[a-f0-9]{64}', source))
        require(re.fullmatch(r'[a-f0-9]{32}', nonce))
        require(re.fullmatch(r'[a-zA-Z0-9./_-]+@sha256:[a-f0-9]{64}', image))
        require(re.fullmatch(r'sha256:[a-f0-9]{64}', image_id))
        self.source, self.image, self.image_id, self.nonce = source, image, image_id, nonce
        self.target = None
        self.creation_attempted = False

    def create_command(self):
        return DOCKER + ['create', '--pull=never', '--name', 'tdf-audit-restore-' + self.nonce,
                '--label', LABEL + '=' + self.nonce, '--network=none', '--read-only',
                '--memory=' + str(MEMORY_LIMIT), '--memory-swap=' + str(MEMORY_LIMIT),
                '--cpus=0.5', '--pids-limit=64', '--security-opt=no-new-privileges:true',
                '--tmpfs', DATA + ':rw,nosuid,nodev,size=268435456',
                '--tmpfs', '/var/run/postgresql:rw,nosuid,nodev,size=16777216',
                '--tmpfs', '/tmp:rw,nosuid,nodev,size=16777216',
                '--env', 'POSTGRES_DB=tdf_hq', '--env', 'POSTGRES_HOST_AUTH_METHOD=trust',
                self.image, 'postgres', '-c', 'shared_buffers=16MB', '-c', 'max_connections=10',
                '-c', 'work_mem=2MB', '-c', 'max_wal_size=64MB', '-c', 'min_wal_size=32MB']

    def admit(self, container):
        target = container.get('Id')
        require(isinstance(target, str) and re.fullmatch(r'[a-f0-9]{64}', target))
        require(target != self.source and (self.target is None or self.target == target))
        require(container['Config']['Labels'].get(LABEL) == self.nonce)
        require(container['Image'] == self.image_id and container['Config']['Image'] == self.image)
        host = container['HostConfig']
        require(host['NetworkMode'] == 'none' and not host.get('PortBindings'))
        require(host['ReadonlyRootfs'] and host['Memory'] == MEMORY_LIMIT and host['MemorySwap'] == MEMORY_LIMIT)
        require(host['NanoCpus'] == 500000000 and host['PidsLimit'] == 64)
        require(not host.get('Privileged') and not host.get('Binds') and not host.get('VolumesFrom'))
        require(not host.get('Devices') and not host.get('CapAdd'))
        require(not host.get('PidMode') and not host.get('UTSMode') and host.get('IpcMode') == 'private')
        require('no-new-privileges:true' in host.get('SecurityOpt', []))
        require(all(mount['Type'] == 'tmpfs' and mount['Destination'] in
                    (DATA, '/var/run/postgresql', '/tmp') for mount in container['Mounts']))
        require(set(host['Tmpfs']) == {DATA, '/var/run/postgresql', '/tmp'})
        self.target = target
        return target

    def inspect(self, *, timeout=60):
        require(self.target is not None)
        values = json.loads(execute(DOCKER + ['inspect', self.target], timeout=timeout))
        require(len(values) == 1)
        self.admit(values[0])

    def write_command(self, program, arguments):
        require(self.target is not None and self.target != self.source)
        return local_database(self.target, program, arguments, read_only=False)

    def cleanup(self):
        if self.target is None and self.creation_attempted:
            # docker create can succeed daemon-side before its client loses the
            # response. Recover only the nonce-named candidate, then revalidate
            # image, full ownership and isolation before allowing deletion.
            values = json.loads(execute(DOCKER + ['inspect', 'tdf-audit-restore-' + self.nonce], timeout=10))
            require(len(values) == 1)
            self.admit(values[0])
        if self.target is not None:
            self.inspect(timeout=10)  # A label alone or a caller-supplied name is never a deletion target.
            execute(DOCKER + ['rm', '--force', self.target], timeout=10)
            self.target = None
            self.creation_attempted = False


def bounded_archive(command, destination):
    def file_limit():
        resource.setrlimit(resource.RLIMIT_FSIZE, (MAX_ARCHIVE, MAX_ARCHIVE))
    with open(destination, 'xb') as output:
        os.chmod(destination, 0o600)
        result = subprocess.run(command, stdout=output, stderr=subprocess.DEVNULL,
                                timeout=180, preexec_fn=file_limit)
    require(result.returncode == 0 and 0 < destination.stat().st_size <= MAX_ARCHIVE)


@contextmanager
def rehearsal_lock(directory):
    # Keep the lock inode permanently: unlinking a held lock permits concurrent
    # callers to lock different inodes. Only this rehearsal uses this lock;
    # deployment/writer exclusion remains a separate release obligation.
    info = directory.lstat()
    require(stat.S_ISDIR(info.st_mode) and info.st_uid == os.geteuid() and info.st_mode & 0o077 == 0)
    descriptor = os.open(directory / 'restore-rehearsal.lock', os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    try:
        info = os.fstat(descriptor)
        require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1 and
                info.st_uid == os.geteuid() and info.st_mode & 0o077 == 0)
        fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        yield
    finally:
        os.close(descriptor)


def rehearse(runtime):
    require(os.geteuid() == 0)
    with rehearsal_lock(Path('/opt/tdf/backups')):
        return rehearse_locked(runtime)


def rehearse_locked(runtime):
    # runtime is the reviewed read-only collector bundled by the launcher.
    snapshot = runtime.inspect()
    db = snapshot['containers']['db']
    size = int(execute(psql(db['containerId'], read_only=True), input=CAPACITY_SQL))
    mem = {line.split(':')[0]: int(line.split()[1]) * 1024
           for line in Path('/proc/meminfo').read_text().splitlines() if line.startswith('MemAvailable:')}
    disk = os.statvfs('/opt/tdf')
    require(0 < size <= MAX_DATABASE and mem['MemAvailable'] >= 1024 * 1024 * 1024)
    require(disk.f_bavail * disk.f_frsize >= 2 * 1024 * 1024 * 1024)
    nonce = uuid.uuid4().hex
    directory = Path('/opt/tdf/backups') / ('rehearsal-' + nonce)
    require(directory.parent.is_dir() and not directory.parent.is_symlink())
    parent_stat = directory.parent.stat()
    require(parent_stat.st_uid == 0 and parent_stat.st_mode & 0o077 == 0)
    directory.mkdir(mode=0o700)
    archive = directory / 'database.pgdump'
    roles = directory / 'roles.sql'
    target = IsolatedRestore(db['containerId'], db['image'], db['imageId'], nonce)
    held = HeldSnapshot(db['containerId'])
    stage = 'snapshot'
    try:
        exported = held.query('BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY; SELECT pg_export_snapshot();')
        require(re.fullmatch(r'[A-Fa-f0-9]+-[A-Fa-f0-9]+-[0-9]+', exported))
        source_counts = validate_counts(json.loads(held.query(COUNTS_SQL)))
        bounded_archive(local_database(db['containerId'], 'pg_dump',
                        ['-d', 'tdf_hq', '-Fc', '--lock-wait-timeout=5s', '--snapshot=' + exported], read_only=True), archive)
        held.close()
        held = None
        # Role passwords never enter an archive. Globals are not part of the
        # exported database snapshot; roles/schema changes must be excluded operationally.
        bounded_archive(local_database(db['containerId'], 'pg_dumpall',
                        ['--roles-only', '--no-role-passwords'], read_only=True), roles)
        stage = 'create-isolate'
        target.creation_attempted = True
        target.target = execute(target.create_command()).strip()
        target.inspect()
        execute(DOCKER + ['start', target.target])
        for attempt in range(45):
            ready = subprocess.run(local_database(target.target, 'psql',
                        ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq', '-c', 'SELECT 1'],
                        read_only=True), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=5)
            if ready.returncode == 0:
                break
            time.sleep(1)
        else:
            raise ValueError('Restore target unavailable')
        stage = 'restore-roles'
        # initdb already created postgres. All other role definitions and ACLs
        # are retained; no --no-owner or --no-acl shortcut disguises restore gaps.
        role_text = roles.read_text()
        require(role_text.count('CREATE ROLE postgres;') == 1)
        role_text = role_text.replace('CREATE ROLE postgres;\n', '', 1)
        target.inspect()
        execute(target.write_command('psql', ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq']), input=role_text)
        stage = 'restore-database'
        target.inspect()
        with archive.open('rb') as input_file:
            result = subprocess.run(target.write_command('pg_restore',
                        ['-d', 'tdf_hq', '--exit-on-error', '--single-transaction']), stdin=input_file,
                        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=180)
        require(result.returncode == 0)
        stage = 'verify-restoration'
        restored_counts = validate_counts(json.loads(execute(psql(target.target, read_only=True), input=COUNTS_SQL)))
        require(restored_counts == source_counts)
        restored = runtime.summarize_database(json.loads(execute(runtime.database_command(target.target), input=runtime.SQL)))
        require(restored['migrations'] == snapshot['database']['migrations'])
        after = runtime.inspect()
        require(after['containers']['db'] == snapshot['containers']['db'])
        require(after['database']['migrations'] == snapshot['database']['migrations'])
        receipt = {'schemaVersion': 1, 'status': 'isolated-database-restore-passed',
                   'sourceBackend': snapshot['publicBackend'], 'databaseImage': db['image'],
                   'sourceDatabaseBytes': size, 'archiveSha256': digest_file(archive),
                   'rolesSha256': digest_file(roles), 'archiveBytes': archive.stat().st_size,
                   'hostArchiveDirectory': str(directory), 'tableCounts': source_counts,
                   'migrationCount': len(restored['migrations']), 'productionDatabaseWritten': False,
                   'limitations': ['Online database snapshot only; assets and globals are not snapshot-coordinated.',
                       'No deployment, restore over production, payment, worker, or API canary was executed.',
                       'Role credentials excluded; secret recovery and off-host recovery are separate obligations.',
                       'Counts and successful archive replay are not byte-for-byte logical data equivalence.']}
        target.cleanup()
        target.target = None
        receipt['isolateRemoved'] = True
        (directory / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
        return receipt
    except Exception:
        # Preserve partial private archives for diagnosis without leaking SQL/data.
        (directory / 'failure.json').write_text(json.dumps({'stage': stage, 'complete': False}) + '\n')
        raise
    finally:
        try:
            if held is not None:
                held.close()
        finally:
            target.cleanup()
