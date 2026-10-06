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
PENDING_NAME = 'restore-rehearsal.pending.json'
DATA = '/var/lib/postgresql/data'
MAX_DATABASE = 128 * 1024 * 1024
MAX_ARCHIVE = 256 * 1024 * 1024
MEMORY_LIMIT = 384 * 1024 * 1024
POSTGRES_COMMAND = ['postgres', '-c', 'shared_buffers=16MB', '-c', 'max_connections=10',
                    '-c', 'work_mem=2MB', '-c', 'max_wal_size=64MB', '-c', 'min_wal_size=32MB',
                    '-c', 'max_locks_per_transaction=1024']
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
                self.image, *POSTGRES_COMMAND]

    def admit(self, container):
        target = container.get('Id')
        require(isinstance(target, str) and re.fullmatch(r'[a-f0-9]{64}', target))
        require(target != self.source and (self.target is None or self.target == target))
        require(container['Config']['Labels'].get(LABEL) == self.nonce)
        require(container['Image'] == self.image_id and container['Config']['Image'] == self.image)
        require(container['Config']['Cmd'] == POSTGRES_COMMAND)
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
        yield descriptor
    finally:
        os.close(descriptor)


def validate_candidate(candidate, ledger, *, complete=False):
    require(candidate['schemaVersion'] == 1)
    require(re.fullmatch(r'[a-f0-9]{40}', candidate['sourceRevision']))
    require(re.fullmatch(r'[a-f0-9]{64}', candidate['manifestSha256']))
    require(hashlib.sha256(candidate['sql'].encode()).hexdigest() == candidate['sqlSha256'])
    require(0 < len(candidate['sql']) <= 8 * 1024 * 1024)
    expected = {}
    for row in candidate['migrations']:
        require(re.fullmatch(r'[a-zA-Z0-9][a-zA-Z0-9_-]*', row['id']) and row['id'] not in expected)
        require(re.fullmatch(r'[a-f0-9]{64}', row['checksum']))
        compatible = row['compatibleAppliedChecksums']
        require(isinstance(compatible, list) and all(re.fullmatch(r'[a-f0-9]{64}', value) for value in compatible))
        expected[row['id']] = {row['checksum'], *compatible}
    require(0 < len(expected) <= 1000)
    observed = set()
    for row in ledger:
        key = row['migration_id']
        require(key in expected and key not in observed and row['checksum'] in expected[key])
        require(re.fullmatch(r'[a-f0-9]{40}', row['source_commit']))
        observed.add(key)
    if complete:
        require(observed == set(expected))
    return len(expected) - len(observed)


DATABASE_CONTROLS = ('revenueFlags', 'providerAccounts', 'socialRuntime',
                     'merchReputationFlags', 'missingMerchReputationFlags',
                     'eventOperationFlags', 'interactionRuntime', 'interactionEntityKinds')


def observe_database(runtime, container_id):
    # Keep restore verification compatible with the collector's optional table
    # boundary. SQL alone deliberately cannot reference an absent event table.
    data = json.loads(execute(runtime.database_command(container_id), input=runtime.SQL))
    data['eventOperationFlags'] = runtime.optional_event_flags(container_id)
    return runtime.summarize_database(data)


def rehearse_candidate(runtime, target, directory, candidate, restored):
    pending = validate_candidate(candidate, restored['migrations'])
    sql_file = directory / 'candidate-migrations.sql'
    with sql_file.open('x') as output:
        os.chmod(sql_file, 0o600)
        output.write(candidate['sql'])
    def diagnostic_limit():
        resource.setrlimit(resource.RLIMIT_FSIZE, (4 * 1024 * 1024, 4 * 1024 * 1024))
    after = None
    for attempt in (1, 2):
        target.inspect()
        with sql_file.open('rb') as input_file, (directory / ('migration-' + str(attempt) + '.log')).open('xb') as output:
            os.chmod(output.name, 0o600)
            result = subprocess.run(target.write_command('psql',
                         ['-X', '-qAt', '-v', 'ON_ERROR_STOP=1', '-d', 'tdf_hq']), stdin=input_file,
                         stdout=output, stderr=output, timeout=180, preexec_fn=diagnostic_limit)
        require(result.returncode == 0)
        after = observe_database(runtime, target.target)
        validate_candidate(candidate, after['migrations'], complete=True)
        # Existing applied history must survive unchanged; new entries bind the
        # immutable candidate. This rejects ledger rewriting during rehearsal.
        old = {row['migration_id']: row for row in restored['migrations']}
        for row in after['migrations']:
            if row['migration_id'] in old:
                require(row == old[row['migration_id']])
            else:
                require(row['source_commit'] == candidate['sourceRevision'])
        if attempt == 1:
            first = after
        else:
            require(after == first)
    return {'sourceRevision': candidate['sourceRevision'], 'manifestSha256': candidate['manifestSha256'],
            'sqlSha256': candidate['sqlSha256'], 'applications': 2, 'pendingBefore': pending,
            'migrationCount': len(after['migrations']), 'schemaVerification': 'passed by canonical batch',
            'controlChanges': {key: {'before': restored[key], 'after': after[key]}
                               for key in DATABASE_CONTROLS if restored[key] != after[key]},
            'deploymentAuthorized': False}


def rehearse(runtime, candidate=None, *, canary_module=None, canary_image=None):
    require(os.geteuid() == 0)
    with rehearsal_lock(Path('/opt/tdf/backups')):
        return rehearse_locked(runtime, candidate, canary_module=canary_module, canary_image=canary_image)


def sync_directory(directory):
    descriptor = os.open(directory, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def reserve_creation(directory, nonce, image):
    # Persist uncertainty before sending any external create request. Docker can
    # finish a request after the client/process dies and its flock is released.
    marker = directory / PENDING_NAME
    descriptor = os.open(marker, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600)
    with os.fdopen(descriptor, 'w') as output:
        json.dump({'nonce': nonce, 'image': image}, output)
        output.flush()
        os.fsync(output.fileno())
    sync_directory(directory)


def release_creation(directory, nonce, image):
    marker = directory / PENDING_NAME
    info = marker.lstat()
    require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1 and
            info.st_uid == os.geteuid() and info.st_mode & 0o077 == 0)
    require(json.loads(marker.read_text()) == {'nonce': nonce, 'image': image})
    marker.unlink()
    sync_directory(directory)


def rehearse_locked(runtime, candidate=None, *, canary_module=None, canary_image=None):
    require((canary_module is None) == (canary_image is None))
    require(canary_image is None or candidate is not None)
    # flock is released by process death, but Docker containers survive it.
    # Inspect stopped containers too; never stack another memory reservation on
    # an unresolved run or automatically delete a target without its admission.
    require(not os.path.lexists(Path('/opt/tdf/backups') / PENDING_NAME))
    require(execute(DOCKER + ['ps', '--all', '--quiet', '--filter', 'label=' + LABEL], timeout=10).strip() == '')
    require(execute(DOCKER + ['ps', '--all', '--quiet', '--filter', 'label=net.tdf.application-canary'], timeout=10).strip() == '')
    # runtime is the reviewed read-only collector bundled by the launcher.
    snapshot = runtime.inspect()
    db = snapshot['containers']['db']
    size = int(execute(psql(db['containerId'], read_only=True), input=CAPACITY_SQL))
    mem = {line.split(':')[0]: int(line.split()[1]) * 1024
           for line in Path('/proc/meminfo').read_text().splitlines() if line.startswith('MemAvailable:')}
    disk = os.statvfs('/opt/tdf')
    minimum_memory = (2 if canary_image is not None else 1) * 1024 * 1024 * 1024
    require(0 < size <= MAX_DATABASE and mem['MemAvailable'] >= minimum_memory)
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
    reservation = False
    application = None
    def cleanup():
        nonlocal reservation
        if application is not None:
            application.cleanup()
            require(not application.creation_attempted and not application.paused)
        target.cleanup()
        if reservation:
            require(not target.creation_attempted)
            release_creation(directory.parent, nonce, db['image'])
            reservation = False
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
        reserve_creation(directory.parent, nonce, db['image'])
        reservation = True
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
        # Errors may contain SQL/data. Retain only on the private host, never
        # in remote output or receipts, with a hard file-size bound.
        def diagnostic_limit():
            resource.setrlimit(resource.RLIMIT_FSIZE, (4 * 1024 * 1024, 4 * 1024 * 1024))
        with archive.open('rb') as input_file, (directory / 'restore.stderr').open('xb') as diagnostic:
            os.chmod(directory / 'restore.stderr', 0o600)
            result = subprocess.run(target.write_command('pg_restore',
                        ['-d', 'tdf_hq', '--exit-on-error', '--single-transaction']), stdin=input_file,
                        stdout=subprocess.DEVNULL, stderr=diagnostic, timeout=180, preexec_fn=diagnostic_limit)
        require(result.returncode == 0)
        stage = 'verify-restoration'
        restored_counts = validate_counts(json.loads(execute(psql(target.target, read_only=True), input=COUNTS_SQL)))
        require(restored_counts == source_counts)
        restored = observe_database(runtime, target.target)
        require(restored['migrations'] == snapshot['database']['migrations'])
        candidate_result = None
        if candidate is not None:
            stage = 'candidate-migrations'
            candidate_result = rehearse_candidate(runtime, target, directory, candidate, restored)
        canary_result = None
        if canary_image is not None:
            stage = 'isolated-application-canary'
            application = canary_module.Canary(restore=__import__('types').SimpleNamespace(DOCKER=DOCKER),
                target=target, directory=directory, image=canary_image, revision=candidate['sourceRevision'])
            canary_result = application.run()
        after = runtime.inspect()
        require(after['containers']['db'] == snapshot['containers']['db'])
        require(after['database']['migrations'] == snapshot['database']['migrations'])
        receipt = {'schemaVersion': 1, 'status': 'isolated-database-restore-passed',
                   'sourceBackend': snapshot['publicBackend'], 'databaseImage': db['image'],
                   'sourceDatabaseBytes': size, 'archiveSha256': digest_file(archive),
                   'rolesSha256': digest_file(roles), 'archiveBytes': archive.stat().st_size,
                   'hostArchiveDirectory': str(directory), 'tableCounts': source_counts,
                   'migrationCount': len(restored['migrations']), 'productionDatabaseWritten': False,
                   'candidateMigrations': candidate_result, 'applicationCanary': canary_result,
                   'limitations': ['Online database snapshot only; assets and globals are not snapshot-coordinated.',
                       'No deployment, restore over production or provider transaction; canary evidence, if requested, is separately scoped.',
                       'Role credentials excluded; secret recovery and off-host recovery are separate obligations.',
                       'Counts and successful archive replay are not byte-for-byte logical data equivalence.']}
        cleanup()
        target.target = None
        receipt['isolateRemoved'] = True
        if canary_result is not None: canary_result['applicationRemoved'] = True
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
            cleanup()
