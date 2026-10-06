#!/usr/bin/env python3
"""Create a private online logical archive, never claim coordinated recovery."""
import fcntl
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import resource
import stat
import subprocess
import sys
import types
import uuid

SOURCE_BYTES = {name: Path(__file__).with_name(name).read_bytes()
                for name in ('backup-postgres.py', 'inspect-runtime.py')}
LOADED_SOURCE_HASHES = {name: hashlib.sha256(value).hexdigest() for name, value in SOURCE_BYTES.items()}
runtime = types.ModuleType('runtime')
exec(compile(SOURCE_BYTES['inspect-runtime.py'], str(Path(__file__).with_name('inspect-runtime.py')), 'exec'), runtime.__dict__)
MAX_ARCHIVE = 2 * 1024 * 1024 * 1024
PENDING = 'logical-backup.pending.json'


def require(condition):
    if not condition:
        raise ValueError('Logical backup boundary rejected')


def private_directory(directory):
    info = directory.lstat()
    require(stat.S_ISDIR(info.st_mode) and info.st_uid == os.geteuid()
            and info.st_mode & 0o077 == 0)


def sync_directory(directory):
    fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def observe():
    ids = runtime.capture(runtime.DOCKER + ['ps', '--all', '--quiet', '--no-trunc',
        '--filter', 'label=com.docker.compose.project=tdf-production',
        '--filter', 'label=com.docker.compose.service=db']).split()
    require(len(ids) == 1)
    values = json.loads(runtime.capture(runtime.DOCKER + ['inspect', ids[0]]))
    require(len(values) == 1)
    source = runtime.summarize_container('db', values[0])
    require(source['running'] is True and source['containerId'] == ids[0])
    runtime.verify_storage(source['containerId'])
    return source


def database_command(source, program, arguments):
    require(program in ('pg_dump', 'pg_dumpall'))
    return runtime.DOCKER + ['exec', '-i', source['containerId'], 'env', '-i',
        'PATH=/usr/local/bin:/usr/bin:/bin', 'PGCONNECT_TIMEOUT=10',
        'PGOPTIONS=-c default_transaction_read_only=on', program,
        '-h', '/var/run/postgresql', '-p', '5432', '-U', 'postgres', *arguments]


def archive(command, destination):
    def limit():
        resource.setrlimit(resource.RLIMIT_FSIZE, (MAX_ARCHIVE, MAX_ARCHIVE))
    with destination.open('xb') as output:
        os.chmod(destination, 0o600)
        result = subprocess.run(command, stdout=output, stderr=subprocess.DEVNULL,
            timeout=240, env=runtime.COMMAND_ENV, preexec_fn=limit)
        require(result.returncode == 0)
        output.flush()
        os.fsync(output.fileno())
    require(0 < destination.stat().st_size <= MAX_ARCHIVE)


def digest(path):
    result = hashlib.sha256()
    with path.open('rb') as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b''):
            result.update(chunk)
    return result.hexdigest()


def source_fingerprints():
    return {name: digest(Path(__file__).with_name(name)) for name in SOURCE_BYTES}


def write_receipt(directory, name, value):
    partial = directory / (name + '.partial')
    with partial.open('x') as output:
        os.chmod(partial, 0o600)
        json.dump(value, output, sort_keys=True)
        output.write('\n')
        output.flush()
        os.fsync(output.fileno())
    os.link(partial, directory / name)  # Exclusive publication; never overwrite.
    sync_directory(directory)
    partial.unlink()
    sync_directory(directory)


def backup(directory):
    started = datetime.now(timezone.utc).isoformat()
    require(source_fingerprints() == LOADED_SOURCE_HASHES)
    private_directory(directory)
    descriptor = os.open(directory / 'backup.lock', os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    run = None
    stage = 'lock'
    try:
        info = os.fstat(descriptor)
        require(stat.S_ISREG(info.st_mode) and info.st_uid == os.geteuid()
                and info.st_nlink == 1 and info.st_mode & 0o077 == 0)
        fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        # A killed parent releases flock before its Docker request necessarily
        # finishes. No retry may admit more work while a prior request is uncertain.
        require(not os.path.lexists(directory / PENDING)
                and not os.path.lexists(directory / (PENDING + '.partial')))
        stage = 'source'
        source = observe()
        disk = os.statvfs(directory)
        require(disk.f_bavail * disk.f_frsize >= 2 * MAX_ARCHIVE)
        run = directory / ('logical-' + uuid.uuid4().hex)
        run.mkdir(mode=0o700)
        sync_directory(directory)
        reservation = {'run': run.name, 'containerId': source['containerId']}
        write_receipt(directory, PENDING, reservation)
        stage = 'database'
        archive(database_command(source, 'pg_dump', ['-d', 'tdf_hq', '-Fc']), run / 'database.pgdump')
        stage = 'roles'
        archive(database_command(source, 'pg_dumpall', ['-l', 'tdf_hq', '--globals-only']), run / 'globals.sql')
        stage = 'archive-list'
        with (run / 'database.pgdump').open('rb') as input_file:
            result = subprocess.run(runtime.DOCKER + ['exec', '-i', source['containerId'],
                'env', '-i', 'PATH=/usr/local/bin:/usr/bin:/bin', 'pg_restore', '--list'],
                stdin=input_file, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                timeout=60, env=runtime.COMMAND_ENV)
        require(result.returncode == 0)
        stage = 'source-recheck'
        require(observe() == source)
        stage = 'receipt'
        require(source_fingerprints() == LOADED_SOURCE_HASHES)
        receipt = {'schemaVersion': 1, 'status': 'logical-archive-created',
            'startedAt': started, 'completedAt': datetime.now(timezone.utc).isoformat(),
            'source': source, 'database': 'tdf_hq',
            'files': {name: {'sha256': digest(run / name), 'bytes': (run / name).stat().st_size}
                      for name in ('database.pgdump', 'globals.sql')},
            'toolSha256': LOADED_SOURCE_HASHES,
            'restored': False, 'offHost': False, 'coordinatedAssets': False,
            'limitations': 'Online database and globals are separate snapshots; assets, uploads, '
                'secrets and off-host recovery are not established. Archive listing is not a restore.'}
        write_receipt(run, 'complete.json', receipt)
        require(json.loads((directory/PENDING).read_text()) == reservation)
        (directory/PENDING).unlink()
        sync_directory(directory)
        return run
    except BaseException:
        if run is not None:
            # No raw diagnostics, SQL, configuration or credential text.
            write_receipt(run, 'failure.json', {'stage': stage, 'complete': False})
        raise
    finally:
        os.close(descriptor)  # The permanent lock inode is never unlinked.


if __name__ == '__main__':
    try:
        os.umask(0o077)
        require(os.geteuid() == 0 and len(sys.argv) == 1)
        backup(Path('/opt/tdf/backups'))
        print('Created private logical database/global archives; restore and off-host recovery not established.')
    except BaseException:
        print('Logical backup failed; inspect root-private evidence. No completion asserted.', file=sys.stderr)
        sys.exit(1)
