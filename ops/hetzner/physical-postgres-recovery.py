#!/usr/bin/env python3
"""Start an already verified cold PG17 copy in a bounded disk-backed isolate.

The caller owns the common rehearsal lock/durable reservation and writer fence.
This module never stops production, initializes a database, or restores in place.
"""
import hashlib
from contextlib import contextmanager
import importlib.util
import json
import os
from pathlib import Path
import re
import stat
import time


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


files = load('physical_recovery_files', 'recovery-files.py')
restore = load('physical_restore_common', 'rehearse-postgres-restore.py')
DATA = restore.DATA
CONFIG = '/recovery-config'
HOST_ROOT = Path('/opt/tdf/backups')
COMMAND = ['600']
USER = '999:999'
ENV = ['PATH=/usr/lib/postgresql/17/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin',
       'LANG=C', 'LC_ALL=C']
SETTINGS = """data_directory = '/var/lib/postgresql/data'
hba_file = '/recovery-config/pg_hba.conf'
ident_file = '/recovery-config/pg_ident.conf'
listen_addresses = '127.0.0.1'
port = 5432
unix_socket_directories = '/tmp'
shared_buffers = '16MB'
max_connections = 10
work_mem = '2MB'
max_wal_size = '64MB'
min_wal_size = '32MB'
max_locks_per_transaction = 1024
ssl = off
archive_mode = off
archive_command = ''
restore_command = ''
shared_preload_libraries = ''
session_preload_libraries = ''
local_preload_libraries = ''
logging_collector = off
"""
CONFIG_FILES = {'postgresql.conf': SETTINGS, 'pg_hba.conf':
                'local all all trust\nhost all all 127.0.0.1/32 trust\n', 'pg_ident.conf': ''}
POSTGRES = ['env', '-i', *ENV, 'postgres', '-D', DATA,
            '-c', 'config_file='+CONFIG+'/postgresql.conf']


def require(condition):
    if not condition:
        raise ValueError('Physical recovery boundary rejected')


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'))+'\n').encode()


def admit_mounts(text):
    """Canonical host uses its root filesystem, with no backup-path bind aliases.

    Device numbers alone cannot distinguish same-filesystem bind mounts. Check
    the host mount namespace before any clone mutation, including all ancestors.
    Privileged concurrent mount replacement is outside this sequential boundary.
    """
    require(isinstance(text, str) and 0 < len(text) <= 4*1024*1024)
    root_seen = False
    for line in text.splitlines():
        fields = line.split()
        require(len(fields) >= 10 and '-' in fields[6:])
        raw = fields[4]
        decoded = re.sub(r'\\([0-7]{3})', lambda match: chr(int(match[1], 8)), raw)
        require(decoded.startswith('/') and '\\' not in decoded)
        mount = Path(decoded)
        if mount == Path('/'):
            root_seen = True
        else:
            require(mount != HOST_ROOT and mount not in HOST_ROOT.parents and HOST_ROOT not in mount.parents)
    require(root_seen)


def verify_mounts():
    admit_mounts(Path('/proc/self/mountinfo').read_text())


def admit_cold_manifest(manifest):
    rows = files.validate_manifest(manifest)
    require(rows['']['uid'] == 999 and rows['']['gid'] == 999
            and rows['']['mode'] in (0o700, 0o750))
    for name in ('PG_VERSION', 'global/pg_control', 'postgresql.auto.conf'):
        require(name in rows and rows[name]['kind'] == 'file')
    for name in ('base', 'global', 'pg_wal', 'pg_tblspc'):
        require(name in rows and rows[name]['kind'] == 'directory')
    require(not any(name in rows for name in ('postmaster.pid', 'backup_label',
                  'tablespace_map', 'recovery.signal', 'standby.signal')))
    require(not any(name.startswith('pg_tblspc/') for name in rows))
    return rows


def control_identity(text, expected_system_id):
    require(isinstance(text, str) and len(text) <= 16384)
    require(re.fullmatch('[1-9][0-9]{0,19}', expected_system_id) is not None)
    values = {}
    for line in text.splitlines():
        require(':' in line)
        key, value = line.split(':', 1)
        require(key not in values)
        values[key] = value.strip()
    require(values.get('Database cluster state') == 'shut down')
    require(values.get('Database system identifier') == expected_system_id)
    require(values.get('pg_control version number') == '1700')
    return {'systemIdentifier': expected_system_id, 'state': 'shut down', 'majorVersion': 17}


class PhysicalClone(restore.IsolatedRestore):
    def __init__(self, source, image, image_id, nonce, directory, system_id, *, creation_records=None):
        super().__init__(source, image, image_id, nonce)
        require(Path(directory) == HOST_ROOT/('rehearsal-'+nonce))
        require(re.fullmatch('[1-9][0-9]{0,19}', system_id) is not None)
        self.directory, self.system_id = Path(directory), system_id
        self.data = self.directory/'physical-data'
        self.config = self.directory/'physical-config'
        self.prepared_manifest = None
        self.evidence = None
        self.start_attempted = False
        self.reservation_pid = None
        self.active_application = None
        self.creation_records = creation_records

    def require_application_owner(self, application):
        require(self.reservation_pid == os.getpid()
                and self.active_application is application
                and application.database is self and application.nonce == self.nonce
                and application.directory == self.directory)

    def admit_application_content(self, application, manifests):
        """Verify previously restored copies, without changing their metadata.

        The coordinator binds manifests to its trusted, retrieved bundle. This
        check establishes copy correspondence, not that capture was coherent.
        """
        self.require_application_owner(application)
        require(isinstance(manifests, dict) and set(manifests) == {'assets', 'uploads'})
        verify_mounts()
        evidence = {}
        with files.directory(str(self.directory), private=True):
            for name, manifest in manifests.items():
                rows = files.validate_manifest(manifest)
                # Image UID1000 must own and traverse/write the restored root.
                # Do not normalize ownership to hide a failed recovery check.
                require(rows['']['uid'] == 1000 and rows['']['gid'] == 1000
                        and rows['']['mode'] & 0o700 == 0o700)
                with files.directory(str(self.directory/('canary-'+name))) as fd:
                    require(files.walk(fd) == manifest)
                evidence[name] = {'manifestSha256': hashlib.sha256(canonical(manifest)).hexdigest(),
                                  'bytes': manifest['bytes'], 'entries': len(rows)}
        return evidence

    @contextmanager
    def with_application(self, application):
        """Register dependency before Docker creation; remove it before the DB.

        An uncertain cleanup deliberately leaves active_application set. The
        enclosing reservation must then preserve its database and durable marker.
        """
        require(self.reservation_pid == os.getpid() and self.target is not None
                and self.active_application is None)
        require(application.database is self and application.nonce == self.nonce
                and application.directory == self.directory and application.target is None
                and not application.creation_attempted and not application.paused)
        self.active_application = application
        try:
            yield application
        finally:
            self.require_application_owner(application)
            application.cleanup()
            require(application.target is None and not application.creation_attempted
                    and not application.paused)
            self.active_application = None

    def prepare(self, manifest):
        """Verify every copied byte first; record clone-only configuration changes."""
        require(self.prepared_manifest is None and self.target is None)
        verify_mounts()
        admit_cold_manifest(manifest)
        with files.directory(str(self.directory), private=True), files.directory(str(self.data)) as fd:
            require(files.walk(fd) == manifest)
            version = os.open('PG_VERSION', os.O_RDONLY | os.O_NOFOLLOW, dir_fd=fd)
            try: require(os.read(version, 16) == b'17\n')
            finally: os.close(version)
            # ALTER SYSTEM settings survive a physical copy. Never load the
            # original auto-conf or production config into the isolated clone.
            auto = os.open('postgresql.auto.conf', os.O_WRONLY | os.O_NOFOLLOW, dir_fd=fd)
            try:
                info = os.fstat(auto)
                require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1)
                os.ftruncate(auto, 0); os.fsync(auto)
            finally: os.close(auto)
            self.config.mkdir(mode=0o755)
            self.config.chmod(0o755)
            with files.directory(str(self.config)) as config_fd:
                for name, content in CONFIG_FILES.items():
                    output = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                     0o444, dir_fd=config_fd)
                    with os.fdopen(output, 'wb') as handle:
                        os.fchmod(handle.fileno(), 0o444)
                        handle.write(content.encode()); handle.flush(); os.fsync(handle.fileno())
                os.fsync(config_fd)
            os.fsync(fd)
            self.prepared_manifest = files.walk(fd)
        restore.sync_directory(self.directory)
        self.evidence = {'originalManifestHash': hashlib.sha256(canonical(manifest)).hexdigest(),
                         'cloneManifestHash': hashlib.sha256(canonical(self.prepared_manifest)).hexdigest(),
                         'configurationHash': hashlib.sha256(canonical(CONFIG_FILES)).hexdigest()}
        return dict(self.evidence)

    def verify_prepared(self):
        require(self.prepared_manifest is not None)
        verify_mounts()
        with files.directory(str(self.directory), private=True), files.directory(str(self.data)) as fd:
            require(files.walk(fd) == self.prepared_manifest)
        with files.directory(str(self.config)) as fd:
            require(set(os.listdir(fd)) == set(CONFIG_FILES))
            for name, content in CONFIG_FILES.items():
                handle = os.open(name, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=fd)
                try:
                    info = os.fstat(handle)
                    require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1
                            and stat.S_IMODE(info.st_mode) == 0o444 and info.st_uid == os.geteuid())
                    require(os.read(handle, 16384) == content.encode())
                finally: os.close(handle)

    def create_command(self):
        require(self.prepared_manifest is not None)
        return restore.DOCKER + ['create', '--pull=never', '--name', 'tdf-audit-restore-'+self.nonce,
            '--label', restore.LABEL+'='+self.nonce, '--network=none', '--read-only', '--restart=no',
            '--user', USER, '--entrypoint', '/bin/sleep', '--cap-drop=ALL',
            '--security-opt=no-new-privileges:true', '--memory='+str(restore.MEMORY_LIMIT),
            '--memory-swap='+str(restore.MEMORY_LIMIT), '--cpus=0.5', '--pids-limit=64',
            '--tmpfs', '/tmp:rw,nosuid,nodev,size=16777216',
            '--mount', 'type=bind,src='+str(self.data)+',dst='+DATA,
            '--mount', 'type=bind,src='+str(self.config)+',dst='+CONFIG+',readonly',
            self.image, *COMMAND]

    def admit(self, container):
        target = container['Id']
        require(re.fullmatch('[a-f0-9]{64}', target) is not None and target != self.source)
        require(self.target is None or self.target == target)
        cfg, host = container['Config'], container['HostConfig']
        require(host.get('RestartPolicy') == {'Name':'no','MaximumRetryCount':0} and host.get('AutoRemove') is False)
        require(cfg['Labels'].get(restore.LABEL) == self.nonce and cfg['Image'] == self.image
                and container['Image'] == self.image_id and cfg['User'] == USER
                and cfg['Entrypoint'] == ['/bin/sleep'] and cfg['Cmd'] == COMMAND)
        require(host['NetworkMode'] == 'none' and set(container['NetworkSettings']['Networks']) == {'none'})
        require(host['ReadonlyRootfs'] and host['Memory'] == restore.MEMORY_LIMIT
                and host['MemorySwap'] == restore.MEMORY_LIMIT and host['NanoCpus'] == 500000000
                and host['PidsLimit'] == 64 and host['CapDrop'] == ['ALL'])
        require(not any(host.get(k) for k in ('Privileged', 'PortBindings', 'Devices', 'CapAdd',
                'VolumesFrom', 'Binds', 'PidMode', 'UTSMode')))
        require(host['IpcMode'] == 'private' and 'no-new-privileges:true' in host['SecurityOpt'])
        require(host['Tmpfs'] == {'/tmp': 'rw,nosuid,nodev,size=16777216'})
        mounts = {m['Destination']: m for m in container['Mounts']}
        # Docker may omit tmpfs from Mounts before/after startup; its complete
        # declaration is independently required in HostConfig above.
        require(len(mounts) == len(container['Mounts']) and set(mounts) in
                ({DATA, CONFIG}, {DATA, CONFIG, '/tmp'}))
        for path, destination, writable in ((self.data, DATA, True), (self.config, CONFIG, False)):
            m = mounts[destination]
            require(m['Type'] == 'bind' and m['Source'] == str(path) and m['RW'] is writable
                    and m['Propagation'] == 'rprivate')
        require('/tmp' not in mounts or mounts['/tmp']['Type'] == 'tmpfs')
        self.target = target
        return target

    def start(self):
        require(self.reservation_pid == os.getpid())
        require(self.target is None and not self.creation_attempted and not self.start_attempted)
        self.verify_prepared()
        if self.creation_records is not None:
            self.creation_records.publish('physical-database', self)
        self.creation_attempted = True
        target = restore.execute(self.create_command()).strip()
        require(re.fullmatch('[a-f0-9]{64}', target) is not None)
        # Full inspection must precede any startup or write.
        values = json.loads(restore.execute(restore.DOCKER+['inspect', target]))
        require(len(values) == 1); self.admit(values[0])
        restore.execute(restore.DOCKER+['start', self.target])
        self.inspect()
        control = restore.execute(restore.DOCKER+['exec', self.target, 'env', '-i', *ENV,
                  'pg_controldata', '-D', DATA])
        cold = control_identity(control, self.system_id)
        self.verify_prepared()
        self.start_attempted = True  # an uncertain daemon response cannot authorize retry
        restore.execute(restore.DOCKER+['exec', '--detach', self.target, *POSTGRES])
        for _ in range(30):
            self.inspect()
            try:
                value = restore.execute(self.write_command('psql', ['-X', '-qAt', '-v',
                    'ON_ERROR_STOP=1', '-d', 'postgres']), input="SELECT system_identifier::text || ':' || "
                    "current_setting('server_version_num') FROM pg_control_system();", timeout=5).strip()
                require(re.fullmatch(re.escape(self.system_id)+r':17[0-9]{4}', value) is not None)
                return {**self.evidence, **cold, 'status': 'physical-clone-started',
                        'initializedDatabase': False, 'productionDatabaseWritten': False}
            except (ValueError, TimeoutError):
                time.sleep(1)
        require(False)

    @contextmanager
    def reserved(self):
        """Serialize with logical restores and retain uncertain daemon work."""
        require(self.reservation_pid is None and not self.creation_attempted
                and self.active_application is None)
        coordinated = self.creation_records is not None and hasattr(self.creation_records, 'reserve')
        with files.directory(str(HOST_ROOT), private=True), restore.rehearsal_lock(HOST_ROOT) as descriptor:
            require(not os.path.lexists(HOST_ROOT/restore.PENDING_NAME))
            for label in (restore.LABEL, 'net.tdf.application-canary'):
                require(restore.execute(restore.DOCKER+['ps', '--all', '--quiet',
                        '--filter', 'label='+label], timeout=10).strip() == '')
            memory = next(int(line.split()[1])*1024 for line in Path('/proc/meminfo').read_text().splitlines()
                          if line.startswith('MemAvailable:'))
            disk = os.statvfs(HOST_ROOT)
            require(memory >= 1024**3 and disk.f_bavail*disk.f_frsize >= 2*1024**3)
            if coordinated:
                self.creation_records.reserve(self, descriptor)
            else:
                restore.reserve_creation(HOST_ROOT, self.nonce, self.image)
            self.reservation_pid = os.getpid()
            try:
                yield self
            finally:
                try:
                    require(self.reservation_pid == os.getpid())
                    self.reservation_pid = None
                    # Do not destroy a dependent application's DB or release its
                    # reservation after an uncertain application cleanup.
                    require(self.active_application is None)
                    self.cleanup()
                    require(self.target is None and not self.creation_attempted)
                    if coordinated:
                        self.creation_records.release(self)
                    else:
                        restore.release_creation(HOST_ROOT, self.nonce, self.image)
                finally:
                    if coordinated:self.creation_records.close()

    def write_command(self, program, arguments):
        require(self.target is not None and self.target != self.source)
        command = restore.local_database(self.target, program, arguments, read_only=False)
        # The clone's fixed configuration uses only its private /tmp socket.
        command[command.index('/var/run/postgresql')] = '/tmp'
        return command
