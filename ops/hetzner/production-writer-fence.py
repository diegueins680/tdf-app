#!/usr/bin/env python3
"""Journaled shutdown of the canonical Docker services and registered TDF timer.

This library has real stop effects but no CLI, restart, deployment or recovery
entrypoint. The release coordinator must hold the shared restore reservation,
retain legacy application descriptors and establish the host worker inventory
before invoking it. Privileged noncooperating host writers remain excluded.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat
import subprocess


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module


sources = load('fence_sources', 'production-recovery-sources.py')
files = load('fence_files', 'recovery-files.py')
quarantine = load('fence_quarantine', 'outbound-quarantine.py')
SERVICE = 'tdf-postgres-backup.service'
TIMER = 'tdf-postgres-backup.timer'
UNITS = frozenset((SERVICE, TIMER))
DIRECTORY = Path('/etc/systemd/system')
ENV = {'PATH': '/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8', 'SYSTEMD_COLORS': '0'}
PROPERTIES = ('Id', 'LoadState', 'ActiveState', 'SubState', 'UnitFileState',
              'FragmentPath', 'DropInPaths', 'NeedDaemonReload', 'Job', 'Transient')


def require(value):
    if not value: raise ValueError('Production writer fence rejected')


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':'))+'\n').encode()


def execute(command):
    result = subprocess.run(command, env=ENV, text=True, capture_output=True, timeout=90)
    # Neither a Docker configuration nor a failed unit's command output is public.
    require(result.returncode == 0 and len(result.stdout) <= 4*1024*1024)
    return result.stdout


def parse_properties(text):
    require(isinstance(text, str) and len(text) <= 16384)
    result = {}
    for line in text.splitlines():
        key, separator, value = line.partition('=')
        require(separator and key in PROPERTIES and key not in result)
        result[key] = value
    require(set(result) == set(PROPERTIES))
    return result


def admit_units(rows, expected_hashes, *, timer_stopped):
    """Unit configuration and scheduled state, not arbitrary host process authority."""
    require(isinstance(rows, dict) and set(rows) == UNITS
            and isinstance(expected_hashes, dict) and set(expected_hashes) == UNITS
            and type(timer_stopped) is bool)
    for name, row in rows.items():
        require(set(row) == {'properties', 'sha256', 'uid', 'mode', 'bytes'})
        p = row['properties']
        require(set(p) == set(PROPERTIES) and p['Id'] == name and p['LoadState'] == 'loaded'
                and p['FragmentPath'] == str(DIRECTORY/name) and not p['DropInPaths']
                and p['NeedDaemonReload'] == 'no' and not p['Job'] and p['Transient'] == 'no')
        require(type(row['uid']) is int and row['uid'] == 0 and row['mode'] == 0o644
                and type(row['bytes']) is int and 0 < row['bytes'] <= 16384
                and re.fullmatch('[a-f0-9]{64}', expected_hashes[name])
                and row['sha256'] == expected_hashes[name])
        require(p['UnitFileState'] == ('enabled' if name == TIMER else 'static'))
        active = name == TIMER and not timer_stopped
        require(p['ActiveState'] == ('active' if active else 'inactive')
                and p['SubState'] == ('waiting' if active else 'dead'))
    return {'timerStopped': timer_stopped, 'backupServiceInactive': True,
            'unitConfigurationSha256': hashlib.sha256(canonical(expected_hashes)).hexdigest(),
            'scope': 'Registered TDF systemd units only; unrelated host process authority is excluded'}


def observe_unit_inventory(expected):
    # The loaded and installed inventories independently reject unknown TDF units.
    for operation in ('list-units', 'list-unit-files'):
        text = execute(['systemctl', operation, '--all', '--plain', '--no-legend', '--no-pager', 'tdf*'])
        names = [line.split()[0] for line in text.splitlines() if line.strip()]
        require(len(names) == len(set(names)) and set(names) == expected)


def observe_backup_units(expected_hashes, *, timer_stopped):
    rows = {}
    for name in sorted(UNITS):
        properties = parse_properties(execute(['systemctl', 'show', name,
                                              '--property='+','.join(PROPERTIES)]))
        with files.directory(str(DIRECTORY)) as parent:
            fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
            try:
                info = os.fstat(fd)
                require(stat.S_ISREG(info.st_mode) and info.st_nlink == 1 and info.st_size <= 16384)
                data = os.read(fd, 16385)
                require(len(data) == info.st_size and files.identity(os.fstat(fd)) == files.identity(info))
                named = os.stat(name, dir_fd=parent, follow_symlinks=False)
                require(files.identity(named) == files.identity(info))
                rows[name] = {'properties': properties, 'sha256': hashlib.sha256(data).hexdigest(),
                              'uid': info.st_uid, 'mode': stat.S_IMODE(info.st_mode), 'bytes': len(data)}
            finally: os.close(fd)
    return admit_units(rows, expected_hashes, timer_stopped=timer_stopped)


def observe_units(expected_hashes, *, timer_stopped):
    observe_unit_inventory(UNITS)
    result=observe_backup_units(expected_hashes,timer_stopped=timer_stopped)
    observe_unit_inventory(UNITS)
    return result


def observe_restricted_units(expected_hashes, restriction_hash, *, timer_stopped):
    """Read-only exact quarantine unit admission, not an install/start adapter.

    The caller must authenticate restriction_hash against its immutable release
    authority. Never derive it from the observation being admitted.
    """
    require(isinstance(restriction_hash,str) and re.fullmatch('[a-f0-9]{64}',restriction_hash))
    before=quarantine.observe_persistent()
    require(before['persistentConfigurationSha256']==restriction_hash)
    expected=UNITS | {quarantine.UNIT}
    observe_unit_inventory(expected)
    result=observe_backup_units(expected_hashes,timer_stopped=timer_stopped)
    observe_unit_inventory(expected)
    require(quarantine.observe_persistent()==before)
    return {**result,'restriction':before,'stopAuthorized':False,'recoveryAuthorized':False}


class WriterFence:
    """Three ordered journal effects. Failure never starts another service.

    Construction/observation have no stop effect. There is deliberately no retry
    or automatic rollback after an interrupted phase: its durable intent remains
    unresolved. The coordinator owns recovery and restoration of timer state.
    """
    def __init__(self, journal, expected, configuration_hash, unit_hashes, legacy_root=None):
        require(re.fullmatch('[a-f0-9]{64}', configuration_hash))
        self.journal, self.expected = journal, json.loads(json.dumps(expected))
        self.configuration_hash, self.unit_hashes = configuration_hash, dict(unit_hashes)
        self.legacy_root = legacy_root
        self.stopped = frozenset()
        self.timer_stopped = False
        self.used = set()
        self.owner = os.getpid()
        self.target_hash = hashlib.sha256(canonical({'containers': self.expected,
            'configurationSha256': configuration_hash, 'units': self.unit_hashes})).hexdigest()

    def observe(self):
        require(os.getpid() == self.owner)
        self.journal.guard()
        # The ordinary path must never consume an exceptional legacy plan.
        # A separately qualified restricted adapter is required for version2.
        require('legacyStopPolicyHash' not in self.journal.records()[0]['event']['plan'])
        result = sources.observe(self.expected, stopped_services=self.stopped)
        require(result['runtimeConfigurationSha256'] == self.configuration_hash)
        units = observe_units(self.unit_hashes, timer_stopped=self.timer_stopped)
        return {'sources': result, 'units': units,
                'databaseCleanShutdownVerified': False, 'hostWorkerInventoryVerified': False}

    def _stop_container(self, service):
        self.observe()
        require(service not in self.stopped)
        target = self.expected[service]['containerId']
        require(re.fullmatch('[a-f0-9]{64}', target))
        output = execute(sources.inspector.DOCKER+['stop', '--timeout', '60', target]).strip()
        require(output == target)
        # Exit137/OOM/restart/configuration changes reject subsequent phases.
        self.stopped = self.stopped | {service}
        self.observe()

    def _perform(self, phase, effect):
        require(os.getpid() == self.owner and phase not in self.used)
        self.observe()
        self.used.add(phase)
        def observed(context):
            effect()
            evidence = self.observe()
            return {**context, 'evidenceHash': hashlib.sha256(canonical(evidence)).hexdigest()}
        return self.journal.perform(phase, self.target_hash, observed)

    def maintenance(self):
        require(not self.stopped and not self.timer_stopped)
        def effect():
            execute(['systemctl', 'stop', TIMER])
            self.timer_stopped = True
            # An already dispatched backup service must finish independently;
            # an active job rejects here before stopping ingress or the database.
            self.observe()
            self._stop_container('edge')
        return self._perform('maintenance', effect)

    def stop_writers(self):
        require(self.stopped == {'edge'} and self.timer_stopped)
        def effect():
            sampled = self.observe()
            if sampled['sources']['legacyUploads']:
                require(self.legacy_root is not None)
                self.legacy_root.guard()
                require(self.legacy_root.target == self.expected['api']['containerId'])
                self.legacy_root.inspect(running=True)
            self._stop_container('api')
            if sampled['sources']['legacyUploads']:
                self.legacy_root.inspect(running=False)
        return self._perform('stop-writers', effect)

    def stop_database(self):
        require(self.stopped == {'edge', 'api'} and self.timer_stopped)
        return self._perform('stop-database', lambda: self._stop_container('db'))
