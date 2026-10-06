#!/usr/bin/env python3
"""Real journal/crash-prefix tests with synthetic Docker/systemd observations.

No production stop, daemon command, namespace capture or database proof occurs.
"""
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    return module


w = load('writer_fence', ROOT/'ops/hetzner/production-writer-fence.py')
j = load('fence_journal', ROOT/'ops/hetzner/release-journal.py')
s = load('fence_fixture', ROOT/'scripts/test-production-recovery-sources.py')
HASHES = {name: str(index)*64 for index, name in enumerate(sorted(w.UNITS), 1)}
PLAN = {key: ('sha256:'+'1'*64 if key.endswith('Image') else '1'*(40 if key.endswith('Revision') else 64))
        for key in j.PLAN_KEYS}


def units(stopped=False):
    rows = {}
    for name in w.UNITS:
        active = name == w.TIMER and not stopped
        p = dict(Id=name, LoadState='loaded', ActiveState='active' if active else 'inactive',
                 SubState='waiting' if active else 'dead', UnitFileState='enabled' if name == w.TIMER else 'static',
                 FragmentPath=str(w.DIRECTORY/name), DropInPaths='', NeedDaemonReload='no', Job='', Transient='no')
        rows[name] = dict(properties=p, sha256=HASHES[name], uid=0, mode=0o644, bytes=200)
    return rows


class SyntheticHost:
    def __init__(self):
        self.containers, self.volumes, self.expected = s.fixture()
        self.rows = units()
        self.commands = []
        self.lost_edge_response = False
        self.backup_race = False
        self.target = self.expected['api']['containerId']
        self.legacy_checks = []

    def sources(self, expected, **kwargs):
        return w.sources.admit(self.containers, self.volumes, expected, **kwargs)

    def unit_observer(self, hashes, **kwargs):
        return w.admit_units(self.rows, hashes, **kwargs)

    def guard(self):
        self.legacy_checks.append('guard')

    def inspect(self, *, running):
        self.legacy_checks.append(running)
        assert self.containers[0]['State']['Running'] is running

    def execute(self, command):
        self.commands.append(command)
        if command == ['systemctl', 'stop', w.TIMER]:
            self.rows[w.TIMER]['properties'].update(ActiveState='inactive', SubState='dead')
            if self.backup_race: self.rows[w.SERVICE]['properties'].update(ActiveState='activating', SubState='start')
            return ''
        prefix = w.sources.inspector.DOCKER+['stop', '--timeout', '60']
        assert command[:-1] == prefix
        target = command[-1]
        container = next(c for c in self.containers if c['Id'] == target)
        container['State'].update(Running=False, Pid=0, Status='exited', ExitCode=0)
        if self.lost_edge_response and target == self.expected['edge']['containerId']:
            raise TimeoutError('synthetic uncertain daemon response')
        return target+'\n'


class WriterFenceTests(unittest.TestCase):
    def test_unit_configuration_job_and_state_negative_controls(self):
        w.admit_units(units(), HASHES, timer_stopped=False)
        w.admit_units(units(True), HASHES, timer_stopped=True)
        for field, value in [('FragmentPath','/tmp/replaced.service'), ('DropInPaths','/tmp/override.conf'),
                             ('NeedDaemonReload','yes'), ('Job','42'), ('Transient','yes'),
                             ('LoadState','not-found'), ('UnitFileState','enabled'), ('ActiveState','activating')]:
            rows=units();rows[w.SERVICE]['properties'][field]=value
            with self.assertRaises(ValueError): w.admit_units(rows,HASHES,timer_stopped=False)
        for field,value in [('sha256','f'*64),('uid',1000),('mode',0o666),('bytes',0)]:
            rows=units();rows[w.TIMER][field]=value
            with self.assertRaises(ValueError): w.admit_units(rows,HASHES,timer_stopped=False)
        with self.assertRaises(ValueError): w.admit_units(units(),HASHES,timer_stopped=True)

    def test_property_parser_rejects_missing_duplicate_and_unexpected_fields(self):
        row=units()[w.SERVICE]['properties'];text='\n'.join(k+'='+v for k,v in row.items())
        self.assertEqual(w.parse_properties(text),row)
        for invalid in (text+'\nJob=',text+'\nUnexpected=secret',text.replace('Job=\n','')):
            with self.assertRaises(ValueError):w.parse_properties(invalid)

    def test_ordinary_inventory_still_denies_quarantine_and_unknown_units(self):
        for names in (w.UNITS|{w.quarantine.UNIT},w.UNITS|{'tdf-unknown.service'}):
            with patch.object(w,'execute',return_value='\n'.join(sorted(names))),patch.object(w,'observe_backup_units') as backup:
                with self.assertRaises(ValueError):w.observe_units(HASHES,timer_stopped=False)
                backup.assert_not_called()

    def test_restricted_inventory_requires_exact_live_guard_and_closing_equality(self):
        receipt={'persistentConfigurationSha256':'a'*64,'hostBypassAdmissionVerified':False}
        inventory='\n'.join(sorted(w.UNITS|{w.quarantine.UNIT}))
        with patch.object(w.quarantine,'observe_persistent',return_value=receipt),patch.object(w,'execute',return_value=inventory),patch.object(w,'observe_backup_units',return_value={'timerStopped':True}) as backup:
            result=w.observe_restricted_units(HASHES,'a'*64,timer_stopped=True)
            self.assertFalse(result['stopAuthorized']);self.assertFalse(result['recoveryAuthorized'])
            backup.assert_called_once_with(HASHES,timer_stopped=True)
        with patch.object(w.quarantine,'observe_persistent',side_effect=[receipt,{**receipt,'persistentConfigurationSha256':'b'*64}]),patch.object(w,'execute',return_value=inventory),patch.object(w,'observe_backup_units',return_value={}):
            with self.assertRaises(ValueError):w.observe_restricted_units(HASHES,'a'*64,timer_stopped=False)
        with patch.object(w.quarantine,'observe_persistent',return_value=receipt),patch.object(w,'execute') as execute:
            with self.assertRaises(ValueError):w.observe_restricted_units(HASHES,'b'*64,timer_stopped=False)
            execute.assert_not_called()

    def test_restricted_unit_inventory_rejects_missing_extra_duplicate_or_drift(self):
        receipt={'persistentConfigurationSha256':'a'*64}
        names=sorted(w.UNITS|{w.quarantine.UNIT});valid='\n'.join(names)
        for invalid in ('\n'.join(sorted(w.UNITS)),valid+'\ntdf-unknown.service',valid+'\n'+names[0]):
            with patch.object(w.quarantine,'observe_persistent',return_value=receipt),patch.object(w,'execute',return_value=invalid),patch.object(w,'observe_backup_units') as backup:
                with self.assertRaises(ValueError):w.observe_restricted_units(HASHES,'a'*64,timer_stopped=False)
                backup.assert_not_called()
        for responses in ([valid,valid+'\ntdf-unknown.service'],[valid,valid,valid,valid+'\ntdf-unknown.service']):
            with patch.object(w.quarantine,'observe_persistent',return_value=receipt),patch.object(w,'execute',side_effect=responses),patch.object(w,'observe_backup_units',return_value={}):
                with self.assertRaises(ValueError):w.observe_restricted_units(HASHES,'a'*64,timer_stopped=False)

    def test_partial_docker_fences_admit_only_declared_stopped_services(self):
        host=SyntheticHost();before=host.sources(host.expected)['runtimeConfigurationSha256']
        stopped=frozenset()
        for service in ('edge','api','db'):
            host.execute(w.sources.inspector.DOCKER+['stop','--timeout','60',host.expected[service]['containerId']])
            with self.assertRaises(ValueError):host.sources(host.expected,stopped_services=stopped)
            stopped=stopped|{service}
            result=host.sources(host.expected,stopped_services=stopped)
            self.assertEqual(result['runtimeConfigurationSha256'],before)
            self.assertIs(result['dockerWritersStopped'],len(stopped)==3)
        for invalid in ({'edge'},frozenset({'unregistered'})):
            with self.assertRaises(ValueError):host.sources(host.expected,stopped_services=invalid)

    def run_host(self, check):
        host=SyntheticHost()
        with tempfile.TemporaryDirectory() as d, j.open_journal(str(Path(d).resolve())) as journal:
            journal.initialize(PLAN,'1'*32)
            config=host.sources(host.expected)['runtimeConfigurationSha256']
            fence=w.WriterFence(journal,host.expected,config,HASHES,host)
            with patch.object(w.sources,'observe',host.sources), patch.object(w,'observe_units',host.unit_observer), patch.object(w,'execute',host.execute):
                check(host,journal,fence)

    def test_real_journal_orders_effects_and_never_reports_clean_database(self):
        def check(host,journal,fence):
            fence.maintenance();fence.stop_writers();fence.stop_database()
            self.assertEqual(journal.status()['completedStages'],list(j.STAGES[:3]))
            self.assertFalse(journal.status()['newWritesPossible'])
            self.assertEqual([c[-1] for c in host.commands],[w.TIMER,*[host.expected[s]['containerId'] for s in ('edge','api','db')]])
            self.assertEqual(host.legacy_checks,['guard',True,False])
            self.assertFalse(fence.observe()['databaseCleanShutdownVerified'])
            self.assertFalse(fence.observe()['hostWorkerInventoryVerified'])
            with self.assertRaises(ValueError):fence.stop_database()
            self.assertEqual(len(host.commands),4)
        self.run_host(check)

    def test_backup_dispatched_while_timer_stops_leaves_pending_intent_and_ingress_running(self):
        def check(host,journal,fence):
            host.backup_race=True
            with self.assertRaises(ValueError):fence.maintenance()
            self.assertEqual(journal.status()['pendingStage'],'maintenance')
            self.assertEqual(host.commands,[['systemctl','stop',w.TIMER]])
            self.assertTrue(all(c['State']['Running'] for c in host.containers))
        self.run_host(check)

    def test_lost_stop_response_does_not_retry_or_continue(self):
        def check(host,journal,fence):
            host.lost_edge_response=True
            with self.assertRaises(TimeoutError):fence.maintenance()
            self.assertEqual(journal.status()['pendingStage'],'maintenance')
            with self.assertRaises(ValueError):fence.maintenance()
            with self.assertRaises(ValueError):fence.stop_writers()
            self.assertEqual(len(host.commands),2)
            self.assertTrue(host.containers[0]['State']['Running'])
            self.assertTrue(host.containers[1]['State']['Running'])
        self.run_host(check)

    def test_wrong_order_or_changed_configuration_has_no_stop_effect(self):
        def check(host,journal,fence):
            with self.assertRaises(ValueError):fence.stop_database()
            host.containers[0]['Config']['Env'].append('CHANGED=synthetic')
            with self.assertRaises(ValueError):fence.maintenance()
            self.assertEqual(host.commands,[])
            self.assertIsNone(journal.status()['pendingStage'])
        self.run_host(check)


if __name__=='__main__':unittest.main()
