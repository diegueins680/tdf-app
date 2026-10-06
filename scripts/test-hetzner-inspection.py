#!/usr/bin/env python3
"""Negative controls for the canonical read-only collector's disclosure boundary."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('hetzner_inspection', ROOT/'ops/hetzner/inspect-runtime.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
SECRET = 'synthetic-credential-MUST-NOT-APPEAR'


def container(service='api'):
    return {'Id': 'a'*64, 'Image': 'sha256:'+'b'*64,
            'Config': {'Image': 'ghcr.io/example/image@sha256:'+'c'*64,
                       'Env': ['PGDATA=/var/lib/postgresql/data', 'DATABASE_URL='+SECRET, 'PASSWORD='+SECRET, 'APP_ENV=production',
                               'ALLOW_ALL_ORIGINS=true', 'ALLOWED_ORIGINS=https://www.tdfrecords.net'],
                       'Labels': {'com.docker.compose.project': module.PROJECT,
                                  'com.docker.compose.service': service,
                                  'com.docker.compose.project.working_dir': module.DIRECTORY,
                                  'com.docker.compose.project.config_files': module.DIRECTORY+'/compose.yaml'}},
            'State': {'Running': True, 'Health': {'Status': 'healthy'}},
            'Mounts': [{'Destination': '/var/lib/postgresql/data', 'Type': 'volume', 'Name': module.VOLUME}],
            'NetworkSettings': {'Ports': {'5432/tcp': None}, 'Networks': {
                module.PROJECT+'_database': {}, **({module.PROJECT+'_outbound': {}} if service=='api' else {})}}}


def database():
    return {'database': module.DATABASE, 'readOnly': 'on', 'role': 'tdf_catalog_inventory',
            'serverVersion': '17.8', 'localConnection': True, 'migrations': [{'migration_id': '2026-10-04_synthetic',
                'checksum': 'a'*64, 'source_commit': 'b'*40, 'extra': SECRET}],
            'revenueFlags': [{'flag_key': 'checkout.provider', 'enabled': False, 'environment': 'production'}],
            'merchReputationFlags': [{'flag_key': 'store_reviews', 'enabled': False, 'environment': 'production', 'reason': SECRET}],
            'providerAccounts': [{'provider': 'manual_bank', 'status': 'ready', 'contract_status': 'approved',
                'credential_status': 'validated', 'environment': 'production', 'enabled': False,
                'feature_flag_key': None, 'credentials': SECRET}],
            'eventOperationFlags': [{'feature_code': 'event.operations.api', 'enabled': False, 'reason': SECRET}],
            'interactionRuntime': {'enabled': False, 'activatedOnce': True, 'private': SECRET},
            'interactionEntityKinds': [{'code': 'event', 'enabled': True, 'reactable': True, 'commentable': True, 'shareable': True, 'private': SECRET}],
            'extensions': [{'extname': 'vector', 'extversion': '0.8.1'}],
            'socialRuntime': {'enabled': False, 'activatedOnce': True, 'private': SECRET}, 'extra': SECRET}


class Boundaries(unittest.TestCase):
    def test_api_redacts_credentials_and_reports_unsafe_cors_honestly(self):
        result=module.summarize_container('api', container())
        self.assertNotIn(SECRET, json.dumps(result))
        self.assertEqual(result['booleanConfiguration']['ALLOW_ALL_ORIGINS'], 'true')
        self.assertIn('RUN_MIGRATIONS', result['missingBooleanConfiguration'])
        self.assertIn('SOCIAL_V2_ENABLED', result['missingBooleanConfiguration'])

    def test_rejects_routing_identity_and_mutable_image_controls(self):
        for mutation in [
            lambda x: x['Config']['Labels'].update({'com.docker.compose.project': 'other'}),
            lambda x: x['Config']['Labels'].update({'com.docker.compose.project.working_dir': '/other'}),
            lambda x: x['Config']['Labels'].update({'com.docker.compose.project.config_files': '/other.yaml'}),
            lambda x: x['Config'].update(Image='ghcr.io/example/image:latest'),
            lambda x: x['NetworkSettings']['Networks'].update({'unexpected-network': {}}),
            lambda x: x['Config']['Env'].append('PROVIDER_ENABLED='+SECRET),
            lambda x: x['Config']['Env'].append('ALLOW_ALL_ORIGINS=false'),
        ]:
            with self.subTest(mutation=mutation):
                value=container();mutation(value)
                with self.assertRaises(ValueError):module.summarize_container('api', value)

    def test_private_upload_mount_is_observed_not_assumed_from_a_directory(self):
        value = container()
        self.assertFalse(module.summarize_container('api', value)['privateUploads']['canonicalWritableBind'])
        mount = {'Destination': '/app/uploads', 'Type': 'bind',
                 'Source': module.DIRECTORY + '/uploads', 'RW': True}
        value['Mounts'].append(mount)
        self.assertTrue(module.summarize_container('api', value)['privateUploads']['canonicalWritableBind'])
        for change in [{'Source': '/private/' + SECRET}, {'RW': False}, {'Type': 'tmpfs'}]:
            probe = copy.deepcopy(value); probe['Mounts'][-1].update(change)
            result = module.summarize_container('api', probe)
            self.assertFalse(result['privateUploads']['canonicalWritableBind'])
            self.assertNotIn(SECRET, json.dumps(result))

    def test_outbound_worker_flags_preserve_absence_and_explicit_configuration(self):
        value = container('api')
        keys = ['SOCIAL_AUTO_REPLY_ENABLED', 'COURSE_PAYMENT_REMINDER_ENABLED']
        missing = module.summarize_container('api', value)
        for key in keys:
            self.assertIn(key, missing['missingBooleanConfiguration'])
            self.assertNotIn(key, missing['booleanConfiguration'])
        value['Config']['Env'] += [keys[0] + '=true', keys[1] + '=false']
        present = module.summarize_container('api', value)
        self.assertEqual(present['booleanConfiguration'][keys[0]], 'true')
        self.assertEqual(present['booleanConfiguration'][keys[1]], 'false')
        self.assertTrue(set(keys).isdisjoint(present['missingBooleanConfiguration']))

    def test_database_container_is_private_and_has_expected_volume(self):
        self.assertEqual(module.summarize_container('db',container('db'))['volume'],module.VOLUME)
        for mutation in [
            lambda x: x['Mounts'][0].update(Name='unrelated_data'),
            lambda x: x['NetworkSettings']['Ports'].update({'5432/tcp': [{'HostPort': '5432'}]}),
            lambda x: x['NetworkSettings']['Networks'].update({module.PROJECT+'_outbound': {}}),
        ]:
            value=container('db');mutation(value)
            with self.assertRaises(ValueError):module.summarize_container('db',value)

    def test_database_output_is_allowlisted(self):
        value=module.summarize_database(database())
        self.assertNotIn(SECRET,json.dumps(value))
        self.assertEqual(value['providerAccounts'][0]['enabled'],False)
        self.assertTrue(value['readOnly'])
        self.assertEqual(value['socialRuntime'], {'enabled': False, 'activatedOnce': True})

    def test_absent_social_runtime_is_unknown_and_non_boolean_state_is_rejected(self):
        value = database()
        value['socialRuntime'] = None
        self.assertIsNone(module.summarize_database(value)['socialRuntime'])
        for invalid in [{}, {'enabled': False, 'activatedOnce': SECRET},
                        {'enabled': 'false', 'activatedOnce': True}]:
            value['socialRuntime'] = invalid
            with self.assertRaises(ValueError): module.summarize_database(value)

    def test_merch_flags_are_allowlisted_and_missing_values_remain_unknown(self):
        value = module.summarize_database(database())
        self.assertEqual(value['merchReputationFlags'], [{'flag': 'store_reviews', 'enabled': False}])
        self.assertEqual(set(value['missingMerchReputationFlags']), module.MERCH_REPUTATION_FLAGS - {'store_reviews'})
        missing = database(); missing['merchReputationFlags'] = []
        self.assertEqual(module.summarize_database(missing)['missingMerchReputationFlags'], sorted(module.MERCH_REPUTATION_FLAGS))
        for mutation in [
            lambda rows: rows.append(copy.deepcopy(rows[0])),
            lambda rows: rows[0].update(environment='development'),
            lambda rows: rows[0].update(enabled='false'),
            lambda rows: rows[0].update(flag_key=SECRET),
        ]:
            bad = database(); mutation(bad['merchReputationFlags'])
            with self.assertRaises(ValueError): module.summarize_database(bad)

    def test_optional_event_flags_distinguish_absent_table_from_disabled_row(self):
        with patch.object(module, 'capture', return_value='f') as capture:
            self.assertIsNone(module.optional_event_flags('a'*64))
            self.assertEqual(capture.call_count, 1)
        with patch.object(module, 'capture', side_effect=['t', '[{"feature_code":"event.operations.api","enabled":false}]']) as capture:
            self.assertEqual(module.optional_event_flags('a'*64), [{'feature_code': 'event.operations.api', 'enabled': False}])
            self.assertEqual(capture.call_count, 2)
            for args, kwargs in capture.call_args_list:
                self.assertEqual(args[0], module.database_command('a'*64))
                self.assertTrue(kwargs['input'].startswith('BEGIN READ ONLY;'))
        value = database(); value['eventOperationFlags'] = None; value['interactionRuntime'] = None
        result = module.summarize_database(value)
        self.assertIsNone(result['eventOperationFlags']); self.assertIsNone(result['interactionRuntime'])

    def test_interaction_and_event_switches_reject_invalid_or_duplicate_values(self):
        for mutation in [
            lambda x: x['eventOperationFlags'][0].update(enabled='false'),
            lambda x: x['eventOperationFlags'][0].update(feature_code=SECRET),
            lambda x: x['eventOperationFlags'].append(x['eventOperationFlags'][0]),
            lambda x: x['interactionEntityKinds'][0].update(reactable='true'),
            lambda x: x['interactionEntityKinds'].append(x['interactionEntityKinds'][0]),
            lambda x: x['interactionRuntime'].update(enabled='false'),
        ]:
            value = database(); mutation(value)
            with self.assertRaises(ValueError): module.summarize_database(value)

    def test_database_identity_and_ledger_controls(self):
        for mutation in [
            lambda x:x.update(database='other'),lambda x:x.update(role='postgres'),
            lambda x:x.update(readOnly='off'),lambda x:x.update(serverVersion='16.4'),
            lambda x:x.update(localConnection=False),
            lambda x:x['migrations'][0].update(checksum=SECRET),
            lambda x:x['migrations'].append(copy.deepcopy(x['migrations'][0])),
        ]:
            value=database();mutation(value)
            with self.assertRaises(ValueError):module.summarize_database(value)

    def test_actual_database_command_pins_socket_and_clears_routing(self):
        command=module.database_command('a'*64)
        self.assertEqual(command[:len(module.DOCKER)], module.DOCKER)
        self.assertEqual(command[len(module.DOCKER):len(module.DOCKER)+5], ['exec','-i','a'*64,'env','-i'])
        self.assertEqual(command[command.index('-h')+1],'/var/run/postgresql')
        self.assertEqual(command[command.index('-p')+1],'5432')
        self.assertEqual(command[command.index('-U')+1],'tdf_catalog_inventory')
        self.assertIn('inet_server_addr() IS NULL',module.SQL)

    def test_shadowed_or_redirected_database_storage_rejected(self):
        for child in ['base', 'pg_wal', 'alternate']:
            value = container('db')
            value['Mounts'].append({'Destination': module.DATA_DIRECTORY + '/' + child,
                                    'Type': 'volume', 'Name': 'foreign'})
            with self.assertRaises(ValueError): module.summarize_container('db', value)
        for setting in [[], ['PGDATA=/alternate'], ['PGDATA='+module.DATA_DIRECTORY]*2]:
            value = container('db'); value['Config']['Env'] = setting
            with self.assertRaises(ValueError): module.summarize_container('db', value)

    def test_effective_server_directory_must_pass_fixed_boolean_observation(self):
        for result in ['f', '', 't\nf', 'private-error']:
            with patch.object(module, 'capture', return_value=result), self.assertRaises(ValueError):
                module.verify_storage('a'*64)
        with patch.object(module, 'capture', return_value='t\n') as capture:
            module.verify_storage('a'*64)
            command = capture.call_args.args[0]
            self.assertEqual(command[command.index('-U')+1], 'postgres')
            self.assertEqual(capture.call_args.kwargs['input'], module.STORAGE_SQL)
            self.assertIn("current_setting('data_directory')='/var/lib/postgresql/data'", module.STORAGE_SQL)

if __name__=='__main__':unittest.main()
