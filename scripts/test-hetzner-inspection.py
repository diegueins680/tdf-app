#!/usr/bin/env python3
"""Negative controls for the canonical read-only collector's disclosure boundary."""
import copy
import importlib.util
import json
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('hetzner_inspection', ROOT/'ops/hetzner/inspect-runtime.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
SECRET = 'synthetic-credential-MUST-NOT-APPEAR'


def container(service='api'):
    return {'Id': 'a'*64, 'Image': 'sha256:'+'b'*64,
            'Config': {'Image': 'ghcr.io/example/image@sha256:'+'c'*64,
                       'Env': ['DATABASE_URL='+SECRET, 'PASSWORD='+SECRET, 'APP_ENV=production',
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
            'providerAccounts': [{'provider': 'manual_bank', 'status': 'ready', 'contract_status': 'approved',
                'credential_status': 'validated', 'environment': 'production', 'enabled': False,
                'feature_flag_key': None, 'credentials': SECRET}],
            'extensions': [{'extname': 'vector', 'extversion': '0.8.1'}], 'extra': SECRET}


class Boundaries(unittest.TestCase):
    def test_api_redacts_credentials_and_reports_unsafe_cors_honestly(self):
        result=module.summarize_container('api', container())
        self.assertNotIn(SECRET, json.dumps(result))
        self.assertEqual(result['booleanConfiguration']['ALLOW_ALL_ORIGINS'], 'true')
        self.assertIn('RUN_MIGRATIONS', result['missingBooleanConfiguration'])

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
        self.assertEqual(command[:8],['docker','exec','-i','a'*64,'env','-u','PGHOSTADDR','-u'])
        for variable in ['PGHOSTADDR','PGSERVICE','PGSERVICEFILE']:
            self.assertEqual(command[command.index(variable)-1],'-u')
        self.assertEqual(command[command.index('-h')+1],'/var/run/postgresql')
        self.assertEqual(command[command.index('-p')+1],'5432')
        self.assertEqual(command[command.index('-U')+1],'tdf_catalog_inventory')
        self.assertIn('inet_server_addr() IS NULL',module.SQL)

if __name__=='__main__':unittest.main()
