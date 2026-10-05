#!/usr/bin/env python3
import copy
import importlib.util
from pathlib import Path
import unittest
import tempfile

spec = importlib.util.spec_from_file_location('conformance', Path(__file__).with_name('specification-conformance.py'))
conformance = importlib.util.module_from_spec(spec)
spec.loader.exec_module(conformance)


class ConformanceControls(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.generated = conformance.generate()

    def test_mapped_implementation_and_tests_are_bidirectional(self):
        result = self.generated
        for requirement in result['requirements']:
            for field in ['implementation', 'tests', 'formalModels']:
                for source in requirement[field]:
                    self.assertIn({'requirement': requirement['id'], 'relationship': field},
                                  result['reverseTraceability'][source])
                    self.assertEqual(result['sourceFingerprints'][source],
                                     conformance.sha(conformance.ROOT/source))

    def test_discovery_does_not_claim_whole_system_coverage(self):
        result = self.generated
        self.assertGreater(len(result['unmappedSurfaces']), 0)
        self.assertGreater(len(result['stateMachines']), 0)
        self.assertGreater(len(result['apiOperations']), 0)
        self.assertEqual(result['conformanceCounts']['PASS'], 0)
        self.assertTrue(all(x['conformance'] == 'not-established-by-client-generation'
                            for x in result['apiOperations']))

    def test_negative_controls_reject_broken_traceability(self):
        controls = {
            'missing statement': lambda r: r[0].pop('statement'),
            'duplicate identity': lambda r: r.append(copy.deepcopy(r[0])),
            'missing implementation': lambda r: r[0].update(implementation=[]),
            'missing tests': lambda r: r[0].update(tests=[]),
            'path traversal': lambda r: r[0].update(tests=['../outside.py']),
            'absent target': lambda r: r[0].update(tests=['does-not-exist.py']),
            'absolute target': lambda r: r[0].update(tests=['/etc/passwd']),
            'invalid status': lambda r: r[0].update(status='green'),
            'invented proof': lambda r: r[0].update(conformance={'classification': 'PASS'}),
            'empty title': lambda r: r[0].update(title=''),
            'empty authorization': lambda r: r[0].update(authorization=' '),
            'null actors': lambda r: r[0].update(actors=None),
            'null state': lambda r: r[0].update(state=None),
            'null transitions': lambda r: r[0].update(allowedTransitions=None),
            'non-string mapping': lambda r: r[0].update(tests=[None]),
        }
        for name, mutate in controls.items():
            with self.subTest(name=name):
                rows = copy.deepcopy(self.generated['requirements'])
                mutate(rows)
                with self.assertRaises(ValueError):
                    conformance.validate_requirements(rows)

    def test_mapping_symlinks_and_symlinked_parents_are_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory).resolve()
            root = base/'repo'
            root.mkdir()
            (base/'outside.py').write_text('private')
            (root/'local.py').write_text('local')
            (root/'escape.py').symlink_to(base/'outside.py')
            (root/'alias.py').symlink_to(root/'local.py')
            (root/'parent').symlink_to(base, target_is_directory=True)
            for mapping in ['escape.py', 'alias.py', 'parent/outside.py']:
                with self.subTest(mapping=mapping):
                    row = copy.deepcopy(self.generated['requirements'][0])
                    row.update(implementation=['local.py'], tests=[mapping], formalModels=[])
                    with self.assertRaisesRegex(ValueError, 'invalid tests mapping'):
                        conformance.validate_requirements([row], root)

    def test_capabilities_retain_conditional_authority_and_mobile_exception(self):
        rows = [x for x in self.generated['capabilityMatrix'] if x['feature'] == 'admin.video-sources']
        self.assertEqual({x['action'] for x in rows}, {'discover', 'view', 'administer'})
        for row in rows:
            self.assertEqual(row['rolesAny'], ['Admin'])
            self.assertTrue(row['strictAdmin'])
            self.assertEqual(row['modulesAll'], ['Admin'])
            self.assertEqual(row['mobile']['kind'], 'security-concealed')

    def test_fragment_operations_are_not_silently_omitted(self):
        operations = {row['id']: row for row in self.generated['apiOperations']}
        self.assertIn('GET /social/v2/me', operations)
        self.assertIn('GET /directory/search', operations)
        self.assertEqual(operations['GET /social/v2/me']['source'], 'tdf-hq/docs/openapi/social-v2.yaml')

    def test_deferred_api_requires_real_requirement_and_provenance(self):
        policy = copy.deepcopy(self.generated['apiAvailability'])
        self.assertEqual(len(policy['deferredOperations']), 25)
        conformance.validate_availability(policy, self.generated['requirements'])
        for field, value in [('id', None), ('id', 'GET /x/{named}'), ('id', 'BAD /x'),
                             ('reason', ''), ('requirement', 'MKT-UNKNOWN-999'), ('sources', []),
                             ('sources', ['../outside.md']), ('sources', ['/etc/passwd']),
                             ('sources', ['absent-file.md']), ('sources', [None])]:
            invalid = copy.deepcopy(policy)
            invalid['deferredOperations'][0][field] = value
            with self.subTest(field=field, value=value), self.assertRaises(ValueError):
                conformance.validate_availability(invalid, self.generated['requirements'])

    def test_duplicate_deferred_api_identity_is_rejected(self):
        policy = copy.deepcopy(self.generated['apiAvailability'])
        policy['deferredOperations'].append(copy.deepcopy(policy['deferredOperations'][0]))
        with self.assertRaisesRegex(ValueError, 'duplicate deferred'):
            conformance.validate_availability(policy, self.generated['requirements'])

    def test_openapi_negative_controls(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory).resolve()
            source = root/'api.yaml'
            source.write_text('paths:\n  /a: {}\n  /a: {}\n')
            with self.assertRaisesRegex(ValueError, 'Duplicate YAML'):
                conformance.load_yaml(source)
            source.write_text('alias:\n  $ref: "#/alias"\n')
            with self.assertRaisesRegex(ValueError, 'Cyclic'):
                conformance.resolve_reference({'$ref': '#/alias'}, source, root)
            with self.assertRaisesRegex(ValueError, 'escaping'):
                conformance.resolve_reference({'$ref': '../private.yaml#/paths'}, source, root)
            with self.assertRaisesRegex(ValueError, 'escaping'):
                conformance.resolve_reference({'$ref': 'https://example.invalid/api#/paths'}, source, root)


if __name__ == '__main__':
    unittest.main()
