#!/usr/bin/env python3
import copy
import hashlib
import importlib.util
from pathlib import Path
import unittest
import tempfile

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('new_surfaces', ROOT/'scripts/check-new-specification-surfaces.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class Admission(unittest.TestCase):
    def fixture(self, path='scripts/new-runner.mjs'):
        body = b'actual immutable source'
        link = {'requirement': 'SYS-TRACE-001', 'relationship': 'implementation'}
        trace = {'surfaces': [{'path': path, 'sha256': hashlib.sha256(body).hexdigest(), 'requirements': [link]}]}
        requirements = [{'id': 'SYS-TRACE-001', 'implementation': [path]}]
        return path, body, trace, requirements

    def test_admits_canonical_root_and_mobile_mappings(self):
        for path in ['scripts/new-runner.mjs', 'tdf-mobile/src/new-client.ts']:
            path, body, trace, requirements = self.fixture(path)
            self.assertEqual(module.validate_changed_paths([path], trace, requirements, lambda _: body), [path])

    def test_new_file_cannot_hide_by_omission_from_generated_inventory(self):
        path, body, trace, requirements = self.fixture()
        trace['surfaces'] = []
        with self.assertRaisesRegex(ValueError, 'no requirement'):
            module.validate_changed_paths([path], trace, requirements, lambda _: body)

    def test_rejects_orphan_stale_and_forged_mapping_controls(self):
        for mutation in [
            lambda x: x['surfaces'][0].update(requirements=[]),
            lambda x: x['surfaces'][0].update(sha256='0'*64),
            lambda x: x['surfaces'][0]['requirements'][0].update(requirement='NOT-REAL'),
            lambda x: x['surfaces'][0]['requirements'][0].update(relationship='tests'),
        ]:
            path, body, trace, requirements = self.fixture()
            mutation(trace)
            with self.assertRaises(ValueError):
                module.validate_changed_paths([path], trace, requirements, lambda _: body)

    def test_existing_unmapped_debt_is_not_reclassified_or_newly_admitted(self):
        path, body, trace, requirements = self.fixture()
        before = copy.deepcopy(trace)
        self.assertEqual(module.validate_changed_paths([], trace, requirements, lambda _: body), [])
        self.assertEqual(trace, before)

    def test_revision_arguments_cannot_be_options_or_mutable_refs(self):
        for value in ['main', '--all', '', '0'*39]:
            with self.assertRaises(ValueError): module.full_sha(value)


class GitAdmission(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='tdf-trace-admission-')
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.mobile = self.root/'tdf-mobile'
        self.mobile.mkdir()
        for repo in [self.root, self.mobile]:
            module.git(repo, 'init', '-q')
            module.git(repo, 'config', 'user.name', 'Synthetic conformance fixture')
            module.git(repo, 'config', 'user.email', 'fixture@example.invalid')
            module.git(repo, 'config', 'commit.gpgsign', 'false')
        self.write(self.mobile, 'src/old.ts', 'old mobile source')
        self.mobile_base = self.commit(self.mobile)
        self.write(self.root, 'scripts/old.py', 'old root source')
        self.write(self.root, '.gitignore', 'tdf-mobile/\n')
        self.pin(self.mobile_base)
        self.base = self.commit(self.root)

    def write(self, root, path, value):
        target = root/path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(value)

    def pin(self, revision):
        module.git(self.root, 'update-index', '--add', '--cacheinfo', '160000,' + revision + ',tdf-mobile')

    def commit(self, root):
        module.git(root, 'add', '.')
        module.git(root, 'commit', '-qm', 'Synthetic fixture state')
        return module.git(root, 'rev-parse', 'HEAD').decode().strip()

    def candidate(self, mapped):
        import json
        revision = module.git(self.mobile, 'rev-parse', 'HEAD').decode().strip()
        rows = [{'path': path, 'sha256': hashlib.sha256((self.root/path).read_bytes()).hexdigest(),
                 'requirements': [{'requirement': 'SYS-TRACE-001', 'relationship': 'implementation'}]}
                for path in mapped]
        self.write(self.root, 'formal/system/traceability.json', json.dumps({'mobileRevision': revision, 'surfaces': rows}))
        self.write(self.root, 'formal/system/requirements.json', json.dumps({'requirements': [{'id': 'SYS-TRACE-001', 'implementation': mapped}]}))
        self.pin(revision)
        return self.commit(self.root)

    def test_real_git_rename_cannot_hide_new_unmapped_surface(self):
        (self.root/'scripts/old.py').rename(self.root/'scripts/renamed.py')
        head = self.candidate([])
        with self.assertRaisesRegex(ValueError, 'no requirement mapping: scripts/renamed.py'):
            module.check(self.root, self.base, head)
        head = self.candidate(['scripts/renamed.py'])
        self.assertEqual(module.check(self.root, self.base, head)['mappedChangedSurfaces'], ['scripts/renamed.py'])

    def test_actual_gitlink_delta_and_wrong_mobile_checkout(self):
        self.write(self.mobile, 'src/new.ts', 'new mobile source')
        self.commit(self.mobile)
        head = self.candidate([])
        with self.assertRaisesRegex(ValueError, 'no requirement mapping: tdf-mobile/src/new.ts'):
            module.check(self.root, self.base, head)
        head = self.candidate(['tdf-mobile/src/new.ts'])
        self.assertEqual(module.check(self.root, self.base, head)['mappedChangedSurfaces'], ['tdf-mobile/src/new.ts'])
        module.git(self.mobile, 'checkout', '-q', self.mobile_base)
        with self.assertRaisesRegex(ValueError, 'checkout differs'):
            module.check(self.root, self.base, head)

    def test_existing_unmapped_root_edit_requires_a_mapping(self):
        self.write(self.root, 'scripts/old.py', 'modified existing root source')
        head = self.candidate([])
        with self.assertRaisesRegex(ValueError, 'no requirement mapping: scripts/old.py'):
            module.check(self.root, self.base, head)
        head = self.candidate(['scripts/old.py'])
        self.assertEqual(module.check(self.root, self.base, head)['mappedChangedSurfaces'], ['scripts/old.py'])

    def test_existing_unmapped_mobile_edit_requires_a_mapping(self):
        self.write(self.mobile, 'src/old.ts', 'modified existing mobile source')
        self.commit(self.mobile)
        head = self.candidate([])
        with self.assertRaisesRegex(ValueError, 'no requirement mapping: tdf-mobile/src/old.ts'):
            module.check(self.root, self.base, head)
        head = self.candidate(['tdf-mobile/src/old.ts'])
        self.assertEqual(module.check(self.root, self.base, head)['mappedChangedSurfaces'], ['tdf-mobile/src/old.ts'])

    def test_working_file_cannot_replace_immutable_source_evidence(self):
        self.write(self.root, 'scripts/new.py', 'committed source')
        head = self.candidate(['scripts/new.py'])
        self.write(self.root, 'scripts/new.py', 'different working source')
        self.assertEqual(module.check(self.root, self.base, head)['mappedChangedSurfaces'], ['scripts/new.py'])

    def test_new_symlink_cannot_be_admitted_as_source(self):
        (self.root/'scripts/link.py').symlink_to('old.py')
        head = self.candidate(['scripts/link.py'])
        with self.assertRaisesRegex(ValueError, 'regular committed file'):
            module.check(self.root, self.base, head)


if __name__ == '__main__':
    unittest.main()
