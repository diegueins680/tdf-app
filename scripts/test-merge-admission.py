#!/usr/bin/env python3
import copy
import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('admission', Path(__file__).with_name('merge-admission.py'))
admission = importlib.util.module_from_spec(spec)
spec.loader.exec_module(admission)
BASE = 'a' * 40


def pull(number, draft=False, head=None):
    return {'number': number, 'draft': draft, 'head': head or f'{number:040x}'}


class FakeGitHub:
    def __init__(self, pulls, changed=None, fail_revoke=False):
        self.before = {'main': BASE, 'pulls': pulls}
        self.changed = changed
        self.fail_revoke = fail_revoke
        self.reads = 0
        self.writes = []

    def snapshot(self):
        self.reads += 1
        return copy.deepcopy(self.changed if self.reads > 1 and self.changed else self.before)

    def status(self, sha, state, description, target):
        if self.fail_revoke and state == 'pending':
            raise RuntimeError('simulated revocation failure')
        self.writes.append((sha, state, description))


class AdmissionTests(unittest.TestCase):
    def test_dry_run_has_no_status_writes(self):
        gh = FakeGitHub([pull(468), pull(478)])
        report = admission.reconcile(gh, target='https://example.test')
        self.assertEqual(report['admittedPR'], 468)
        self.assertFalse(report['applied'])
        self.assertEqual(gh.writes, [])

    def test_revoke_others_before_granting_oldest_ready_pr(self):
        gh = FakeGitHub([pull(467, True), pull(468), pull(478)])
        report = admission.reconcile(gh, apply=True, target='https://example.test')
        self.assertEqual(report['admittedPR'], 468)
        self.assertEqual([w[1] for w in gh.writes], ['pending', 'pending', 'success'])
        self.assertIn(BASE, gh.writes[-1][2])

    def test_revocation_failure_never_grants_another_slot(self):
        gh = FakeGitHub([pull(468), pull(478)], fail_revoke=True)
        with self.assertRaises(RuntimeError):
            admission.reconcile(gh, apply=True, target='https://example.test')
        self.assertFalse(any(w[1] == 'success' for w in gh.writes))

    def test_changed_observations_withhold_and_revoke_selected_admission(self):
        before = {'main': BASE, 'pulls': [pull(468), pull(478)]}
        changes = [
            {**before, 'main': 'b' * 40},
            {**before, 'pulls': [pull(468, head='c' * 40), pull(478)]},
            {**before, 'pulls': [pull(468, True), pull(478)]},
            {**before, 'pulls': [pull(478)]},
            {**before, 'pulls': [pull(467), pull(468), pull(478)]},
        ]
        for changed in changes:
            with self.subTest(changed=changed):
                gh = FakeGitHub(before['pulls'], changed=changed)
                with self.assertRaises(RuntimeError):
                    admission.reconcile(gh, apply=True, target='https://example.test')
                self.assertFalse(any(w[1] == 'success' for w in gh.writes))
                self.assertEqual(gh.writes[-1][0], pull(468)['head'])

    def test_drafts_do_not_occupy_the_slot(self):
        gh = FakeGitHub([pull(468, True)])
        report = admission.reconcile(gh, apply=True, target='https://example.test')
        self.assertIsNone(report['admittedPR'])
        self.assertEqual(gh.writes[0][1], 'pending')

    def test_duplicate_head_cannot_receive_exclusive_pr_admission(self):
        gh = FakeGitHub([pull(468), pull(469, head=pull(468)['head']), pull(478)])
        report = admission.reconcile(gh, apply=True, target='https://example.test')
        self.assertEqual(report['admittedPR'], 478)
        self.assertTrue(all(w[1] == 'pending' for w in gh.writes if w[0] == pull(468)['head']))

    def test_empty_queue_does_not_mutate_status(self):
        gh = FakeGitHub([])
        admission.reconcile(gh, apply=True, target='https://example.test')
        self.assertEqual(gh.writes, [])

    def test_unchanged_status_is_not_reposted(self):
        gh = admission.GitHub('owner/repo')
        calls = []
        def api(endpoint, *args):
            calls.append((endpoint, args))
            return {'total_count': 1, 'statuses': [{'context': admission.CONTEXT,
                    'state': 'success', 'description': 'same'}]}
        gh.api = api
        gh.status('a' * 40, 'success', 'same', 'https://example.test')
        self.assertEqual(len(calls), 1)

    def test_workflow_executes_only_main_and_serializes_controller(self):
        workflow = (Path(__file__).parent.parent / '.github/workflows/merge-admission.yml').read_text()
        self.assertIn('ref: main', workflow)
        self.assertIn('persist-credentials: false', workflow)
        self.assertIn('cancel-in-progress: false', workflow)
        self.assertIn("if: vars.TDF_INTEGRATION_BOOTSTRAP_ACTIVE != 'true'", workflow)
        self.assertNotIn('pull_request.head', workflow)
        self.assertNotIn('pull-requests: write', workflow)
        self.assertNotIn('contents: write', workflow)


if __name__ == '__main__':
    unittest.main()
