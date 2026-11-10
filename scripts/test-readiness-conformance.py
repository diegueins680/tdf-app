#!/usr/bin/env python3
"""DEPLOY-READINESS-001: recovery may drain dead pooled connections, never arbitrary errors."""
import json
from pathlib import Path
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent / 'lib'))
from readiness_conformance import classify_health, record_health, require_recovery

READY = (200, {'status': 'ok', 'db': 'ok'}, {'Cache-Control': 'no-store'})
UNAVAILABLE = (503, {'status': 'degraded', 'db': 'unavailable'},
               {'Cache-Control': 'no-store', 'Retry-After': '5'})


class RecoveryTests(unittest.TestCase):
    def exercise(self, responses, *, seconds_per_probe=0):
        self.now = 0
        self.calls = 0
        self.observations = []
        def health():
            self.calls += 1
            self.now += seconds_per_probe
            return responses[min(self.calls - 1, len(responses) - 1)]
        def check(name, condition):
            if not condition:
                raise AssertionError(name)
        def record(phase, attempt, status, body, headers):
            kind = classify_health(status, body, headers)
            self.observations.append(kind)
            return kind
        def pause(seconds):
            self.now += seconds
        require_recovery(health, check, record, clock=lambda: self.now, pause=pause)

    def test_immediate_fresh_success(self):
        self.exercise([READY])
        self.assertEqual(self.calls, 1)

    def test_witnessed_seven_dead_connections_before_recovery(self):
        self.exercise([UNAVAILABLE] * 7 + [READY])
        self.assertEqual(self.observations, ['unavailable'] * 7 + ['ready'])

    def test_attempt_bound_rejects_permanent_unavailability(self):
        with self.assertRaisesRegex(AssertionError, 'recovers after'):
            self.exercise([UNAVAILABLE])
        self.assertEqual(self.calls, 16)

    def test_elapsed_deadline_rejects_unavailability_before_attempt_bound(self):
        with self.assertRaisesRegex(AssertionError, 'recovers after'):
            self.exercise([UNAVAILABLE], seconds_per_probe=11)
        self.assertLess(self.calls, 16)

    def test_success_after_deadline_cannot_pass(self):
        with self.assertRaisesRegex(AssertionError, 'recovers after'):
            self.exercise([READY], seconds_per_probe=31)

    def test_unexpected_status_is_not_retried(self):
        with self.assertRaisesRegex(AssertionError, 'documented contract'):
            self.exercise([(502, UNAVAILABLE[1], UNAVAILABLE[2]), READY])
        self.assertEqual(self.calls, 1)

    def test_malformed_ready_body_is_not_retried(self):
        with self.assertRaisesRegex(AssertionError, 'documented contract'):
            self.exercise([(200, {'status': 'ok', 'db': 'unavailable'}, READY[2]), READY])
        self.assertEqual(self.calls, 1)

    def test_unavailable_body_and_headers_remain_strict(self):
        for response in [
            (503, {'status': 'starting', 'db': 'starting'}, UNAVAILABLE[2]),
            (503, {**UNAVAILABLE[1], 'detail': 'PRIVATE'}, UNAVAILABLE[2]),
            (503, UNAVAILABLE[1], {'Cache-Control': 'no-store'}),
            (503, UNAVAILABLE[1], {'Cache-Control': 'no-store', 'Retry-After': '1'}),
            (503, UNAVAILABLE[1], {'Cache-Control': 'public', 'Retry-After': '5'}),
            (200, READY[1], {}),
        ]:
            with self.subTest(response=response), self.assertRaisesRegex(AssertionError, 'documented contract'):
                self.exercise([response, READY])
            self.assertEqual(self.calls, 1)

    def test_diagnostics_exclude_arbitrary_body_and_headers(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            observations = []
            kind = record_health(output, observations, 'restored', 1, 503,
                                 {'detail': 'PRIVATE_BODY'}, {'X-Debug': 'PRIVATE_HEADER'})
            self.assertEqual(kind, 'invalid')
            text = (output / 'readiness-responses.json').read_text()
            self.assertNotIn('PRIVATE', text)
            self.assertEqual(json.loads(text), observations)
            self.assertEqual(observations[0]['responseKind'], 'invalid')


if __name__ == '__main__':
    unittest.main()
