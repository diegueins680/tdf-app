#!/usr/bin/env python3
"""Keep incomplete execution from becoming financial verification evidence."""
import contextlib
import importlib.util
import io
from pathlib import Path
import subprocess
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    'checkout_money_verifier', Path(__file__).with_name('verify-checkout-money.py'))
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)


def report(examples=61, failures=0, pending=0, properties=(1000, 1000, 1000)):
    # Independent transcript inputs include the observed one-case counterexample.
    text = '\n'.join(f'    +++ OK, passed {count} tests.' for count in properties)
    return text + f'\n{examples} examples, {failures} failures, {pending} pending\n'


class CheckoutMoneyEvidence(unittest.TestCase):
    def test_complete_intended_execution_and_real_assertion_failure(self):
        self.assertTrue(verifier.expected_execution(
            0, verifier.execution_summary(report()), intended=True))
        self.assertTrue(verifier.expected_execution(
            1, verifier.execution_summary(report(failures=1, properties=(1000,))), intended=False))

    def test_one_case_runs_cannot_claim_three_thousand_generated_cases(self):
        # Old verifier exits zero with this actual Hspec shape and claims 1000 each.
        self.assertFalse(verifier.expected_execution(
            0, verifier.execution_summary(report(properties=(1, 1, 1))), intended=True))

    def test_intended_execution_rejects_incomplete_or_failed_evidence(self):
        cases = [report(examples=0), report(examples=60), report(examples=62),
                 report(failures=1), report(pending=1), report(properties=()),
                 report(properties=(1000, 1000)), report(properties=(1000, 999, 1000)),
                 report(properties=(1000, 1000, 1000, 1000))]
        for output in cases:
            with self.subTest(output=output):
                self.assertFalse(verifier.expected_execution(
                    0, verifier.execution_summary(output), intended=True))
        self.assertFalse(verifier.expected_execution(
            1, verifier.execution_summary(report()), intended=True))

    def test_mutations_require_complete_nonpending_assertion_failures(self):
        for exit_code, output in [(0, report(failures=1)), (1, report()),
                                  (1, report(examples=0, failures=1)),
                                  (1, report(examples=60, failures=1)),
                                  (1, report(failures=1, pending=1))]:
            with self.subTest(exit_code=exit_code, output=output):
                self.assertFalse(verifier.expected_execution(
                    exit_code, verifier.execution_summary(output), intended=False))

    def test_missing_or_conflicting_reports_are_not_counterexamples(self):
        for output in ['', 'Compiler error', report() + report()]:
            with self.subTest(output=output), self.assertRaises(ValueError):
                verifier.execution_summary(output)

    def test_hspec_singular_summary_and_no_pending_suffix(self):
        self.assertEqual(verifier.execution_summary('1 example, 1 failure\n'),
                         {'examples': 1, 'failures': 1, 'pending': 0, 'propertyPassCounts': []})

    def test_real_runner_rejects_the_old_false_accept_before_mutations(self):
        reduced = subprocess.CompletedProcess([], 0, report(properties=(1, 1, 1)), '')
        with patch.object(verifier.subprocess, 'run', return_value=reduced) as run:
            with contextlib.redirect_stdout(io.StringIO()), self.assertRaisesRegex(
                    SystemExit, 'Checkout money conformance failed: intended'):
                verifier.run()
        self.assertEqual(run.call_count, 1)
        self.assertIn('--fail-on=empty', run.call_args.args[0])

    def test_real_runner_never_counts_a_compile_error_as_a_killed_mutation(self):
        runs = [subprocess.CompletedProcess([], 0, report(), ''),
                subprocess.CompletedProcess([], 1, '', 'Compiler error')]
        with patch.object(verifier.subprocess, 'run', side_effect=runs) as run:
            with contextlib.redirect_stdout(io.StringIO()), self.assertRaisesRegex(
                    SystemExit, 'Checkout money conformance failed: wrapped-checkout-aggregate'):
                verifier.run()
        self.assertEqual(run.call_count, 2)


if __name__ == '__main__':
    unittest.main()
