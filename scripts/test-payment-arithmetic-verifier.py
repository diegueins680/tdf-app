#!/usr/bin/env python3
"""Fail-closed translator and semantic negative controls, using the real solver."""
import importlib.util
from pathlib import Path
import unittest
import sys

sys.dont_write_bytecode = True

spec = importlib.util.spec_from_file_location("payment_proof", Path(__file__).with_name("verify-payment-arithmetic.py"))
proof = importlib.util.module_from_spec(spec)
spec.loader.exec_module(proof)


class CorrespondenceTests(unittest.TestCase):
    def test_actual_source(self):
        self.assertEqual(len(proof.verify(proof.SOURCE.read_text())), 2)

    def test_mutated_arithmetic_does_not_keep_old_proof(self):
        text = proof.SOURCE.read_text()
        for old, new in [
            ("amount > maximumCapture - paymentCapturedMinor lifecycle", "newCaptured > maximumCapture"),
            ("amount > paymentCapturedMinor lifecycle - paymentRefundedMinor lifecycle", "newRefunded > paymentCapturedMinor lifecycle"),
            ("newCaptured = paymentCapturedMinor lifecycle + amount", "newCaptured = paymentCapturedMinor lifecycle - amount"),
            ("newRefunded = paymentRefundedMinor lifecycle + amount", "newRefunded = paymentRefundedMinor lifecycle - amount"),
        ]:
            with self.subTest(mutation=new):
                self.assertIn(old, text)
                with self.assertRaises(AssertionError):
                    proof.verify(text.replace(old, new))

    def test_unknown_structure_and_expressions_fail(self):
        text = proof.SOURCE.read_text()
        with self.assertRaises(ValueError):
            proof.verify(text.replace("paymentCapturedMinor = newCaptured", "paymentCapturedMinor = amount"))
        with self.assertRaises(ValueError):
            proof.verify(text.replace("newCaptured = paymentCapturedMinor lifecycle + amount", "newCaptured = abs amount"))

    def test_weakened_caller_validator_refutes_precondition(self):
        source = proof.SOURCE.read_text()
        for selector in ('paymentAuthorizedMinor', 'paymentRefundedMinor'):
            if selector == 'paymentAuthorizedMinor':
                changed = source.replace('[ paymentAuthorizedMinor lifecycle\n      , paymentCapturedMinor lifecycle',
                                         '[ paymentCapturedMinor lifecycle')
            else:
                changed = source.replace('      , ' + selector + ' lifecycle\n', '')
            with self.subTest(selector=selector):
                self.assertNotEqual(changed, source)
                # This must be a solver counterexample, not unsupported syntax.
                with self.assertRaises(AssertionError):
                    proof.verify(changed)

    def test_redundant_captured_sign_guard_is_not_a_false_counterexample(self):
        changed = proof.SOURCE.read_text().replace('      , paymentCapturedMinor lifecycle\n', '')
        # Refunded >= 0 and refunded <= captured still imply captured >= 0.
        for helper in ('capture', 'refund'):
            admitted, precondition, _ = proof.caller_obligation(changed, helper)
            proof.check(proof.z3.And(admitted, proof.z3.Not(precondition)), 'unsat')

    def test_validator_bypass_or_extra_clause_is_unsupported(self):
        source = proof.SOURCE.read_text()
        for changed in (
            source.replace('| otherwise = applyPaymentEvent lifecycle event',
                           '| otherwise = Right lifecycle'),
            source.replace('| otherwise = applyPaymentEvent lifecycle event',
                           '| paymentAmountMinor lifecycle == 42 = Right lifecycle\n'
                           '  | otherwise = applyPaymentEvent lifecycle event'),
            source.replace('paymentAuthorizedMinor lifecycle\n      , paymentCapturedMinor lifecycle',
                           'abs (paymentAuthorizedMinor lifecycle)\n      , paymentCapturedMinor lifecycle'),
        ):
            with self.subTest(source=changed):
                with self.assertRaises(ValueError):
                    proof.caller_obligation(changed, 'capture')


if __name__ == "__main__":
    unittest.main()
