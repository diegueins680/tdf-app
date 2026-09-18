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


if __name__ == "__main__":
    unittest.main()
