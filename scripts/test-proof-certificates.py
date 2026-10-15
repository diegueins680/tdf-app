#!/usr/bin/env python3
"""Admission regressions; optional real Ethos controls use PROOF_CHECKER_TOOLS."""
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('certificates', Path(__file__).with_name('verify-proof-certificates.py'))
certificates = importlib.util.module_from_spec(spec)
spec.loader.exec_module(certificates)
INPUT = '(set-option :timeout 30000)\n(set-logic QF_LIA)\n(declare-fun x () Int)\n(assert (< x x))\n(check-sat)\n'
RAW = 'unsupported\nunsat\n(\n(declare-const x Int)\n(assume @p1 (< x x))\n(step @p2 false :rule trust :args (false))\n)\n'


class Admission(unittest.TestCase):
    def test_reference_declarations_are_not_shadowed(self):
        bound = certificates.bind_certificate(RAW, INPUT)
        self.assertNotIn('declare-const', bound)
        self.assertIn('(assume @p1 (< x x))', bound)

    def test_unknown_commands_declarations_and_unclosed_scopes_refuse(self):
        mutations = [
            RAW.replace('(declare-const x Int)', '(declare-const x Bool)'),
            RAW.replace('(declare-const x Int)', '(declare-const other Int)'),
            RAW.replace('(declare-const x Int)', '(declare-const x Int)\n(declare-const x Int)'),
            RAW.replace('(assume @p1', '(include "extra.eo")\n(assume @p1'),
            RAW.replace('(assume @p1', '(reset)\n(assume @p1'),
            RAW.replace('(assume @p1', '(declare-rule forge :conclusion false)\n(assume @p1'),
            RAW.replace('(assume @p1', '(assume-push @local true)\n(assume @p1'),
            RAW.replace('(step @p2', '(step-pop @pop :rule scope :premises (@p1))\n(step @p2'),
            RAW.replace('(assume @p1', '(define false () true)\n(assume @p1'),
        ]
        for changed in mutations:
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                certificates.bind_certificate(changed, INPUT)

    def test_final_false_is_required(self):
        for changed in (RAW.replace('(step @p2 false :rule trust :args (false))', ''),
                        RAW.replace('@p2 false', '@p2 true'),
                        RAW.replace('@p2 false', '@p2 @fakefalse'),
                        RAW.replace(')\n)\n', ')\n(define @t1 () false)\n)\n')):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                certificates.bind_certificate(changed, INPUT)

    def test_warning_unknown_and_truncated_output_refuse(self):
        for changed in (RAW.replace('unsat', 'unknown'), RAW[:-3], RAW+'(step @p3 false)\n',
                        RAW.replace('(\n', '(\n; WARNING: trust\n', 1)):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                certificates.bind_certificate(changed, INPUT)

    def test_unsupported_is_only_permitted_for_known_timeout(self):
        with self.assertRaises(ValueError):
            certificates.bind_certificate(RAW, INPUT.replace(':timeout', ':unknown'))

    def test_reference_requires_one_assertion(self):
        for changed in (INPUT.replace('(assert (< x x))', ''),
                        INPUT.replace('(check-sat)', '(assert false)\n(check-sat)'),
                        INPUT.replace('(check-sat)', '(push 1)\n(check-sat)')):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                certificates.bind_certificate(RAW, changed)

    def test_strict_parser_refuses_unsupported_lexical_forms(self):
        for changed in ('(foo "bar")', '(foo |bar|)', '(foo \\bar)', 'atom', '(foo', ')'):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                certificates.sexprs(changed)


@unittest.skipUnless(os.environ.get('PROOF_CHECKER_TOOLS'), 'Set PROOF_CHECKER_TOOLS for real independent-checker controls')
class RealChecker(unittest.TestCase):
    def test_false_assumption_trust_and_empty_proof_are_not_certificates(self):
        tools = Path(os.environ['PROOF_CHECKER_TOOLS']).resolve()
        with tempfile.TemporaryDirectory(prefix='tdf-certificate-controls-') as directory:
            out = Path(directory)
            reference = out/'input.smt2'
            reference.write_text(INPUT)
            generated = subprocess.run([str(tools/'cvc5'), '--lang=smt2', '--produce-proofs',
                '--dump-proofs', '--proof-format-mode=cpc', '--proof-granularity=dsl-rewrite', str(reference)],
                capture_output=True, text=True, check=True, timeout=30)
            bound = certificates.bind_certificate(generated.stdout, INPUT)
            prefix = f'(include "{tools}/cvc5-source/proofs/eo/cpc/Cpc.eo")\n(reference "{reference}")\n'
            proof = out/'proof.cpc'
            def check(body):
                proof.write_text(prefix+body)
                return subprocess.run([str(tools/'ethos'), str(proof)], capture_output=True, text=True, timeout=30)
            result = check(bound)
            self.assertEqual((result.returncode, result.stdout.strip()), (0, 'correct'), result.stderr)
            # Ethos must reject a different assumption, even with a valid
            # subsequent derivation from false. It cannot silently drop binding.
            forged = '(assume @p1 false)\n(step @p2 false :rule and_elim :premises (@p1) :args (0))\n'
            rejected = check(forged)
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn('not part of the referenced assertions', rejected.stdout+rejected.stderr)
            trusted = check('(step @p1 false :rule trust :args (false))\n')
            self.assertEqual((trusted.returncode, trusted.stdout.strip()), (0, 'incomplete'))
            # Empty files return "correct" from Ethos: our admission must refuse
            # them before treating checker success as a refutation certificate.
            empty = check('')
            self.assertEqual((empty.returncode, empty.stdout.strip()), (0, 'correct'))
            with self.assertRaises(ValueError):
                certificates.bind_certificate('unsat\n(\n)\n', INPUT)
            with self.assertRaises(ValueError):
                certificates.bind_certificate('unsat\n(\n(assume @p1 (< x x))\n)\n', INPUT)


if __name__ == '__main__':
    unittest.main()
