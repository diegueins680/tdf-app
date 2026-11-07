#!/usr/bin/env python3
"""Admission regressions; optional real Ethos controls use PROOF_CHECKER_TOOLS."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

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
    def test_reuse_requires_identical_formulas_and_preserves_obligation_ids(self):
        # Real producer/checker, synthetic proof inputs and explicitly mocked
        # provenance: exercises receipt admission, not application conformance.
        tools = Path(os.environ['PROOF_CHECKER_TOOLS']).resolve()
        metadata = certificates.assurance.capture_metadata()
        provenance = {'revision': 'synthetic-certificate-reuse-fixture', 'worktreeDirty': True,
                      'files': {p: certificates.sha(content) for p, content in metadata.items()}}
        with tempfile.TemporaryDirectory(prefix='tdf-certificate-reuse-') as directory:
            root = Path(directory)
            for mode in ('identical', 'mutated', 'transient-input', 'transient-contract'):
                with self.subTest(mode=mode):
                    mutated = mode == 'mutated'
                    report = root/mode
                    report.mkdir()
                    hashes = {}
                    for suite, expectations in certificates.EXPECTATIONS.items():
                        (report/suite).mkdir()
                        for identity, expected in expectations.items():
                            text = INPUT if expected == 'unsat' else INPUT.replace('(< x x)', '(= x x)')
                            if mutated and (suite, identity) == ('arithmetic', 'refund-safety'):
                                text = INPUT.replace('(< x x)', '(< x (+ x 1))')
                            key = suite+'/'+identity+'.smt2'
                            (report/key).write_text(text)
                            hashes[key] = certificates.sha(text.encode())
                    (report/'report.json').write_text(json.dumps({'provenance': provenance,
                        'execution': {'passed': True}, 'evidenceHashes': hashes}))
                    read_bytes, read_text = Path.read_bytes, Path.read_text
                    input_reads = []
                    def bytes_at(path):
                        if mode == 'transient-input' and path == report/'arithmetic/capture-safety.smt2':
                            input_reads.append(True)
                            if len(input_reads) == 2:
                                return INPUT.replace('(< x x)', '(= x x)').encode()
                        return read_bytes(path)
                    def text_at(path, *args, **kwargs):
                        if mode == 'transient-contract' and path == certificates.ROOT/'formal/system/auth-predicates.json':
                            contract = json.loads(read_bytes(path))
                            contract['obligations'][0]['requirementId'] = 'SYS-MONEY-001'
                            return json.dumps(contract)
                        return read_text(path, *args, **kwargs)
                    with patch.object(certificates.assurance, 'fingerprint', return_value=provenance), \
                         patch.object(Path, 'read_bytes', bytes_at), patch.object(Path, 'read_text', text_at):
                        result = certificates.run(report, tools, root/(mode+'-output'))
                    rows = {entry['id']: entry for entry in result['certificates']}
                    if not mutated:
                        self.assertTrue(result['executionPassed'], result['error'])
                        self.assertEqual(len(rows), 16)
                        self.assertTrue(all(entry['status'] == 'PROVEN' for entry in rows.values()))
                        self.assertEqual(result['metrics']['independentlyCheckedDistinctInputs'], 1)
                        self.assertEqual(sum('reusedFrom' in entry for entry in rows.values()), 15)
                        self.assertNotEqual(rows['arithmetic:capture-safety']['sourceObligationIds'],
                                            rows['arithmetic:refund-safety']['sourceObligationIds'])
                        for suite, names in certificates.CERTIFICATES.items():
                            for name in names:
                                entry = rows[suite+':'+name]
                                self.assertEqual(entry['input'], suite+'/'+name+'.smt2')
                        expected = json.loads(metadata['formal/system/auth-predicates.json'])['obligations'][0]
                        self.assertEqual(rows['authorization:'+expected['id']]['requirementIds'],
                                         [expected['requirementId']])
                        if mode == 'transient-input':
                            self.assertEqual(len(input_reads), 1)
                    else:
                        self.assertFalse(result['executionPassed'])
                        self.assertEqual(rows['arithmetic:refund-safety']['status'], 'UNPROVEN')
                        self.assertNotIn('reusedFrom', rows['arithmetic:refund-safety'])
                        self.assertIsNotNone(result['error'])

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
