#!/usr/bin/env python3
import unittest
from lib.requirement_declarations import extract_declarations

class Declarations(unittest.TestCase):
    def extract(self, source, authority='inferred'):
        return extract_declarations(source, 'docs/social/contract.md', authority)

    def test_stable_authored_identity_survives_text_and_line_edits(self):
        source = '| ID | Rule |\n| --- | --- |\n| TC01 | Reject stale revision |\n'
        first = self.extract(source)[0]
        changed = self.extract('\n\n' + source.replace('stale revision', 'revoked session'))[0]
        self.assertEqual(first['id'], changed['id'])
        self.assertNotEqual(first['statement'], changed['statement'])
        self.assertNotEqual(first['sourceSha256'], changed['sourceSha256'])
        self.assertNotEqual(first['line'], changed['line'])
        self.assertEqual(first['declaredId'], 'TC01')
        self.assertEqual(first['verification'], 'open')

    def test_traceability_preserves_claims_without_promoting_pass_or_policy(self):
        result = self.extract('| Requirement | Property | Evidence |\n|---|---|---|\n| PROFILE-01 No private disclosure | Authorized | historical PASS |')[0]
        self.assertEqual(result['statement'], 'No private disclosure')
        self.assertEqual(result['declarationColumns']['Evidence'], 'historical PASS')
        self.assertEqual(result['status'], 'inferred')
        self.assertEqual(result['verification'], 'open')
        self.assertEqual(result['reconciliation'], 'unreviewed')

    def test_fenced_examples_ranges_and_reference_tables_are_not_declarations(self):
        for source in [
            '```md\n| ID | Rule |\n|---|---|\n| TC01 | example |\n```',
            '~~~~md\n| ID | Rule |\n|---|---|\n| TC01 | example |\n~~~\n| TC02 | still fenced |\n~~~~',
            '| ID | Rule |\n|---|---|\n| EO-001–EO-003 | reference |\n| EO-001, EO-003 | reference |',
            '    | ID | Rule |\n    |---|---|\n    | TC01 | indented code |',
            '```md\n```not-a-closing-fence\n| ID | Rule |\n|---|---|\n| TC01 | still fenced |\n```',
            '| Test | Result |\n|---|---|\n| TC01 | PASS |',
        ]:
            with self.subTest(source=source): self.assertEqual(self.extract(source), [])

    def test_non_table_and_ambiguous_headers_do_not_silently_extract(self):
        self.assertEqual(self.extract('| ID | Rule |\n| TC01 | no separator |'), [])
        self.assertEqual(self.extract('| ID | Rule |\n|---|---|---|\n| TC01 | wrong separator |'), [])
        with self.assertRaises(ValueError):
            self.extract('| ID | Rule | Rule |\n|---|---|---|\n| TC01 | require | bypass |')

    def test_spaced_ranges_and_references_are_not_declarations(self):
        for separator in ['-', '–', '—', '/', ',', 'to', 'through', 'and', 'TO', 'Through', 'AND']:
            for endpoint in ['EO-003', '003']:
                with self.subTest(separator=separator, endpoint=endpoint):
                    self.assertEqual(self.extract(f'| ID | Rule |\n|---|---|\n| EO-001 {separator} {endpoint} | reference |'), [])
        for identifier in ['EO-001-EO-003', 'EO-001-003']:
            self.assertEqual(self.extract(f'| ID | Rule |\n|---|---|\n| {identifier} | reference |'), [])

    def test_statement_required_even_in_one_column_table(self):
        self.assertEqual(self.extract('| ID |\n|---|\n| TC01 |'), [])
        self.assertEqual(self.extract('| ID | Rule |\n|---|---|\n| TC01 | |'), [])
        result = self.extract('| ID |\n|---|\n| TC01 Reject stale writes |')
        self.assertEqual(result[0]['statement'], 'Reject stale writes')

    def test_escaped_pipe_and_backslash_parity(self):
        result = self.extract('| ID | Rule |\n|---|---|\n| LW-01 | Accept `read\\|write` only |')
        self.assertEqual(result[0]['statement'], 'Accept `read\\|write` only')

    def test_duplicate_identifier_cannot_hide_conflicting_declaration(self):
        result = self.extract('| ID | Rule |\n|---|---|\n| TC01 | require grant |\n| TC01 | bypass grant |')
        self.assertEqual(len(result), 1)
        self.assertEqual(len(result[0]['occurrences']), 2)
        self.assertIn('competing', result[0]['reconciliation'])

    def test_approval_is_not_inferred_from_text_and_archive_status_is_retained(self):
        source = '| ID | Rule |\n|---|---|\n| TC01 | approved and PASS |'
        self.assertEqual(self.extract(source)[0]['status'], 'inferred')
        self.assertEqual(self.extract(source, 'historical')[0]['status'], 'historical')

if __name__ == '__main__': unittest.main()
