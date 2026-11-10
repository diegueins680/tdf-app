#!/usr/bin/env python3
"""Fail-closed controls for the Google caller-to-public-name projection."""
import importlib.util
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('google_name_verifier', ROOT/'scripts/verify-google-account-name.py')
verifier = importlib.util.module_from_spec(spec)
spec.loader.exec_module(verifier)


class CallerCorrespondence(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = verifier.SERVER.read_text()

    def changed(self, old, new):
        self.assertEqual(self.source.count(old), 1, 'Mutation must target exactly one real source fragment')
        return self.source.replace(old, new)

    def test_actual_constructor_and_both_actual_sinks_are_projected(self):
        projection = verifier.caller_projection(self.source)
        caller = verifier.function_block(self.source, 'completeGoogleLogin')
        record = caller[caller.index('ProviderProfile\n'):].strip()
        self.assertIn(record.replace('\n', '\n      '), projection)
        self.assertIn('displayName = ppDisplayName', projection)
        self.assertIn('in (displayName, displayName)', projection)
        self.assertIn(projection, verifier.extract(self.source))

    def test_wrong_provider_is_not_a_google_correspondence(self):
        with self.assertRaisesRegex(ValueError, 'Google provider delegate'):
            verifier.caller_projection(self.changed('completeProviderLogin ttl googleLoginSpec ',
                                                    'completeProviderLogin ttl appleLoginSpec '))

    def test_wrong_caller_argument_order_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Google provider delegate'):
            verifier.caller_projection(self.changed(
                '  completeProviderLogin ttl googleLoginSpec creationIntent acceptedTermsVersion ',
                '  completeProviderLogin ttl googleLoginSpec acceptedTermsVersion creationIntent '))

    def test_private_email_cannot_replace_provider_name(self):
        with self.assertRaisesRegex(ValueError, 'Google public-name constructor'):
            verifier.caller_projection(self.changed('ppDisplayName = initialAccountName gpName',
                                                    'ppDisplayName = gpEmail'))

    def test_private_email_cannot_become_provider_name_fallback(self):
        with self.assertRaisesRegex(ValueError, 'Google public-name constructor'):
            verifier.caller_projection(self.changed('ppDisplayName = initialAccountName gpName',
                                                    'ppDisplayName = initialAccountName (gpName <|> Just gpEmail)'))

    def test_wrong_subject_mapping_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Google public-name constructor'):
            verifier.caller_projection(self.changed('ppSubject = gpSubject', 'ppSubject = gpEmail'))

    def test_unrecognized_constructor_field_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Google public-name constructor'):
            verifier.caller_projection(self.changed('ppIssuer = gpIssuer', 'ppIssuer = "unsupported"'))

    def test_shared_private_email_name_substitution_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'shared public-name persistence path'):
            verifier.caller_projection(self.changed('let displayName = ppDisplayName',
                                                    'let displayName = primaryEmail'))

    def test_party_sink_divergence_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'shared public-name persistence path'):
            verifier.caller_projection(self.changed('partyDisplayName = displayName\n                        , partyIsOrg',
                                                    'partyDisplayName = primaryEmail\n                        , partyIsOrg'))

    def test_fan_sink_divergence_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'shared public-name persistence path'):
            verifier.caller_projection(self.changed('ensureFanProfileIfMissing pid displayName now\n                      tempPassword',
                                                    'ensureFanProfileIfMissing pid primaryEmail now\n                      tempPassword'))

    def test_missing_sink_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'shared FanProfile name sink'):
            verifier.caller_projection(self.changed('ensureFanProfileIfMissing pid displayName now\n                      tempPassword',
                                                    'pure ()\n                      tempPassword'))

    def test_shared_control_flow_change_needs_review_even_with_same_sinks(self):
        with self.assertRaisesRegex(ValueError, 'changed shared login control flow'):
            verifier.caller_projection(self.changed('pid <- insert partyRecord\n                  policyResult',
                                                    'pid <- insert partyRecord\n                  transactionUndo\n                  policyResult'))

    def test_provider_policy_change_needs_review(self):
        with self.assertRaisesRegex(ValueError, 'changed Google provider policy'):
            verifier.caller_projection(self.changed('plsProviderLabel = "Google"', 'plsProviderLabel = "Apple"'))

    def test_duplicate_google_caller_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported function declaration'):
            verifier.caller_projection(self.source + '\n' + verifier.function_block(self.source, 'completeGoogleLogin'))

    def test_duplicate_pure_validator_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported pure-function declaration'):
            verifier.extract(self.source + '\n' + verifier.function_block(self.source, 'validateGoogleIdTokenInfo'))


if __name__ == '__main__':
    unittest.main(verbosity=2)
