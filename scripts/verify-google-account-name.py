#!/usr/bin/env python3
"""Execute exact pure ServerAuth fragments and the full-module regression spec.

The normal backend suite imports the real ServerAuth module. This small local
probe extracts its complete JSON decoder and pure validation dependencies to
avoid substituting a hand-written decoder. Extraction/caller correspondence,
Haskell libraries and the compiler are trusted; HTTP/SQL refinement is separate.
"""
import hashlib
import json
import os
import platform
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SERVER = ROOT / 'tdf-hq/src/TDF/ServerAuth.hs'
HELPER = ROOT / 'tdf-hq/src/TDF/Auth/AccountName.hs'
TEST = ROOT / 'tdf-hq/test/TDF/Auth/GoogleProfileNameSpec.hs'
FUNCTIONS = [
    'normalizeAuthEmailAddress', 'maxAuthEmailAddressChars',
    'maxAuthEmailLocalPartChars', 'maxAuthEmailDomainLabelChars',
    'isValidAuthEmailAddress', 'hasValidAuthTopLevelLabel',
    'isValidAuthEmailLocalPart', 'isValidAuthEmailLocalChar',
    'isValidAuthDomainLabel', 'isValidAuthDomainChar',
    'validateGoogleIdTokenInfo', 'issuerAllowed', 'invalidGoogleSubjectChar',
    'maxGoogleSubjectChars', 'sanitizeGoogleProfileName',
    'maxSignupDisplayNameChars', 'isUnsafeSignupNameChar',
    'hasMeaningfulDisplayNameChar', 'cleanOptional', 'validateGoogleTokenExpiry',
]
HEADER = '''{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module TDF.Auth.GoogleProfileValidation where
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Value(..), withObject, (.:), (.:?))
import Data.Char (GeneralCategory(..), generalCategory, isAlphaNum, isAscii, isAsciiLower, isControl, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import TDF.Auth.AccountName (initialAccountName)
'''

# Explicit review boundaries for the effectful control flow and provider policy.
# These hashes supplement the derived name projection; they do not prove SQL or
# HTTP refinement. Any change to either block needs a new correspondence review.
SHARED_LOGIN_SHA256 = 'c0fa5ceb99d103b399e475b3066378825b92901c805fc46e256dbb8fa2a8f718'
GOOGLE_POLICY_SHA256 = '424142fce7ad71ceaa52ba98616bee88d2d6b7102a846b5d05bc8c549f2b7dd5'


def function_block(source, name):
    matches = list(re.finditer(r'^' + re.escape(name) + r' ::', source, re.M))
    if len(matches) != 1:
        raise ValueError('Unsupported function declaration: ' + name)
    start = matches[0].start()
    following = re.search(r'\n[a-z][A-Za-z0-9_\']* ::', source[matches[0].end():])
    if following is None:
        raise ValueError('Missing function boundary: ' + name)
    return source[start:matches[0].end() + following.start()]


def caller_projection(source):
    """Derive the pure name path from the reviewed caller and both actual sinks.

    Accept only this explicit Google delegate shape, not arbitrary text containing
    the expected names. The copied ProviderProfile constructor and extracted sink
    expressions are executed below, alongside all original regression assertions.
    The reviewed shared-flow hash rejects changes outside this small projection.
    """
    caller = function_block(source, 'completeGoogleLogin')
    signature = ('completeGoogleLogin :: SessionTokenTtl -> Maybe Bool -> Maybe Text -> '
                 'Maybe Bool -> Maybe Text -> Maybe LoginRequest -> GoogleProfile -> '
                 'SqlPersistT IO (Either Text LoginResponse)\n')
    delegate = ('completeGoogleLogin ttl creationIntent acceptedTermsVersion marketingConsent '
                'requestedOnboardingIntent linkProof GoogleProfile{..} =\n'
                '  completeProviderLogin ttl googleLoginSpec creationIntent acceptedTermsVersion '
                'marketingConsent requestedOnboardingIntent linkProof\n    ')
    if not caller.startswith(signature + delegate):
        raise ValueError('Unsupported Google provider delegate')
    record = caller[len(signature + delegate):].strip()
    match = re.fullmatch(
        r'ProviderProfile\n      \{ ppIssuer = gpIssuer\n'
        r'      , ppSubject = gpSubject\n      , ppEmail = Just gpEmail\n'
        r'      , ppDisplayName = (?P<name>[^\n]+)\n      \}', record)
    if match is None or match['name'] != 'initialAccountName gpName':
        raise ValueError('Unsupported Google public-name constructor')
    shared = function_block(source, 'completeProviderLogin')

    def unique_expression(pattern, description):
        matches = list(re.finditer(pattern, shared, re.M))
        if len(matches) != 1:
            raise ValueError('Unsupported shared ' + description)
        return matches[0]['expression']

    selected = unique_expression(r'^\s+let displayName = (?P<expression>[^\n]+)$', 'name binding')
    party = unique_expression(r'^\s+, partyDisplayName = (?P<expression>[^\n]+)$', 'Party name sink')
    fan = unique_expression(r'^\s+ensureFanProfileIfMissing pid (?P<expression>[^\n]+) now$', 'FanProfile name sink')
    if (selected, party, fan) != ('ppDisplayName', 'displayName', 'displayName'):
        raise ValueError('Unsupported shared public-name persistence path')
    if hashlib.sha256(shared.encode()).hexdigest() != SHARED_LOGIN_SHA256:
        raise ValueError('Review changed shared login control flow')
    policy = function_block(source, 'googleLoginSpec')
    if hashlib.sha256(policy.encode()).hexdigest() != GOOGLE_POLICY_SHA256:
        raise ValueError('Review changed Google provider policy')
    # Every RHS below is copied from the actual caller/helper, never an alternate
    # implementation of the selector or a hand-written provider profile.
    return ('googleAccountPublicNames :: GoogleProfile -> (Text, Text)\n'
            'googleAccountPublicNames GoogleProfile{..} =\n'
            '  let ProviderProfile{..} = ' + record.replace('\n', '\n      ') + '\n'
            '      displayName = ' + selected + '\n'
            '  in (' + party + ', ' + fan + ')\n')


def extract(source):
    start = source.index('data GoogleIdTokenInfo =')
    end = source.index('\ndata SignupDbError', start)
    blocks = [source[start:end]]
    for name in FUNCTIONS:
        matches = list(re.finditer(r'^' + re.escape(name) + r' ::', source, re.M))
        if len(matches) != 1:
            raise ValueError('Unsupported pure-function declaration: ' + name)
        start = matches[0].start()
        following = re.search(r'\n[a-z][A-Za-z0-9_\']* ::', source[matches[0].end():])
        if following is None:
            raise ValueError('Missing function boundary: ' + name)
        end = matches[0].end() + following.start()
        blocks.append(source[start:end])
    return HEADER + '\n\n'.join(blocks) + '\n\n' + caller_projection(source)


def run():
    inputs = [SERVER, HELPER, TEST, ROOT/'tdf-hq/test/TDF/Auth/AccountNameSpec.hs',
              Path(__file__).resolve(), ROOT/'tdf-hq/stack.yaml',
              ROOT/'tdf-hq/stack.yaml.lock', ROOT/'tdf-hq/tdf-hq.cabal']
    snapshot = {p: p.read_bytes() for p in inputs}
    source, helper, test = (snapshot[p] for p in [SERVER, HELPER, TEST])
    if test.decode().count('import TDF.ServerAuth\n') != 1:
        raise ValueError('Regression import shape changed')
    results = []
    with tempfile.TemporaryDirectory(prefix='tdf-google-name-control-') as temporary:
        work = Path(temporary)
        for variant in ['intended', 'restore-email-fallback', 'unsafe-neutral-name']:
            current = source.decode()
            current_helper = helper.decode()
            if variant == 'restore-email-fallback':
                old = 'gpName = normalizedName\n'
                if current.count(old) != 1:
                    raise ValueError('Privacy mutation source shape changed')
                current = current.replace(old, 'gpName = normalizedName <|> Just normalizedEmail\n')
            if variant == 'unsafe-neutral-name':
                if current_helper.count('_ -> "Cuenta TDF"') != 1:
                    raise ValueError('Neutral-name mutation source shape changed')
                current_helper = current_helper.replace('_ -> "Cuenta TDF"', '_ -> "private@example.invalid"')
            directory = work / variant
            auth = directory / 'TDF/Auth'
            auth.mkdir(parents=True)
            (auth / 'AccountName.hs').write_text(current_helper)
            (auth / 'GoogleProfileValidation.hs').write_text(extract(current))
            original_assertion = '          initialAccountName (gpName profile) `shouldBe` maybe "Cuenta TDF" id expectedName\n'
            if test.decode().count(original_assertion) != 1:
                raise ValueError('Regression selector assertion shape changed')
            derived_assertion = ('          googleAccountPublicNames profile `shouldBe`\n'
                                 '            (maybe "Cuenta TDF" id expectedName, maybe "Cuenta TDF" id expectedName)\n')
            projected_test = test.decode().replace(
                'import TDF.ServerAuth\n', 'import TDF.Auth.GoogleProfileValidation\n')
            projected_test = projected_test.replace(
                'GoogleProfile(..), validateGoogleIdTokenInfo, validateGoogleTokenExpiry',
                'GoogleProfile(..), validateGoogleIdTokenInfo, validateGoogleTokenExpiry, googleAccountPublicNames')
            (auth / 'GoogleProfileNameSpec.hs').write_text(projected_test.replace(
                original_assertion, original_assertion + derived_assertion))
            main = directory / 'Main.hs'
            main.write_text('import Test.Hspec\nimport qualified TDF.Auth.GoogleProfileNameSpec as P\n'
                            'import qualified TDF.Auth.AccountNameSpec as A\nmain = hspec (P.spec >> A.spec)\n')
            command = ['stack', '--stack-yaml', str(ROOT / 'tdf-hq/stack.yaml'),
                       'exec', '--', 'runghc', '-i', '-i' + str(directory),
                       '-i' + str(ROOT / 'tdf-hq/test'), str(main), '--seed=20261027',
                       '--format=specdoc', '--no-color', '--fail-on=empty']
            p = subprocess.run(command, cwd=ROOT, env=os.environ, text=True,
                               capture_output=True, timeout=180)
            output = p.stdout + p.stderr
            summaries = re.findall(r'^19 examples, (\d+) failures?(?:, (\d+) pending)?\s*$', output, re.M)
            expected = {'intended': 0, 'restore-email-fallback': 8, 'unsafe-neutral-name': 11}[variant]
            accepted = (len(summaries) == 1 and int(summaries[0][0]) == expected
                        and int(summaries[0][1] or 0) == 0
                        and (p.returncode == 0 if expected == 0 else p.returncode != 0))
            results.append({'variant': variant, 'exit': p.returncode,
                            'expectedFailures': expected, 'expectedResultObserved': accepted,
                            'output': output})
            if not accepted:
                print(json.dumps({'runs': results}, indent=2))
                raise SystemExit('Google decoder-to-name privacy verification failed: ' + variant)
    if {p: p.read_bytes() for p in inputs} != snapshot:
        raise RuntimeError('Privacy verification inputs changed during execution')
    versions = {'python': platform.python_version(),
                'stack': subprocess.check_output(['stack', '--numeric-version'], text=True).strip(),
                'ghc': subprocess.check_output(['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'),
                                               'exec', '--', 'ghc', '--numeric-version'], text=True).strip()}
    for package in ['hspec', 'aeson', 'text', 'time']:
        versions[package] = subprocess.check_output(
            ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), 'exec', '--',
             'ghc-pkg', 'field', package, 'version', '--simple-output'], text=True).strip()
    print(json.dumps({'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'],
          cwd=ROOT, text=True).strip(),
          'worktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'],
                                cwd=ROOT, text=True).strip()),
          'sources': {str(p.relative_to(ROOT)): hashlib.sha256(data).hexdigest()
                      for p, data in snapshot.items()},
          'tools': versions,
          'runs': results, 'scope': '19 compiled source-fragment examples, including the actual JSON decoder, '
          'validator, name selector and actual shared-provider Party/FanProfile name projection; '
          'all original assertions retained, eight restored-email-fallback and eleven unsafe-neutral-name '
          'failures. Reviewed control-flow '
          'hashes are correspondence assumptions, not persistence execution. Full-module regressions run '
          'in the backend suite. No HTTP/SQL, provider certification or whole-program proof.'}, indent=2))


if __name__ == '__main__':
    run()
