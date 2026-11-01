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
'''


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
    # This is a correspondence check, not execution of account persistence.
    if source.count('let displayName = initialAccountName gpName') != 1:
        raise ValueError('Review Google account-name caller correspondence')
    return HEADER + '\n\n'.join(blocks)


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
        for variant in ['intended', 'restore-email-fallback']:
            current = source.decode()
            if variant != 'intended':
                old = 'gpName = normalizedName\n'
                if current.count(old) != 1:
                    raise ValueError('Privacy mutation source shape changed')
                current = current.replace(old, 'gpName = normalizedName <|> Just normalizedEmail\n')
            directory = work / variant
            auth = directory / 'TDF/Auth'
            auth.mkdir(parents=True)
            (auth / 'AccountName.hs').write_bytes(helper)
            (auth / 'GoogleProfileValidation.hs').write_text(extract(current))
            (auth / 'GoogleProfileNameSpec.hs').write_text(test.decode().replace(
                'import TDF.ServerAuth\n', 'import TDF.Auth.GoogleProfileValidation\n'))
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
            expected = 0 if variant == 'intended' else 8
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
          'validator and name selector; eight restored-fallback failures. Full-module regressions run '
          'in the backend suite. No HTTP/SQL, provider certification or whole-program proof.'}, indent=2))


if __name__ == '__main__':
    run()
