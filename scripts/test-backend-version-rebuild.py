#!/usr/bin/env python3
"""Exercise cached Stack/GHC builds of the actual version module, without overrides."""
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Version.hs'
PRAGMA = '{-# OPTIONS_GHC -fforce-recomp #-}\n'
if os.environ.get('TDF_VERSION_REBUILD_OUTPUT'):
    OUT = Path(os.environ['TDF_VERSION_REBUILD_OUTPUT']).resolve()
    OUT.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    OUT.mkdir(mode=0o700)  # Never overwrite a prior passing or failed receipt.
else:
    OUT = Path(tempfile.mkdtemp(prefix='tdf-version-rebuild-')).resolve()
assert not os.path.lexists('/app/COMMIT'), 'runtime commit override must be absent'
assert PRAGMA in SOURCE.read_text(), 'candidate lacks module-local metadata refresh'
assert shutil.which('stack'), 'repository Stack toolchain is required'
INPUTS = ['tdf-hq/src/TDF/Version.hs', 'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock',
          'scripts/test-backend-version-rebuild.py']


def fingerprints():
    return {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in INPUTS}


report = {'schemaVersion': 1,
          'sourceRevision': subprocess.check_output(['git', 'rev-parse', 'HEAD'],
                                                    cwd=ROOT, text=True).strip(),
          'sourceWorktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'],
                                                              cwd=ROOT, text=True).strip()),
          'sourceFingerprints': fingerprints(),
          'sourceSha256': hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
          'status': 'running', 'cases': [],
          'scope': 'Actual TDF.Version in a minimal cached Stack package; Git HEAD changes '
                   'without changing Haskell source. Not whole-binary build attestation.'}


def save():
    (OUT / 'result.json').write_text(json.dumps(report, indent=2) + '\n')


def run(case, args, name):
    env = {key: value for key, value in os.environ.items()
           if key not in {'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR', 'GIT_INDEX_FILE'}}
    # CI retains cases beneath the root checkout. Until this case initializes
    # its own Git repository, ancestor discovery must not find the root's HEAD.
    env['GIT_CEILING_DIRECTORIES'] = str(case.parent)
    process = subprocess.Popen(args, cwd=case, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, text=True, start_new_session=True)
    try:
        stdout, stderr = process.communicate(timeout=600)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        try:
            stdout, stderr = process.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            stdout, stderr = process.communicate()
        (case / (name + '.log')).write_text(stdout + stderr + '\nTEST DEADLINE EXCEEDED\n')
        raise
    (case / (name + '.log')).write_text(stdout + stderr)
    assert process.returncode == 0, name + ' command failed; inspect retained log'
    return stdout.strip()


def git(case, *args):
    return run(case, ['git', '-c', 'core.hooksPath=/dev/null', '-c', 'commit.gpgsign=false',
                     '-c', 'user.name=TDF synthetic version test',
                     '-c', 'user.email=version-test@example.invalid', *args],
               'git-' + args[0])


def probe(label, pragma, force_dirty):
    case = OUT / label
    case.mkdir(mode=0o700)
    # Preserve the repository's resolver/extra dependency pins. The probe has
    # only this actual module, a version-number stub and an executable printer.
    for name in ['stack.yaml', 'stack.yaml.lock']:
        shutil.copyfile(ROOT / 'tdf-hq' / name, case / name)
    (case / 'TDF').mkdir()
    source = SOURCE.read_text()
    (case / 'TDF/Version.hs').write_text(source if pragma else source.replace(PRAGMA, '', 1))
    (case / 'tdf-version-probe.cabal').write_text('''cabal-version: 2.4
name: tdf-version-probe
version: 0.1.0.0
build-type: Simple
executable version-probe
  main-is: Main.hs
  hs-source-dirs: .
  other-modules: TDF.Version, Paths_tdf_hq
  build-depends: base, aeson, bytestring, text, time, template-haskell, gitrev
  default-language: Haskell2010
''')
    (case / 'Main.hs').write_text('''import TDF.Version
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as B
main :: IO ()
main = getVersionInfo >>= B.putStrLn . encode
''')
    # The stub replaces only Cabal's package-version module, never Git/time logic.
    (case / 'Paths_tdf_hq.hs').write_text('''module Paths_tdf_hq where
import Data.Version (Version, makeVersion)
version :: Version
version = makeVersion [0,1,0,0]
''')
    (case / 'README.md').write_text('Synthetic version cache fixture A\n')
    (case / '.gitignore').write_text('.stack-work/\n*.log\n')
    command = ['stack', '--no-terminal', 'build', '--fast']
    if force_dirty:
        command.append('--force-dirty')
    command.append('tdf-version-probe')
    run(case, command, 'initial-build-without-git')
    install = Path(run(case, ['stack', 'path', '--local-install-root'], 'install-root'))
    binary = install / 'bin/version-probe'

    def identity(name):
        # No inherited commit aliases, credentials or runtime metadata settings.
        result = subprocess.run([str(binary)], cwd=case,
                                env={'PATH': os.environ['PATH'], 'LANG': 'C.UTF-8'},
                                capture_output=True, text=True, timeout=15)
        assert result.returncode == 0, 'fixture executable failed'
        value = json.loads(result.stdout)
        (case / (name + '.json')).write_text(json.dumps(value, indent=2) + '\n')
        return value['commit']

    assert identity('initial-version') == 'dev', 'initial cache must reproduce non-Git build'
    git(case, 'init', '--quiet')
    git(case, 'add', '--', 'stack.yaml', 'stack.yaml.lock', 'tdf-version-probe.cabal',
        'Main.hs', 'Paths_tdf_hq.hs', 'TDF/Version.hs', 'README.md', '.gitignore')
    git(case, 'commit', '--quiet', '-m', 'Synthetic version fixture A')
    expected_a = git(case, 'rev-parse', 'HEAD')
    run(case, command, 'cached-build-at-A')
    actual_a = identity('version-at-A')
    if not pragma or not force_dirty:
        assert actual_a == 'dev' and actual_a != expected_a, 'named stale-cache control not detected'
        return {'case': label, 'result': 'expected-stale-metadata-detected',
                'expectedCommit': expected_a, 'reportedCommit': actual_a}

    assert actual_a == expected_a, 'corrected cached build must report A'
    objects = list((case / '.stack-work').rglob('Main.o'))
    assert len(objects) == 1, 'expected one unrelated probe object'
    unrelated_before = (objects[0].stat().st_mtime_ns,
                        hashlib.sha256(objects[0].read_bytes()).hexdigest())
    (case / 'README.md').write_text('Synthetic version cache fixture B; Haskell unchanged\n')
    git(case, 'add', '--', 'README.md')
    git(case, 'commit', '--quiet', '-m', 'Synthetic README-only fixture B')
    expected_b = git(case, 'rev-parse', 'HEAD')
    assert expected_a != expected_b
    run(case, command, 'cached-build-at-B')
    actual_b = identity('version-at-B')
    assert actual_b == expected_b, 'README-only commit must refresh compiled identity'
    assert 'Compiling TDF.Version' in (case / 'cached-build-at-B.log').read_text()
    unrelated_after = (objects[0].stat().st_mtime_ns,
                       hashlib.sha256(objects[0].read_bytes()).hexdigest())
    assert unrelated_before == unrelated_after, 'unrelated Main object must remain reusable'
    return {'case': label, 'result': 'passed', 'commitA': expected_a, 'commitB': expected_b,
            'reportedCommitB': actual_b, 'unrelatedObjectReused': True,
            'binarySha256': hashlib.sha256(binary.read_bytes()).hexdigest()}


save()
print('Backend version cache evidence: ' + str(OUT), flush=True)
try:
    for args in [('intended', True, True), ('without-stack-force-dirty', True, False),
                 ('without-module-force-recomp', False, True)]:
        report['cases'].append(probe(*args))
        save()
    assert fingerprints() == report['sourceFingerprints'], 'probe inputs changed during verification'
    assert subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip() \
        == report['sourceRevision'], 'candidate Git HEAD changed during verification'
    report['status'] = 'passed'
except BaseException as error:
    report['status'] = 'failed'
    report['error'] = type(error).__name__ + ': ' + str(error)
    raise
finally:
    save()
print(json.dumps({'status': report['status'], 'cases': len(report['cases']),
                  'evidence': str(OUT)}, sort_keys=True))
