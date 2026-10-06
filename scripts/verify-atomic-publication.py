#!/usr/bin/env python3
"""Actual isolated Haskell filesystem tests and two compiled negative controls.

Uses the Stack-selected compiler and dependencies; all build products and mutated
sources live in a temporary directory. Does not change application source files.
"""
import hashlib
import os
from pathlib import Path
import subprocess
import tempfile
import time
import signal
import stat

ROOT = Path(__file__).resolve().parent.parent
BACKEND = ROOT / 'tdf-hq'
SOURCE = BACKEND / 'src/TDF/Storage/AtomicPublication.hs'
source = SOURCE.read_text()
original = hashlib.sha256(SOURCE.read_bytes()).hexdigest()


def run(command, *, expect_failure=False):
    result = subprocess.run(command, cwd=BACKEND, text=True, capture_output=True, timeout=180)
    if (result.returncode != 0) != expect_failure:
        print(result.stdout); print(result.stderr)
        raise AssertionError('Atomic publication check returned an unexpected result')
    return result.stdout


with tempfile.TemporaryDirectory(prefix='tdf-atomic-publication-') as workspace:
    base = Path(workspace)
    variants = [
        ('intended', source, None),
        ('overwrite', source.replace('new exclusiveFlag', 'new 0'),
         'never overwrites an existing different final'),
        ('partial-final', source.replace('Handle, hClose,', 'Handle, IOMode(WriteMode), openBinaryFile, hClose,')
         .replace('openBinaryTempFile directory ".tdf-rider-pending.tmp"',
                  '((,) destination <$> openBinaryFile destination WriteMode)'),
         'keeps publication absent while writing and after asynchronous cancellation'),
    ]
    for name, text, match in variants:
        assert name == 'intended' or text != source
        directory = base / name
        module = directory / 'TDF/Storage/AtomicPublication.hs'
        module.parent.mkdir(parents=True)
        module.write_text(text)
        binary = directory / 'verify'
        run(['stack', 'exec', '--', 'ghc', '-O0', '-threaded', '-i'+str(directory), '-isrc', '-itest',
             '-outputdir', str(directory / 'objects'), '-o', str(binary), 'test/AtomicPublicationMain.hs'])
        output = run([str(binary), *([] if match is None else ['--match', match])], expect_failure=match is not None)
        if match is not None:
            assert '1 example, 1 failure' in output and match in output and 'expected:' in output, output
        else:
            assert '11 examples, 0 failures' in output, output
        print(name + ': ' + ('expected runtime counterexample detected' if match else '11 real filesystem checks passed'), flush=True)
        if match is None:
            crash = directory / 'crash'
            crash.mkdir(mode=0o700)
            child = subprocess.Popen([str(binary), '--crash-writer', str(crash)], cwd=BACKEND,
                                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            try:
                deadline = time.monotonic() + 10
                while not (crash / 'ready').exists():
                    assert child.poll() is None and time.monotonic() < deadline
                    time.sleep(.01)
                assert not (crash / 'rider').exists()
                child.kill()
                assert child.wait(timeout=5) == -signal.SIGKILL
                assert not (crash / 'rider').exists()
                staging = [p for p in crash.iterdir() if p.name.startswith('.tdf-rider-pending')]
                assert len(staging) == 1
                st = staging[0].lstat()
                assert stat.S_ISREG(st.st_mode) and stat.S_IMODE(st.st_mode) == 0o600 and st.st_nlink == 1
                assert staging[0].read_bytes() == b'A' * 4096
                print('SIGKILL: private single-link staging retained; no final published', flush=True)
            finally:
                if child.poll() is None:
                    child.kill(); child.wait(timeout=5)
assert hashlib.sha256(SOURCE.read_bytes()).hexdigest() == original
print('Atomic publication source unchanged: ' + original)
