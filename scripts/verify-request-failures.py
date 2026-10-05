#!/usr/bin/env python3
"""Run real Haskell middleware tests and controlled broken implementations.

This is finite runtime testing, not a browser exploit or a universal formal proof.
The same spec is included in the full backend suite. No network is contacted by
the middleware; Stack may need its pinned snapshot installed beforehand.
"""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
FILES = ['tdf-hq/src/TDF/App/FailureBoundary.hs', 'tdf-hq/src/TDF/Cors.hs',
         'tdf-hq/src/TDF/Config.hs', 'tdf-hq/src/TDF/Internationalization.hs',
         'tdf-hq/test/TDF/FailureBoundarySpec.hs', 'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock']


def verify():
    sources = {name: (ROOT/name).read_bytes() for name in FILES}
    mutations = {
        'intended': None,
        'catch-cancellation': [
            ('import Control.Exception (SomeException,', 'import qualified Control.Exception as Unsafe\nimport Control.Exception (SomeException,'),
            ('Safe.handleAny failed (next request sendOnce)', 'Unsafe.handle failed (next request sendOnce)'),
        ],
        'respond-twice': [
            ('if started then throwIO exception else do', 'if False then throwIO exception else do'),
            ('when alreadyStarted (throwIO (userError "Response delivery already started"))', 'pure ()'),
        ],
        'render-private-exception': [
            ('import Data.Text (Text)', 'import Data.Text (Text, pack)'),
            ('logger "[HTTP] Unhandled request failure"', 'logger (pack (show exception))'),
        ],
    }
    expected = {'catch-cancellation': 'propagates cancellation',
                'respond-twice': 'never invokes the response callback again',
                'render-private-exception': 'never logs the exception payload'}
    results = []
    for name, mutation in mutations.items():
        with tempfile.TemporaryDirectory(prefix='tdf-request-failure-control-') as directory:
            work = Path(directory)
            for source, data in sources.items():
                target = work/source
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(data)
            if mutation:
                target = work/'tdf-hq/src/TDF/App/FailureBoundary.hs'
                original = target.read_text()
                for needle, replacement in mutation:
                    if original.count(needle) != 1:
                        raise ValueError(f'{name}: source changed; review mutation correspondence')
                    original = original.replace(needle, replacement)
                target.write_text(original)
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal',
                       'exec', '--', 'runghc', '-i', '-i' + str(work/'tdf-hq/src'),
                       str(work/'tdf-hq/test/TDF/FailureBoundarySpec.hs')]
            run = subprocess.run(command, cwd=work, capture_output=True, text=True, timeout=180)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', run.stdout)
            if not summary or int(summary[1]) < 13:
                raise RuntimeError(f'{name}: incomplete Hspec execution\n{run.stdout}\n{run.stderr}')
            failures = int(summary[2])
            if mutation:
                failure_details = run.stdout.partition('\nFailures:\n')[2]
                if run.returncode == 0 or failures == 0 or expected[name] not in failure_details:
                    raise AssertionError(f'{name}: invalid implementation escaped detection')
            elif run.returncode != 0 or failures != 0:
                raise AssertionError(f'Intended implementation failed\n{run.stdout}\n{run.stderr}')
            results.append({'control': name, 'exitCode': run.returncode,
                            'examples': int(summary[1]), 'failures': failures,
                            'output': run.stdout, 'stderr': run.stderr})
    for name, data in sources.items():
        if (ROOT/name).read_bytes() != data:
            raise RuntimeError(f'Source changed during execution: {name}')
    return {'sourceFingerprints': {p: hashlib.sha256(b).hexdigest() for p, b in sources.items()},
            'classification': 'finite runtime conformance with three detected implementation mutations',
            'scope': 'Request/diagnostic response privacy, cancellation and single response; finite runtime controls, no whole-server or production proof',
            'results': results}


if __name__ == '__main__':
    print(json.dumps(verify(), indent=2))
