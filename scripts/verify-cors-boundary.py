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
FILES = ['tdf-hq/src/TDF/Cors.hs', 'tdf-hq/src/TDF/Config.hs',
         'tdf-hq/src/TDF/Internationalization.hs', 'tdf-hq/test/TDF/CorsSpec.hs',
         'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock']


def verify():
    sources = {name: (ROOT/name).read_bytes() for name in FILES}
    mutations = {
        'intended': None,
        'production-classification-bypassed': (
            'isProduction = isProductionRuntime (zip productionRuntimeKeys runtimeValues)',
            'isProduction = False', 'rejects credentialed allow-all in production'),
        'implicit-preview-credentials': (
            'allowPagesDevWildcard = not isProduction',
            'allowPagesDevWildcard = True', 'does not implicitly trust production origin'),
    }
    results = []
    for name, mutation in mutations.items():
        with tempfile.TemporaryDirectory(prefix='tdf-cors-control-') as directory:
            work = Path(directory)
            for source, data in sources.items():
                target = work/source
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(data)
            if mutation:
                target = work/'tdf-hq/src/TDF/Cors.hs'
                original = target.read_text()
                if original.count(mutation[0]) != 1:
                    raise ValueError(f'{name}: source changed; review mutation correspondence')
                target.write_text(original.replace(mutation[0], mutation[1]))
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal',
                       'exec', '--', 'runghc', '-i', '-i' + str(work/'tdf-hq/src'),
                       str(work/'tdf-hq/test/TDF/CorsSpec.hs')]
            run = subprocess.run(command, cwd=work, capture_output=True, text=True, timeout=180)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', run.stdout)
            if not summary or int(summary[1]) < 25:
                raise RuntimeError(f'{name}: incomplete Hspec execution\n{run.stdout}\n{run.stderr}')
            failures = int(summary[2])
            if mutation:
                failure_details = run.stdout.partition('\nFailures:\n')[2]
                if run.returncode == 0 or failures == 0 or mutation[2] not in failure_details:
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
            'classification': 'finite runtime conformance with two detected implementation mutations',
            'scope': 'CORS startup and middleware decisions; no authentication, CSRF, browser or deployment proof',
            'results': results}


if __name__ == '__main__':
    print(json.dumps(verify(), indent=2))
