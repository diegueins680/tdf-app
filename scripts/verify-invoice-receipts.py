#!/usr/bin/env python3
"""Run actual receipt arithmetic properties and three controlled source mutations.

Stack uses the pinned toolchain; copied sources cannot mutate the workspace.
"""
import hashlib
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
FILES = ['tdf-hq/src/TDF/Invoice/Receipt.hs', 'tdf-hq/test/TDF/Invoice/ReceiptSpec.hs',
         'tdf-hq/stack.yaml', 'tdf-hq/stack.yaml.lock']


def verify():
    sources = {name: (ROOT/name).read_bytes() for name in FILES}
    mutations = {
        'intended': None,
        'ignore-header': [('unless (actual == stored)', 'unless True')],
        'ignore-line-total': [('unless (total == toInteger storedTotal)', 'unless True')],
        'multiply-before-widening': [('toInteger quantity * toInteger unit', 'toInteger (quantity * unit)')],
    }
    expected = {'ignore-header': 'rejects inconsistent stored header values',
                'ignore-line-total': 'rejects a stored line that does not match',
                'multiply-before-widening': 'rejects multiplication overflow that wraps back to nonnegative zero'}
    results = []
    for name, mutation in mutations.items():
        with tempfile.TemporaryDirectory(prefix='tdf-receipt-control-') as directory:
            work = Path(directory)
            for source, data in sources.items():
                target = work/source
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(data)
            if mutation:
                target = work/'tdf-hq/src/TDF/Invoice/Receipt.hs'
                original = target.read_text()
                for needle, replacement in mutation:
                    if original.count(needle) != 1:
                        raise ValueError(f'{name}: source changed; review mutation correspondence')
                    original = original.replace(needle, replacement)
                target.write_text(original)
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'), '--no-terminal',
                       'exec', '--', 'runghc', '-i', '-i' + str(work/'tdf-hq/src'),
                       str(work/'tdf-hq/test/TDF/Invoice/ReceiptSpec.hs')]
            run = subprocess.run(command, cwd=work, capture_output=True, text=True, timeout=180)
            summary = re.search(r'(\d+) examples?, (\d+) failures?', run.stdout)
            if not summary or int(summary[1]) < 10:
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
            'scope': 'Exact invoice snapshot arithmetic; finite QuickCheck domains and explicit Int boundaries; no whole-server or production proof',
            'results': results}


if __name__ == '__main__':
    print(json.dumps(verify(), indent=2))
