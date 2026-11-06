#!/usr/bin/env python3
"""Execute the actual intent store and independently reject three clock mutants.

Requires the owned, migrated provider-retry database. Compile/tool failures do
not count as counterexamples. This is regression evidence, not a universal proof.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = Path('tdf-hq/src/TDF/Commerce/PaymentIntentStore.hs')
PROBE = Path('tdf-hq/test/PaymentIntentExpiryProbe.hs')
CLOCK = '      admissionTimes <- rawSql "SELECT clock_timestamp()" [] :: SqlPersistT IO [Single UTCTime]\n'


def run(output=None):
    if any(os.environ.get(key) for key in ('PGHOSTADDR', 'PGSERVICE', 'PGSERVICEFILE')):
        raise ValueError('Unset libpq routing overrides before expiry verification')
    subprocess.run(['node', '--input-type=module', '-e',
                    "import { disposablePostgresUrl } from './scripts/lib/disposable-postgres-url.mjs'; "
                    "const u = disposablePostgresUrl(process.env.TDF_PROVIDER_RETRY_DATABASE_URL, "
                    "{ci:process.env.CI==='true'}); "
                    "if (u.pathname !== '/tdf_provider_retry_test') throw Error('Wrong fixture database');"],
                   cwd=ROOT, check=True)
    paths = [SOURCE, PROBE, Path(__file__).relative_to(ROOT),
             Path('tdf-hq/stack.yaml'), Path('tdf-hq/stack.yaml.lock'),
             Path('tdf-hq/tdf-hq.cabal'), Path('tdf-hq/test/TDF/Test/DisposablePostgres.hs')]
    paths.append(Path('scripts/lib/disposable-postgres-url.mjs'))
    paths += sorted(Path(p) for p in subprocess.check_output(
        ['git', 'ls-files', 'tdf-hq/src/TDF/Commerce'], cwd=ROOT, text=True).splitlines())
    snapshots = {str(p): (ROOT/p).read_bytes() for p in set(paths)}
    source = snapshots[str(SOURCE)].decode()
    if source.count(CLOCK) != 1 or source.count('      checkoutRows <- rawSql\n') != 1:
        raise ValueError('Clock/source shape changed; review mutation placement')
    variants = {
        'intended': source,
        'request-time': source.replace(CLOCK, '      let admissionTimes = [Single picOccurredAt]\n'),
        'transaction-start-time': source.replace('SELECT clock_timestamp()', 'SELECT CURRENT_TIMESTAMP'),
        'clock-before-lock': source.replace(CLOCK, '').replace(
            '      checkoutRows <- rawSql\n', CLOCK+'      checkoutRows <- rawSql\n'),
    }
    report = {
        'schemaVersion': 1,
        'claim': 'Fresh intent expiry is checked using database time after the checkout row lock.',
        'assurance': 'TESTED',
        'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
        'worktreeDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT)),
        'sourceSha256': {p: hashlib.sha256(b).hexdigest() for p, b in sorted(snapshots.items())},
        'tools': {}, 'runs': [],
        'limits': ['Owned synthetic PostgreSQL fixture; no provider HTTP or production data.',
                   'Admission instant, not provider completion or transaction commit deadline.',
                   'Database wall clock, transaction/lock behavior and compiler are trusted.'],
    }
    for name, command in {
        'stack': ['stack', '--version'],
        'ghc': ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'),
                'exec', '--', 'ghc', '--numeric-version'],
        'postgres': ['psql', os.environ['TDF_PROVIDER_RETRY_DATABASE_URL'],
                     '-XAt', '-v', 'ON_ERROR_STOP=1', '-c', 'SELECT version()'],
    }.items():
        report['tools'][name] = subprocess.check_output(command, cwd=ROOT, text=True).splitlines()[0]
    with tempfile.TemporaryDirectory(prefix='tdf-intent-expiry-') as temporary:
        directory = Path(temporary)
        probe = directory/'Main.hs'
        probe.write_bytes(snapshots[str(PROBE)])
        for name, variant in variants.items():
            modules = directory/name
            for path, content in snapshots.items():
                if path.startswith('tdf-hq/src/') or path.startswith('tdf-hq/test/TDF/'):
                    copied = modules/Path(path).relative_to('tdf-hq').relative_to(
                        'src' if path.startswith('tdf-hq/src/') else 'test')
                    copied.parent.mkdir(parents=True, exist_ok=True)
                    copied.write_bytes(content)
            target = modules/'TDF/Commerce/PaymentIntentStore.hs'
            target.write_text(variant)
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'),
                       'exec', '--', 'runghc', '-i'+str(modules),
                       '-i'+str(ROOT/'tdf-hq/src'), '-i'+str(ROOT/'tdf-hq/test'),
                       str(probe), '--seed=20261025', '--fail-on=empty']
            result = subprocess.run(command, cwd=ROOT, text=True, capture_output=True, timeout=90)
            log = result.stdout+result.stderr
            match = re.search(r'7 examples, (\d+) failures?', log)
            failures = int(match[1]) if match else None
            expected_failures = 0 if name == 'intended' else 7 if name == 'request-time' else 2
            expected = failures == 0 and result.returncode == 0 if name == 'intended' else (
                failures == expected_failures and result.returncode != 0
                and 'Canonical checkout has expired' in log)
            if name in ('transaction-start-time', 'clock-before-lock'):
                expected = expected and 'checks time after a witnessed checkout lock wait' in log
            report['runs'].append({'variant': name, 'exit': result.returncode,
                                   'expectedResultObserved': bool(expected), 'output': log})
    report['sourcesUnchanged'] = all((ROOT/p).read_bytes() == b for p, b in snapshots.items())
    report['passed'] = report['sourcesUnchanged'] and all(
        r['expectedResultObserved'] for r in report['runs'])
    rendered = json.dumps(report, indent=2)+'\n'
    if output:
        Path(output).write_text(rendered)
    print(rendered, end='')
    if not report['passed']:
        raise SystemExit('Payment intent expiry verification failed')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--output')
    run(parser.parse_args().output)
