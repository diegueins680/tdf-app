#!/usr/bin/env python3
"""Check actual Servant transport provenance with six behavior-changing controls."""
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Commerce/DatafastClientIP.hs'
MAIN = ('import Test.Hspec\n'
        'import qualified TDF.Commerce.DatafastClientIPSpec as ClientIP\n'
        'main = hspec ClientIP.spec\n')


def main():
    source = SOURCE.read_text()
    controls = {
        'trust-browser-header': ('observed `elem` trusted', 'not (null headers)'),
        'trust-neighbour-proxy': ('observed `elem` trusted', 'observed `elem` trusted || not (customerAddress observed)'),
        'accept-reserved-customer': ('customerAddress customer && customer `notElem` trusted', 'True'),
        'accept-duplicate-header': ('[value] | BS.length value', '(value:_) | BS.length value'),
        'accept-address-list': ('let value = T.strip raw', 'let value = T.strip (T.takeWhile (/= \',\') raw)'),
        'omit-customer-ip': ('[("customer.ip", TE.encodeUtf8 (T.pack (show address)))]', '[]'),
    }
    reports = []
    with tempfile.TemporaryDirectory(prefix='tdf-dataweb-client-ip-controls-') as temporary:
        directory = Path(temporary)
        entry = directory / 'Main.hs'
        entry.write_text(MAIN)
        for name, control in [('intended', None), *controls.items()]:
            variant = directory / name
            target = variant / 'TDF/Commerce/DatafastClientIP.hs'
            target.parent.mkdir(parents=True)
            content = source
            if control:
                old, new = control
                if content.count(old) != 1:
                    raise RuntimeError(f'Control shape drift: {name}')
                content = content.replace(old, new)
            target.write_text(content)
            command = ['stack', '--stack-yaml', str(ROOT / 'tdf-hq/stack.yaml'),
                       'exec', '--', 'runghc', '-package=iproute', '-package=wai-extra',
                       '-package=servant-server', '-i' + str(variant),
                       '-i' + str(ROOT / 'tdf-hq/src'), '-i' + str(ROOT / 'tdf-hq/test'),
                       str(entry), '--seed=20261114', '--fail-on=empty', '--no-color']
            result = subprocess.run(command, cwd=ROOT, env=os.environ,
                                    capture_output=True, text=True, timeout=180)
            output = result.stdout + result.stderr
            summaries = re.findall(r'^(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?$', output, re.M)
            complete = len(summaries) == 1 and summaries[0][0] == '11' and not summaries[0][2]
            failures = int(summaries[0][1]) if complete else None
            accepted = (complete and result.returncode != 0 and failures > 0) if control else (
                complete and result.returncode == 0 and failures == 0
                and re.findall(r'\+\+\+ OK, passed (\d+) tests\.', output) == ['100'])
            reports.append({'variant': name, 'exit': result.returncode,
                            'examples': 11 if complete else None, 'failures': failures,
                            'expected_result_observed': bool(accepted), 'output': output})
            if not accepted:
                print(json.dumps({'runs': reports}, indent=2))
                raise SystemExit(f'Dataweb client IP control failed: {name}')
    print(json.dumps({'scope': '11 actual Servant/Wai cases, one 100-case property and six source controls; no provider HTTP or PostgreSQL',
                      'runs': reports}, indent=2))


if __name__ == '__main__':
    main()
