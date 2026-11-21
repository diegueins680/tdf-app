#!/usr/bin/env python3
"""Execute cart conservation/refusal properties and six behavior-changing controls."""
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Commerce/DatafastConfig.hs'
MONEY = ROOT / 'tdf-hq/src/TDF/Commerce/Money.hs'
MAIN = ('import Test.Hspec\n'
        'import qualified TDF.Commerce.DatafastConfigSpec as Datafast\n'
        'main = hspec Datafast.spec\n')


def main():
    source, money = SOURCE.read_text(), MONEY.read_text()
    controls = {
        'drop-cart': ('cart', 'pure (concat (zipWith renderLine [0 :: Int ..] linesToSend))', 'pure []'),
        'ignore-currency': ('cart', 'T.toUpper (T.strip currency) == "USD"', 'True'),
        'ignore-decimal-bound': ('cart', 'amount <= 999999999999', 'True'),
        'ignore-quantity-bound': ('cart', 'dfclQuantity line <= 99999', 'True'),
        'accept-invalid-name': ('cart',
            "not (T.any isControl description) && not (T.any (== '&') description)", 'True'),
        'ignore-conservation': ('money', 'sum subtotals /= toInteger expected', 'False'),
    }
    reports = []
    with tempfile.TemporaryDirectory(prefix='tdf-dataweb-cart-controls-') as temporary:
        directory = Path(temporary)
        entry = directory / 'Main.hs'
        entry.write_text(MAIN)
        for name, control in [('intended', None), *controls.items()]:
            variant = directory / name
            target = variant / 'TDF/Commerce/DatafastConfig.hs'
            target.parent.mkdir(parents=True)
            variant_money = target.with_name('Money.hs')
            target.write_text(source)
            variant_money.write_text(money)
            if control:
                kind, old, new = control
                module = target if kind == 'cart' else variant_money
                original = module.read_text()
                if original.count(old) != 1:
                    raise RuntimeError(f'Control shape drift: {name}')
                module.write_text(original.replace(old, new))
            command = ['stack', '--stack-yaml', str(ROOT / 'tdf-hq/stack.yaml'),
                       'exec', '--', 'runghc', '-i' + str(variant),
                       '-i' + str(ROOT / 'tdf-hq/src'), '-i' + str(ROOT / 'tdf-hq/test'),
                       str(entry), '--seed=20261110', '--fail-on=empty', '--no-color']
            result = subprocess.run(command, cwd=ROOT, env=os.environ,
                                    capture_output=True, text=True, timeout=180)
            output = result.stdout + result.stderr
            summaries = re.findall(r'^(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?$',
                                   output, re.M)
            complete = len(summaries) == 1 and summaries[0][0] == '21' and not summaries[0][2]
            failures = int(summaries[0][1]) if complete else None
            if control:
                accepted = complete and result.returncode != 0 and failures > 0
            else:
                accepted = (complete and result.returncode == 0 and failures == 0
                            and re.findall(r'\+\+\+ OK, passed (\d+) tests\.', output) == ['100', '100'])
            reports.append({'variant': name, 'exit': result.returncode,
                            'examples': 21 if complete else None, 'failures': failures,
                            'expected_result_observed': bool(accepted), 'output': output})
            if not accepted:
                print(json.dumps({'runs': reports}, indent=2))
                raise SystemExit(f'Dataweb cart control failed: {name}')
    print(json.dumps({'scope': '21 Hspec cases, two 100-case properties and six source controls; no provider HTTP',
                      'runs': reports}, indent=2))


if __name__ == '__main__':
    main()
