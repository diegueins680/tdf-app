#!/usr/bin/env python3
"""Execution/property correspondence with source mutations, not a universal proof."""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Commerce/Money.hs'
MERCH_SOURCE = ROOT / 'tdf-hq/src/TDF/Commerce/Merch.hs'
TEST = ROOT / 'tdf-hq/test/TDF/Commerce/CheckoutMoneySpec.hs'
MAIN = 'import Test.Hspec\nimport qualified TDF.Commerce.CheckoutMoneySpec as Money\nmain = hspec Money.spec\n'


def run():
    source = SOURCE.read_text()
    merch_source = MERCH_SOURCE.read_text()
    controls = {
        'wrapped-checkout-aggregate': ('sum subtotals /= toInteger expected',
            'toInteger (fromInteger (sum subtotals) :: Int64) /= toInteger expected'),
        'unbounded-storage-quantity': ('toInteger quantity > toInteger (maxBound :: Int32)', 'False'),
        'hidden-zero-quantity': ('quantity <= 0 || toInteger quantity', 'quantity < 0 || toInteger quantity'),
        'wrapped-cart-aggregate': ('sum (map toInteger amounts)', 'toInteger (sum amounts)'),
        'wrapped-cart-product': ('narrowCartAmount (toInteger quantity * toInteger unit)',
            'narrowCartAmount (toInteger (quantity * unit))'),
        'merch-wrapped-lines': ('toInteger quantity * toInteger price', 'toInteger (quantity * price)'),
        'merch-wrapped-aggregate': ('total = sum amounts', 'total = toInteger (fromInteger (sum amounts) :: Int64)'),
        'merch-wrapped-commission': ('commissionBase * toInteger commissionBps `div` 10000',
            'toInteger ((fromInteger commissionBase :: Int64) * fromIntegral commissionBps `div` 10000)'),
        'merch-wrapped-payable': ('total = commissionBase + toInteger tax + toInteger shipping',
            'total = toInteger ((fromInteger commissionBase :: Int64) + tax + shipping)'),
    }
    records = []
    with tempfile.TemporaryDirectory(prefix='tdf-money-controls-') as temporary:
        directory = Path(temporary)
        main = directory/'Main.hs'
        main.write_text(MAIN)
        for name, replacement in [('intended', None), *controls.items()]:
            variant = directory/name
            module = variant/'TDF/Commerce/Money.hs'
            module.parent.mkdir(parents=True)
            merch_module = variant/'TDF/Commerce/Merch.hs'
            module.write_text(source)
            merch_module.write_text(merch_source)
            if replacement:
                old, new = replacement
                target_source = merch_source if name.startswith('merch-') else source
                target_module = merch_module if name.startswith('merch-') else module
                if target_source.count(old) != 1:
                    raise RuntimeError(f'Mutation shape drift: {name}')
                target_module.write_text(target_source.replace(old, new))
            command = ['stack', '--stack-yaml', str(ROOT/'tdf-hq/stack.yaml'),
                       'exec', '--', 'runghc', '-i'+str(variant),
                       '-i'+str(ROOT/'tdf-hq/test'), str(main), '--seed=20261004']
            result = subprocess.run(command, cwd=ROOT, env=os.environ,
                                    text=True, capture_output=True, timeout=120)
            output = result.stdout + result.stderr
            passed = result.returncode == 0 if replacement is None else (
                result.returncode != 0 and
                re.search(r'[1-9][0-9]* examples, [1-9][0-9]* failures?', output) is not None)
            records.append({'variant': name, 'exit': result.returncode,
                            'expectedResultObserved': bool(passed), 'output': output})
            if not passed:
                print(json.dumps({'runs': records}, indent=2))
                raise SystemExit(f'Checkout money conformance failed: {name}')
    print(json.dumps({'scope': '61 executable examples, including three 1000-case properties; nine source mutation controls',
                     'seed': 20261004,
                     'sourceSha256': hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
                     'merchSourceSha256': hashlib.sha256(MERCH_SOURCE.read_bytes()).hexdigest(),
                     'testSha256': hashlib.sha256(TEST.read_bytes()).hexdigest(),
                     'runs': records}, indent=2))


if __name__ == '__main__':
    run()
