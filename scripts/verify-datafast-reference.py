#!/usr/bin/env python3
"""Run the real PostgreSQL reference verifier and behavior-changing controls.

Invoked inside Stack by test-datafast-cart.sh --reference-controls, which owns
the isolated database and authoritative six-table schema. Never contacts a
provider or uses a merchant credential.
"""
import json
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
SOURCE = ROOT / 'tdf-hq/src/TDF/Server/ServiceStorefront.hs'
RUNNER = ROOT / 'tdf-hq/test/integration/DatafastCart.hs'


def main():
    source = SOURCE.read_text()
    start = source.index('validateBoundDatafastSuccessfulPayment checkout environment')
    end = source.index('\nvalidateDatafastSuccessfulPayment\n', start)
    verifier = source[start:end]
    controls = {
        'ignore-checkout-resource': ('binding.provider_resource_id = ?', '?::text IS NOT NULL'),
        'ignore-bound-domain': ('binding.merchant_reference = checkout.domain_order_id', 'TRUE'),
        'ignore-binding-environment': ('binding.environment = attempt.environment', 'TRUE'),
        'ignore-binding-merchant': ('binding.merchant_account_ref = attempt.merchant_account_ref', 'TRUE'),
        'ignore-binding-amount': ('binding.amount_minor = checkout.total_minor', 'TRUE'),
        'trust-provider-reference': ('validateDatafastSuccessfulPayment expected amount currency status',
            'validateDatafastSuccessfulPayment (fromMaybe expected (sdfpsMerchantTransactionId status)) amount currency status'),
    }
    reports = []
    with tempfile.TemporaryDirectory(prefix='tdf-dataweb-reference-controls-') as temporary:
        for name, control in [('intended', None), *controls.items()]:
            directory = Path(temporary) / name
            target = directory / 'TDF/Server/ServiceStorefront.hs'
            target.parent.mkdir(parents=True)
            variant = verifier
            if control:
                old, new = control
                if variant.count(old) != 1:
                    raise RuntimeError(f'Reference control shape drift: {name}')
                variant = variant.replace(old, new)
            target.write_text(source[:start] + variant + source[end:])
            result = subprocess.run(
                ['runghc', '-package=tdf-hq', '-i' + str(directory), str(RUNNER),
                 '--match=Dataweb bound transaction reference', '--seed=20261114',
                 '--fail-on=empty', '--no-color'],
                cwd=ROOT / 'tdf-hq', capture_output=True, text=True, timeout=180)
            output = result.stdout + result.stderr
            summaries = re.findall(r'^(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?$',
                                   output, re.M)
            complete = len(summaries) == 1 and summaries[0][0] == '8' and not summaries[0][2]
            failures = int(summaries[0][1]) if complete else None
            assertions_only = 'uncaught exception' not in output.lower()
            accepted = complete and assertions_only and (
                (result.returncode == 0 and failures == 0) if control is None
                else (result.returncode != 0 and failures > 0
                      and ('expected:' in output or 'predicate failed on' in output)))
            reports.append({'variant': name, 'exit': result.returncode, 'examples': 8 if complete else None,
                            'failures': failures, 'expected_result_observed': bool(accepted),
                            'output': output})
            if not accepted:
                print(json.dumps({'runs': reports}, indent=2))
                raise SystemExit(f'Dataweb reference control failed: {name}')
    print(json.dumps({'scope': 'Exact bound reference PostgreSQL verifier; no provider HTTP',
                      'runs': reports}, indent=2))


if __name__ == '__main__':
    main()
