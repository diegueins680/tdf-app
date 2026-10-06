import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import Ajv from 'ajv';
import { parse } from 'yaml';

export function validateInvoiceReceiptSamples(samples) {
  const contract = parse(readFileSync(new URL('../tdf-hq/docs/openapi/api.yaml', import.meta.url), 'utf8'));
  const ajv = new Ajv({ allErrors: true, nullable: true });
  ajv.addFormat('int64', { type: 'number', validate: value => Number.isInteger(value)
    && BigInt(value) >= -(1n << 63n) && BigInt(value) < (1n << 63n) });
  assert.deepEqual(Object.keys(samples).sort(), ['InvoiceDTO', 'ReceiptDTO']);
  for (const [schema, sample] of Object.entries(samples)) {
    const validate = ajv.compile({ components: contract.components, $ref: `#/components/schemas/${schema}` });
    assert.equal(validate(sample), true, `${schema}: ${JSON.stringify(validate.errors)}`);
    for (const field of contract.components.schemas[schema].required) {
      const invalid = { ...sample }; delete invalid[field];
      assert.equal(validate(invalid), false, `${schema} must detect omitted ${field}`);
    }
    const wrongCurrency = { ...sample, currency: 123 };
    assert.equal(validate(wrongCurrency), false, `${schema} must reject a numeric currency`);
  }
  return { samples: 2, result: 'passed', scope: 'Actual HTTP JSON shape and required-field negative controls; JS representable numbers only' };
}
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  if (process.argv.length !== 3) throw new Error('Usage: node scripts/verify-invoice-receipt-api.mjs PRIVATE_SAMPLES.json');
  console.log(JSON.stringify(validateInvoiceReceiptSamples(JSON.parse(readFileSync(process.argv[2], 'utf8')))));
}
