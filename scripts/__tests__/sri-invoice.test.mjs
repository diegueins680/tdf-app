import test from 'node:test';
import assert from 'node:assert/strict';

import {
  computeInvoiceTotal,
  extractIssuedInvoiceData,
  mapTaxBpsToSriIvaCode,
  normalizeSriLine,
} from '../lib/sri-invoice.mjs';

test('mapTaxBpsToSriIvaCode maps common Ecuadorian IVA values', () => {
  assert.equal(mapTaxBpsToSriIvaCode(0), '0');
  assert.equal(mapTaxBpsToSriIvaCode(500), '5');
  assert.equal(mapTaxBpsToSriIvaCode(1500), '4');
  assert.throws(() => mapTaxBpsToSriIvaCode(1200), /Unsupported SRI tax rate/);
});

test('normalizeSriLine derives stable SRI product codes', () => {
  const line = normalizeSriLine({
    description: 'Show artístico',
    quantity: 1,
    unitCents: 7500,
    taxBps: 0,
  });

  assert.equal(line.code, 'SHOWARTISTICO7500');
  assert.equal(line.auxiliaryCode, 'SHOWARTISTICO7500');
  assert.equal(line.description, 'SHOW ARTÍSTICO');
  assert.equal(line.sriIvaCode, '0');
  assert.equal(line.unitPrice, 75);
});

test('normalizeSriLine accepts sriAdditionalInfo from backend payloads', () => {
  const line = normalizeSriLine({
    description: 'Show artístico',
    quantity: 1,
    unitCents: 7500,
    taxBps: 0,
    sriAdditionalInfo: 'Show artístico marzo 2026',
  });

  assert.equal(line.additionalInfo, 'SHOW ARTÍSTICO MARZO 2026');
});

test('computeInvoiceTotal honors quantity and cents conversion', () => {
  const total = computeInvoiceTotal([
    { unitCents: 7500, quantity: 1 },
    { unitCents: 2500, quantity: 2 },
  ]);
  assert.equal(total, 125);
});

test('extractIssuedInvoiceData parses the SRI success banner', () => {
  const parsed = extractIssuedInvoiceData(
    'Nueva factura Número de autorización: 3003202601179321509200120011000000000163200767814 Número de comprobante: 001-100-000000016 Correo enviado a: facturas@example.com Ver RIDE',
  );

  assert.equal(parsed.authorizationNumber, '3003202601179321509200120011000000000163200767814');
  assert.equal(parsed.invoiceNumber, '001-100-000000016');
  assert.equal(parsed.buyerEmail, 'facturas@example.com');
});
