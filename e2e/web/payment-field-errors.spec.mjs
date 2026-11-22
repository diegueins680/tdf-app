import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Entirely synthetic reads; invalid input must never initiate a payment or
// report a bank transfer. This is rendering/validation evidence, not a charge.
async function fixture(page, baseURL, bank) {
  const origin = new URL(baseURL).origin;
  const writes = [];
  await page.addInitScript(() => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    localStorage.setItem('tdf:event-ticket-checkout:41:92', 'synthetic-order-capability');
  });
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    if (request.method() !== 'GET') {
      writes.push(url.pathname);
      return route.fulfill({ status: 409, json: { error: 'No synthetic write allowed' } });
    }
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    if (path === '/public/events/41/tickets') return route.fulfill({ json: {
      eventId: 41, title: 'Evento sintético', startsAt: '2030-10-24T19:00:00Z',
      timezone: 'America/Guayaquil', checkoutAvailable: false, tiers: [],
    } });
    if (path === '/public/events/41/ticket-orders/92') {
      expect(request.headers()['x-order-lookup-token']).toBe('synthetic-order-capability');
      return route.fulfill({ json: {
        eventId: 41, orderId: 92, checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
        paymentStatus: 'pending', fulfillmentStatus: 'seat_held',
        holdExpiresAt: '2030-10-24T18:00:00Z',
        quote: { currency: 'USD', quantity: 1, unitPriceMinor: 2000,
          grossFaceValueMinor: 2000, discountMinor: 0, netFaceValueMinor: 2000,
          buyerPlatformFeeMinor: 0, organizerPlatformFeeMinor: 0, taxMinor: 0,
          checkoutTotalMinor: 2000, organizerPayableMinor: 2000, platformFeeMinor: 0,
          termsVersion: 'synthetic', policyVersion: 'synthetic' },
        paymentMethods: bank ? [] : ['payphone_wallet'], tickets: [],
        ...(bank ? { bankTransfer: {
          paymentReference: 'SYNTHETIC-92', amountMinor: 2000, currency: 'USD',
          instructions: 'Banco sintético; cuenta de prueba 0000.',
          evidenceStatus: 'awaiting_evidence', receiptAttached: false,
        } } : {}),
      } });
    }
    return route.fulfill({ json: [] });
  });
  await page.goto('/eventos/41/orden/92');
  return writes;
}

async function accessible(page) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await axe.run(document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(({ id, nodes }) => ({ id, targets: nodes.map((node) => node.target) })));
  expect(violations).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
}

test('PayPhone errors identify and focus the fields without starting a payment @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const writes = await fixture(page, baseURL, false);
  const wallet = page.getByRole('button', { name: 'Billetera PayPhone', exact: true });
  await wallet.click();
  const phone = page.getByRole('textbox', { name: 'Número PayPhone', exact: true });
  const country = page.getByRole('textbox', { name: 'Código país', exact: true });
  await expect(phone).toBeFocused();
  await country.fill('');
  await phone.fill('123');
  await wallet.click();
  await expect(country).toBeFocused();
  await expect(country).toHaveAttribute('aria-invalid', 'true');
  await expect(phone).toHaveAttribute('aria-invalid', 'true');
  await country.fill('593');
  await wallet.click();
  await expect(phone).toBeFocused();
  await expect(phone).toHaveValue('123');
  await accessible(page);
  expect(writes).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('payphone-field-errors.png'), fullPage: true });
});

test('Transfer errors remain associated with their fields and preserve the reference @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
  const writes = await fixture(page, baseURL, true);
  const reference = page.getByRole('textbox', { name: 'Número de comprobante de la transferencia' });
  await reference.fill('aa');
  await reference.press('Tab');
  await expect(reference).toHaveAttribute('aria-invalid', 'true');
  await reference.fill('COMP-SYNTHETIC-77');
  const receipt = page.getByLabel('Adjuntar comprobante (opcional)');
  await receipt.setInputFiles({ name: 'invalid.svg', mimeType: 'image/svg+xml', buffer: Buffer.from('<svg/>') });
  await expect(receipt).toHaveAttribute('aria-invalid', 'true');
  await expect(receipt).toHaveAttribute('aria-describedby', /ticket-transfer-receipt-error/);
  await expect(page.getByRole('button', { name: 'Ya transferí' })).toBeDisabled();
  await expect(reference).toHaveValue('COMP-SYNTHETIC-77');
  await accessible(page);
  expect(writes).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('transfer-field-errors.png'), fullPage: true });
});
