import { expect, test } from '@playwright/test';
import axe from 'axe-core';

// Browser rendering/keyboard/accessibility only. Responses are synthetic and
// MUST NOT be reported as provider payment or purchase-to-check-in evidence.
test('Guest issued QR downloads on a narrow screen without installing the app @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const code = 'TDF-1234567812344123A123123456789ABC';
  await page.setViewportSize({ width: 320, height: 900 });
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
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/public/events/41/tickets') return route.fulfill({ json: {
      eventId: 41, title: 'Synthetic ticket browser test', startsAt: '2030-10-24T19:00:00Z',
      timezone: 'America/Guayaquil', venueName: 'Synthetic venue', checkoutAvailable: true,
      policy: { policyVersion: 'test', currency: 'USD', buyerFeeBps: 0, organizerFeeBps: 0,
        taxBps: 0, holdMinutes: 10, termsVersion: 'test', termsSummary: 'Synthetic policy',
        refundPolicy: 'Synthetic policy', transferAllowed: false },
      tiers: [{ tierId: 8, code: 'GA', name: 'General', unitPriceMinor: 2000, currency: 'USD', remaining: 1 }],
    } });
    if (path === '/public/events/41/ticket-orders/92') {
      expect(request.headers()['x-order-lookup-token']).toBe('synthetic-order-capability');
      return route.fulfill({ json: { eventId: 41, orderId: 92, checkoutId: 'synthetic',
        paymentStatus: 'paid', fulfillmentStatus: 'issued', holdExpiresAt: '2030-10-24T18:00:00Z',
        quote: { currency: 'USD', quantity: 1, unitPriceMinor: 2000, grossFaceValueMinor: 2000,
          discountMinor: 0, netFaceValueMinor: 2000, buyerPlatformFeeMinor: 0,
          organizerPlatformFeeMinor: 0, taxMinor: 0, checkoutTotalMinor: 2000,
          organizerPayableMinor: 2000, platformFeeMinor: 0, termsVersion: 'test', policyVersion: 'test' },
        paymentMethods: [], tickets: [{ ticketId: 501, ticketCode: code, status: 'issued', holderName: 'Synthetic attendee' }],
      } });
    }
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/eventos/41/orden/92');
  await expect(page.getByRole('img', { name: 'Código QR privado de acceso' })).toBeVisible();
  const save = page.getByRole('button', { name: 'Guardar QR para el acceso' });
  await save.focus();
  const download = page.waitForEvent('download');
  await page.keyboard.press('Enter');
  expect((await download).suggestedFilename()).toBe('tdf-ticket.png');
  expect(new URL(page.url()).search).not.toContain('synthetic-order-capability');
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => (await axe.run(document, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(({ id, nodes }) => ({ id, targets: nodes.map((node) => node.target) })));
  expect(violations).toEqual([]);
  await page.evaluate(() => { document.documentElement.style.fontSize = '200%'; });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
