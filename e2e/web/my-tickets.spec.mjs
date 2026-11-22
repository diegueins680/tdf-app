import { expect, test } from '@playwright/test';
import { localApiFixturePattern } from './helpers/local-api-fixture.mjs';

test.use({ serviceWorkers: 'block' });

// Synthetic UI coverage only; no purchase, provider or ticket issuance claim.
async function fixture(page, baseURL, account = false) {
  const origin = new URL(baseURL).origin;
  const session = { username: 'synthetic-ticket-buyer', displayName: 'Synthetic buyer',
    partyId: 77, roles: ['customer'], modules: [], featureFlags: [] };
  const reads = [];
  await page.addInitScript(({ account, session }) => {
    localStorage.setItem('tdf-hq-ui/locale', 'es');
    if (account) localStorage.setItem('tdf-hq-ui/session', JSON.stringify(session));
    else for (const id of [92, 93, 94, 95]) {
      localStorage.setItem(`tdf:event-ticket-checkout:41:${id}`, `synthetic-key-${id}`);
    }
  }, { account, session });
  await page.route(localApiFixturePattern(baseURL), async route => {
    const request = route.request(), url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    const path = url.pathname;
    if (path === '/session') return route.fulfill({ json: account ? session : null });
    if (path === '/session/preferences') return route.fulfill({ json: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/public/events/41/tickets') return route.fulfill({ json: {
      eventId: 41, title: 'Entradas de prueba', startsAt: '2099-08-20T22:00:00Z',
      timezone: 'America/Guayaquil', venueName: 'Sala de prueba', tiers: [],
    } });
    if (path === '/social-events/ticket-orders') return route.fulfill({ json: [{
      ticketOrderId: '150', ticketOrderEventId: '41', ticketOrderQuantity: 2,
      ticketOrderAmountCents: 2400, ticketOrderCurrency: 'USD', ticketOrderStatusValue: 'partially_refunded',
      ticketOrderTickets: [
        { ticketCode: 'SYNTHETIC-VALID', ticketStatus: 'issued' },
        { ticketCode: 'SYNTHETIC-REFUNDED', ticketStatus: 'refunded' },
      ],
    }] });
    const match = /^\/public\/events\/41\/ticket-orders\/(92|93|94|95)$/.exec(path);
    if (match) {
      const id = Number(match[1]);
      expect(request.method()).toBe('GET');
      expect(request.headers()['x-order-lookup-token']).toBe(`synthetic-key-${id}`);
      expect(url.search).toBe('');
      reads.push(id);
      if (id === 94) return route.fulfill({ status: 404, json: {} });
      return route.fulfill({ json: { eventId: 41, orderId: id === 95 ? 999 : id,
        paymentStatus: id === 92 ? 'processing' : 'unrecognized-future-status',
        fulfillmentStatus: 'seat_held', holdExpiresAt: '2099-08-20T22:00:00Z',
        quote: { quantity: 1, checkoutTotalMinor: 1200, currency: 'USD' },
        paymentMethods: [], tickets: [],
      } });
    }
    return route.fulfill({ status: 404, json: {} });
  });
  return reads;
}

test('guest tickets preserve private keys and distinguish processing, unknown and failed reads @mobile-flow @critical', async ({ page, baseURL }, info) => {
  const reads = await fixture(page, baseURL);
  await page.goto('/mis-entradas');
  await expect(page.getByRole('heading', { name: 'Mis entradas', exact: true })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Ver estado de pago', exact: true })).toHaveAttribute('href', '/eventos/41/orden/92');
  await expect(page.getByText('Estado por confirmar', { exact: true })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Ver la orden', exact: true })).toHaveAttribute('href', '/eventos/41/orden/93');
  await expect(page.getByText(/No pudimos leer 2 compras guardadas/)).toBeVisible();
  await expect(page.getByRole('button', { name: 'Completar pago', exact: true })).toHaveCount(0);
  await expect(page.getByText('Sin entradas válidas', { exact: true })).toHaveCount(0);
  await expect(page.getByText('Todavía no tienes entradas aquí.', { exact: true })).toHaveCount(0);
  expect(reads.sort()).toEqual([92, 93, 94, 95]);
  expect(await page.content()).not.toContain('synthetic-key-');
  expect(page.url()).not.toContain('synthetic-key-');
  await page.getByRole('link', { name: 'Ver estado de pago', exact: true }).focus();
  await expect(page.getByRole('link', { name: 'Ver estado de pago', exact: true })).toBeFocused();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  await page.screenshot({ path: info.outputPath('guest-my-tickets.png'), fullPage: true });
});

test('account tickets show only issued codes after a partial refund @mobile-flow @critical', async ({ page, baseURL }, info) => {
  await fixture(page, baseURL, true);
  await page.goto('/mis-entradas');
  await expect(page.getByText('Códigos vigentes: SYNTHETIC-VALID', { exact: true })).toBeVisible();
  await expect(page.getByText(/SYNTHETIC-REFUNDED/)).toHaveCount(0);
  await expect(page.getByRole('link', { name: 'Ver el evento', exact: true })).toHaveAttribute('href', '/eventos/41');
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  await page.screenshot({ path: info.outputPath('account-my-tickets.png'), fullPage: true });
});
