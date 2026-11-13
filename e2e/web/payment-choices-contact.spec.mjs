import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function fieldAccessibility(page, element) {
  await page.addScriptTag({ content: axe.source });
  const violations = await element.evaluate(async node => (await globalThis.axe.run(node, {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, impact: v.impact })));
  expect(violations).toEqual([]);
}

async function showControl(control) {
  await expect(async () => {
    await control.evaluate(element => element.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(control).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await control.evaluate(element => {
      const rect = element.getBoundingClientRect();
      return element.contains(document.elementFromPoint(rect.x + rect.width / 2, rect.y + rect.height / 2));
    })).toBe(true);
  }).toPass({ timeout: 8000 });
}

for (const language of ['es', 'en']) test(`one payment list preserves exact recovery and local-method locks (${language}) @critical @mobile-flow`, async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin;
  await page.addInitScript(locale => {
    localStorage.setItem('tdf-hq-ui/locale', locale);
    localStorage.setItem('tdf:event-ticket-checkout:41:92', 'synthetic-order-capability');
  }, language);
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() !== 'GET') {
      writes.push({ path, method: request.method(), body: request.postDataJSON(), key: request.headers()['idempotency-key'] });
      expect(path).toBe('/commerce/checkouts/aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa/payment-sessions');
      expect(request.headers()['x-checkout-lookup-token']).toBe('synthetic-order-capability');
      return route.fulfill({ status: 500, json: { error: 'Synthetic ambiguous response; no real charge' } });
    }
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    if (path === '/public/events/41/tickets') return route.fulfill({ json: {
      eventId: 41, title: 'Evento sintético', startsAt: '2030-10-24T19:00:00Z', timezone: 'America/Guayaquil', checkoutAvailable: false, tiers: [],
    } });
    if (path === '/public/events/41/ticket-orders/92') return route.fulfill({ json: {
      eventId: 41, orderId: 92, checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', paymentStatus: 'pending', fulfillmentStatus: 'seat_held', holdExpiresAt: '2030-10-24T18:00:00Z',
      quote: { currency: 'USD', quantity: 1, unitPriceMinor: 2000, grossFaceValueMinor: 2000, discountMinor: 0, netFaceValueMinor: 2000,
        buyerPlatformFeeMinor: 0, organizerPlatformFeeMinor: 0, taxMinor: 0, checkoutTotalMinor: 2000, organizerPayableMinor: 2000,
        platformFeeMinor: 0, termsVersion: 'synthetic', policyVersion: 'synthetic' },
      paymentMethods: ['datafast', 'paypal', 'bank_transfer', 'placetopay_card', 'placetopay_bank_redirect', 'placetopay_deuna_qr', 'payphone_wallet', 'unsupported_synthetic_method'], tickets: [],
    } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/eventos/41/orden/92');
  const english = language === 'en';
  const group = page.getByRole('group', { name: english ? 'Available payment methods' : 'Métodos de pago disponibles', exact: true });
  await expect(group).toHaveCount(1); await expect(group.getByRole('button')).toHaveCount(7);
  const localCard = group.getByRole('button', { name: english ? 'Card · Datafast' : 'Tarjeta · Datafast', exact: true });
  const card = group.getByRole('button', { name: english ? 'Card · PlaceToPay' : 'Tarjeta · PlaceToPay', exact: true });
  const bank = group.getByRole('button', { name: english ? 'Bank transfer' : 'Transferencia bancaria', exact: true });
  await expect(localCard).toBeEnabled(); await expect(bank).toBeEnabled();
  expect(writes).toEqual([]);
  await fieldAccessibility(page, group);
  await showControl(localCard);
  await page.screenshot({ path: info.outputPath(`payment-choice-list-${language}.png`) });
  await group.getByRole('button', { name: english ? 'PayPhone wallet' : 'Billetera PayPhone', exact: true }).click();
  await expect(page.getByRole('textbox', { name: english ? 'PayPhone number' : 'Número PayPhone', exact: true })).toBeFocused();
  expect(writes).toEqual([]);
  await card.click();
  const warning = page.getByText(english ? /We do not know yet whether the payment went through/ : /Todavía no sabemos si el pago se completó/);
  await expect(warning).toBeVisible();
  await expect(localCard).toBeDisabled(); await expect(bank).toBeDisabled();
  await expect(group.getByRole('button', { name: 'PayPal', exact: true })).toBeDisabled();
  expect(writes).toHaveLength(1);
  expect(writes[0].body).toMatchObject({ provider: 'placetopay', paymentMethod: 'card' });
  expect(writes[0].key).toBeTruthy();
  await page.reload();
  await expect(warning).toBeVisible(); await expect(localCard).toBeDisabled(); await expect(bank).toBeDisabled();
  await page.getByRole('button', { name: english ? 'Recover original payment' : 'Recuperar pago original', exact: true }).click();
  await expect.poll(() => writes.length).toBe(2);
  expect(writes[1].key).toBe(writes[0].key); expect(writes[1].body).toEqual(writes[0].body);
  await expect(localCard).toBeDisabled(); await expect(bank).toBeDisabled();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});

test('booking contact starts summarized, remains editable and preserves keyboard focus @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin;
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') { writes.push(path); return route.fulfill({ status: 409, json: {} }); }
    if (path === '/session') return route.fulfill({ json: { username: 'contact@example.test', displayName: 'Contacto sintético', partyId: 42, roles: ['Customer'], modules: [], preferences: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } } });
    if (path === '/session/preferences') return route.fulfill({ json: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/services/catalog/public') return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
      scId: '22222222-2222-4222-8222-222222222222', scCode: 'dj-booth-practice', scName: 'Práctica en DJ Booth', scNameEs: 'Práctica en DJ Booth', scNameEn: 'DJ Booth practice',
      scKind: 'dj-practice', scPricingModel: 'hourly', scRateCents: 5000, scCurrency: 'USD', scBillingUnit: 'hour', scDefaultDurationMinutes: 30, scRequiresEngineer: false, scDefaultResources: [], scActive: true, scSortOrder: 1,
    }] } });
    if (path === '/bookings/public/availability') return route.fulfill({ json: { available: true } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    if (path.startsWith('/reviews/')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/reservar?service=dj-booth-practice');
  const name = page.getByRole('textbox', { name: 'Nombre completo', exact: true });
  await expect(page.getByRole('region', { name: 'Datos de contacto', exact: true })).toContainText('contact@example.test');
  await expect(name).toHaveCount(0);
  const edit = page.getByRole('button', { name: 'Editar datos', exact: true });
  await edit.focus(); await edit.press('Enter');
  await expect(name).toBeFocused();
  const email = page.getByRole('textbox', { name: 'Correo', exact: true });
  await email.fill('edited@example.test');
  await fieldAccessibility(page, page.locator('#booking-contact-fields'));
  await page.getByRole('button', { name: 'Usar estos datos de contacto', exact: true }).click();
  await expect(edit).toBeFocused(); await expect(edit).toHaveAttribute('aria-expanded', 'false');
  await expect(page.getByRole('region', { name: 'Datos de contacto', exact: true })).toContainText('edited@example.test');
  await showControl(edit);
  await page.screenshot({ path: info.outputPath('booking-contact-summary.png') });
  await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  await expect(page.getByLabel(/^Fecha y hora/)).toBeVisible();
  await page.getByRole('button', { name: 'Volver', exact: true }).click();
  await expect(page.getByRole('region', { name: 'Datos de contacto', exact: true })).toContainText('edited@example.test');
  expect(writes).toEqual([]);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
