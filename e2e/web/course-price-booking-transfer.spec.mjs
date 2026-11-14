import { expect, test } from '@playwright/test';
import axe from 'axe-core';

async function inspectField(page, input) {
  await expect(input).toHaveAttribute('aria-invalid', 'true');
  const helper = await input.getAttribute('aria-describedby');
  expect(helper).toBeTruthy();
  await expect(page.locator(`[id="${helper}"]`)).not.toBeEmpty();
  await page.addScriptTag({ content: axe.source });
  expect(await input.evaluate(async node => (await globalThis.axe.run(node.closest('.MuiFormControl-root'), {
    runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa', 'wcag22aa'] },
  })).violations.map(v => ({ id: v.id, impact: v.impact })))).toEqual([]);
}

async function showField(input) {
  await expect(async () => {
    await input.evaluate(node => node.scrollIntoView({ block: 'center', behavior: 'instant' }));
    await expect(input).toBeInViewport({ ratio: 1, timeout: 500 });
    expect(await input.evaluate(node => {
      const rect = node.getBoundingClientRect();
      return node.contains(document.elementFromPoint(rect.x + rect.width / 2, rect.y + rect.height / 2));
    })).toBe(true);
  }).toPass({ timeout: 8000 });
}

test('course prices retain exact cents with decimal entry and invalid drafts survive reload @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin;
  const metadata = { slug: 'sintetico-nov-2030', title: 'Curso sintético', subtitle: 'Fixture', format: 'Presencial', duration: 'Una sesión', price: 25.5,
    currency: 'USD', capacity: 16, sessions: [{ label: 'Sesión sintética', date: '2030-11-20' }], syllabus: [{ title: 'Tema sintético', topics: ['Tema'] }],
    daws: [], includes: [], instructorName: 'Instructor sintético', instructorBio: '', instructorAvatarUrl: '', sessionStartHour: 15, sessionDurationHours: 2 };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() === 'POST' && (['/session/onboarding/reconcile', '/radio/presence'].includes(path) || /^\/navigation\/preferences\/[^/]+\/visit$/.test(path))) return route.fulfill({ json: {} });
    if (request.method() === 'DELETE' && path === '/radio/presence') return route.fulfill({ status: 204 });
    if (request.method() !== 'GET') {
      const body = request.postDataJSON(); writes.push({ path, method: request.method(), body });
      expect(path).toBe('/admin/courses'); expect(request.method()).toBe('POST');
      expect(body).not.toHaveProperty('priceInput'); expect(body).not.toHaveProperty('priceInputError');
      expect(body.priceCents).toBe(4099); metadata.price = 40.99;
      return route.fulfill({ json: metadata });
    }
    const preferences = { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' };
    if (path === '/session') return route.fulfill({ json: { username: 'synthetic-admin', displayName: 'Admin sintético', partyId: 42, roles: ['Admin'], modules: ['admin'], featureFlags: [], preferences } });
    if (path === '/session/preferences') return route.fulfill({ json: preferences });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path.startsWith('/public/courses/')) return route.fulfill({ json: metadata });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/configuracion/cursos');
  const price = page.getByRole('textbox', { name: 'Precio (USD)', exact: true });
  await expect(price).toHaveValue('25.50');
  await price.fill('25,505'); await inspectField(page, price);
  const publish = page.getByRole('button', { name: 'Publicar curso', exact: true });
  await expect(publish).toBeDisabled(); expect(writes).toEqual([]);
  await expect.poll(() => page.evaluate(() => JSON.parse(localStorage.getItem('tdf-course-builder-draft') ?? '{}').priceInput)).toBe('25,505');
  expect(await page.evaluate(() => JSON.parse(localStorage.getItem('tdf-course-builder-draft')).priceCents)).toBeUndefined();
  await page.reload(); await expect(price).toHaveValue('25,505'); await expect(publish).toBeDisabled();
  await price.fill('40,99'); await expect(price).toHaveAttribute('aria-invalid', 'false');
  await publish.click(); await expect.poll(() => writes.length).toBe(1);
  await expect.poll(() => page.evaluate(() => localStorage.getItem('tdf-course-builder-draft'))).toBeNull();
  await page.reload(); await expect(price).toHaveValue('40.99');
  await showField(price); await page.screenshot({ path: info.outputPath('course-price-decimals.png') });
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  expect(writes).toHaveLength(1);
});

test('booking transfer errors identify the reference and preserve rejected evidence for retry @critical @mobile-flow', async ({ page, baseURL }, info) => {
  const writes = [], origin = new URL(baseURL).origin; let evidenceAttempts = 0;
  const serviceId = '22222222-2222-4222-8222-222222222222';
  const quote = { policyVersion: 'synthetic-policy', currency: 'USD', durationMinutes: 60, subtotalMinor: 5000, taxMinor: 0,
    totalMinor: 5000, depositMinor: 2500, balanceMinor: 2500, depositBps: 5000, termsVersion: 'synthetic-terms' };
  const checkout = { booking: { bookingId: 456, title: 'Sesión sintética', startsAt: '2030-11-20T17:00:00Z', endsAt: '2030-11-20T18:00:00Z', status: 'Tentative', serviceOfferingId: serviceId, serviceType: 'Práctica sintética', resources: [] },
    checkoutId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', lookupToken: 'synthetic-booking-capability', paymentStatus: 'awaiting_payment', fulfillmentStatus: 'on_hold',
    holdExpiresAt: '2030-11-20T16:15:00Z', quote, paymentMethods: ['bank_transfer'], manualPayment: null };
  await page.route('**/*', async route => {
    const request = route.request(), url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) return url.origin === origin ? route.continue() : route.abort();
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (request.method() !== 'GET') {
      const body = request.postDataJSON(); writes.push({ path, method: request.method(), body });
      if (path === '/bookings/public/checkout') { expect(body.pbcTermsAccepted).toBe(true); return route.fulfill({ json: checkout }); }
      expect(request.headers()['x-order-lookup-token']).toBe('synthetic-booking-capability');
      if (path === '/bookings/public/orders/456/manual-payment') return route.fulfill({ json: { ...checkout, manualPayment: { paymentMethod: 'bank_transfer', status: 'awaiting_evidence' } } });
      expect(path).toBe('/bookings/public/orders/456/manual-payment/evidence'); expect(body.customerReference).toBe('REF-SYNTHETIC-77');
      evidenceAttempts += 1;
      if (evidenceAttempts === 1) return route.fulfill({ status: 503, json: { error: 'Synthetic evidence rejection; no payment confirmed' } });
      return route.fulfill({ json: { ...checkout, manualPayment: { paymentMethod: 'bank_transfer', status: 'submitted' } } });
    }
    if (path === '/session') return route.fulfill({ status: 401, json: {} });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/services/catalog/public') return route.fulfill({ json: { sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [{
      scId: serviceId, scCode: 'dj-booth-practice', scName: 'Práctica sintética', scNameEs: 'Práctica sintética', scNameEn: 'Synthetic session',
      scKind: 'dj-practice', scPricingModel: 'hourly', scRateCents: 5000, scCurrency: 'USD', scBillingUnit: 'hour', scDefaultDurationMinutes: 60, scRequiresEngineer: false, scDefaultResources: [], scActive: true, scSortOrder: 1,
    }] } });
    if (path === '/bookings/public/availability') return route.fulfill({ json: { available: true, quote } });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
    if (path.startsWith('/reviews/')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
    return route.fulfill({ json: [] });
  });
  await page.goto('/reservar?service=dj-booth-practice');
  await page.getByRole('textbox', { name: 'Nombre completo', exact: true }).fill('Contacto sintético');
  await page.getByRole('textbox', { name: 'Correo', exact: true }).fill('contact@example.test');
  await page.getByRole('button', { name: 'Continuar', exact: true }).click();
  await page.getByLabel(/^Fecha y hora/).fill('2030-11-20T12:00');
  await page.getByLabel('Duración (min)', { exact: true }).fill('60');
  await page.getByRole('checkbox', { name: 'Aceptar precio y política de reserva', exact: true }).check();
  await page.getByRole('button', { name: 'Crear orden y retener horario', exact: true }).click();
  await page.getByRole('button', { name: 'Registrar transferencia', exact: true }).click();
  const dialog = page.getByRole('dialog', { name: 'Registrar transferencia bancaria', exact: true });
  const reference = dialog.getByRole('textbox', { name: 'Referencia o comprobante bancario', exact: true });
  await expect(reference).toBeFocused();
  await reference.fill('aa'); await reference.press('Tab'); await inspectField(page, reference);
  const submit = dialog.getByRole('button', { name: 'Enviar para revisión', exact: true });
  await expect(submit).toBeDisabled(); expect(evidenceAttempts).toBe(0);
  await showField(reference); await page.screenshot({ path: info.outputPath('booking-transfer-reference-error.png') });
  await reference.fill('  REF-SYNTHETIC-77  '); await submit.click();
  await expect(dialog.getByText('No pudimos enviar la referencia. No se confirmó ningún pago.', { exact: true })).toBeVisible();
  await expect(reference).toHaveValue('  REF-SYNTHETIC-77  ');
  await submit.click(); await expect(dialog).toBeHidden();
  expect(evidenceAttempts).toBe(2); expect(writes).toHaveLength(4);
  expect(writes[2].body).toEqual(writes[3].body);
  await expect(page.getByText(/Referencia recibida. Permanece pendiente/)).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
