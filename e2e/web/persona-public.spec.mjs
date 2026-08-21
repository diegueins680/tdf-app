import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const fictionalCityId = '22222222-2222-4222-8222-222222222222';
const directoryResponse = {
  items: [{
    id: '11111111-1111-4111-8111-111111111111',
    type: 'profile',
    slug: 'synthetic-bassist',
    title: 'Bajista Sintética de Quito',
    summary: 'Perfil público inequívocamente ficticio para pruebas aisladas.',
    imageUrl: null,
    location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    sponsored: false,
    score: 0.8,
  }],
  sponsoredItems: [],
  facets: { entityTypes: { profile: 1 }, cities: [], total: 1 },
  nextCursor: null,
};
const taxonomies = {
  locale: 'es', professions: [], classifiedCategories: [], compensationTypes: [],
  serviceOfferings: [], currencies: [], instruments: [], genres: [],
  cities: [{ id: fictionalCityId, code: 'quito-ec-p', name: 'Quito', countryId: '33333333-3333-4333-8333-333333333333' }],
};
const publicTicketStorefront = {
  eventId: 41,
  title: 'Festival Sintético TDF',
  description: 'Evento inequívocamente ficticio para pruebas aisladas.',
  startsAt: '2030-08-20T22:00:00Z',
  endsAt: '2030-08-21T02:00:00Z',
  timezone: 'America/Guayaquil',
  venueName: 'Sala Ficticia Quito',
  venueAddress: null,
  checkoutAvailable: true,
  unavailableReason: null,
  tiers: [{
    tierId: 8, code: 'GENERAL', name: 'General', description: null,
    unitPriceMinor: 2500, currency: 'USD', remaining: 25,
    salesStart: null, salesEnd: null, transfersAllowed: true,
  }],
};
const publicTicketCheckout = {
  orderId: 91,
  eventId: 41,
  checkoutId: '44444444-4444-4444-8444-444444444444',
  lookupToken: 'fictional-order-capability',
  paymentStatus: 'unpaid',
  fulfillmentStatus: 'seat_held',
  holdExpiresAt: '2030-08-20T21:00:00Z',
  quote: {
    policyVersion: 'fictional-policy-v1', currency: 'USD', quantity: 1,
    unitPriceMinor: 2500, grossFaceValueMinor: 2500, discountMinor: 0,
    netFaceValueMinor: 2500, buyerPlatformFeeMinor: 50,
    organizerPlatformFeeMinor: 50, taxMinor: 0, checkoutTotalMinor: 2550,
    organizerPayableMinor: 2450, platformFeeMinor: 100,
    termsVersion: 'fictional-ticket-terms-v1',
  },
  paymentMethods: [],
  tickets: [],
};

async function mockIsolatedPublicApi(page) {
  await page.route('**/health', (route) => route.fulfill({ json: { status: 'ok' } }));
  await page.route('**/session', (route) => route.fulfill({ status: 401, contentType: 'application/json', body: '{"error":"unauthenticated"}' }));
  await page.route('**/fans/artists', (route) => route.fulfill({ json: [] }));
  await page.route('**/directory/taxonomies?*', (route) => route.fulfill({ json: taxonomies }));
  await page.route('**/directory/search?*', (route) => route.fulfill({ json: directoryResponse }));
  await page.route('**/directory/suggestions?*', (route) => route.fulfill({ json: [] }));
  await page.route('**/directory/profiles/synthetic-bassist/reviews?*', (route) => route.fulfill({
    json: { items: [], nextCursor: null, summary: { reviewAverage: null, reviewCount: 0 } },
  }));
  await page.route('**/directory/profiles/synthetic-bassist', (route) => route.fulfill({
    json: {
      id: '11111111-1111-4111-8111-111111111111', slug: 'synthetic-bassist',
      name: 'Bajista Sintética de Quito', bio: 'Perfil inequívocamente ficticio.',
      kind: 'person', status: 'published', portfolio: [], professions: [], instruments: [], genres: [],
      location: { city: 'Quito', countryCode: 'EC', precision: 'city' },
    },
  }));
  // A malformed 200 response reproduces a common proxy/provider failure and verifies that
  // catalog-dependent shell preferences fall back without blanking the application.
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: {} }));
  await page.route('https://api.frankfurter.dev/**', (route) => route.fulfill({ json: { base: 'USD', rates: { USD: 1 } } }));
}

function observeRuntime(page) {
  const consoleErrors = [];
  const failedRequests = [];
  page.on('console', (message) => {
    const expectedAuthRejection = message.type() === 'error'
      && /401 \(Unauthorized\)/.test(message.text())
      && ['/session', '/login'].some((suffix) => message.location().url.endsWith(suffix));
    if (message.type() === 'error' && !expectedAuthRejection) {
      consoleErrors.push(message.text().replace(/(Bearer|token|secret|password)\s+\S+/gi, '$1 [redacted]'));
    }
  });
  page.on('requestfailed', (request) => {
    const reason = request.failure()?.errorText ?? 'unknown';
    // Browser navigation cancels in-flight optional rate/catalog reads. This is not a network
    // failure presented to the user and must not obscure genuine request failures.
    if (reason === 'net::ERR_ABORTED' || reason === 'cancelled') return;
    failedRequests.push(`${request.method()} ${new URL(request.url()).pathname}: ${reason}`);
  });
  return { consoleErrors, failedRequests };
}

async function expectNoSeriousAxeViolations(page, testInfo) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.evaluate(async () => {
    const result = await globalThis.axe.run(document, {
      resultTypes: ['violations'],
      rules: { 'color-contrast': { enabled: true } },
    });
    return result.violations
      .filter((violation) => violation.impact === 'critical' || violation.impact === 'serious')
      .map((violation) => ({ id: violation.id, impact: violation.impact, help: violation.help, nodes: violation.nodes.map((node) => node.target) }));
  });
  await testInfo.attach('axe-serious-critical.json', { body: JSON.stringify(violations, null, 2), contentType: 'application/json' });
  expect(violations).toEqual([]);
}

test.beforeEach(async ({ page }) => {
  await mockIsolatedPublicApi(page);
});

test('@critical PW-PER-01-AUTH redirects protected URLs and explains rejected login', async ({ page }, testInfo) => {
  const runtime = observeRuntime(page);
  let loginPayload;
  await page.route('**/login', async (route) => {
    loginPayload = route.request().postDataJSON();
    await route.fulfill({ status: 401, contentType: 'application/json', json: { error: 'Credenciales inválidas. Revisa los datos e intenta otra vez.' } });
  });

  await page.goto('/crm/contactos');
  await expect(page).toHaveURL(/\/login\?redirect=%2Fcrm%2Fcontactos/);
  await expect(page.locator('main#main-content')).toHaveCount(1);
  await expect(page.getByRole('heading', { name: 'Iniciar sesión' })).toBeVisible();
  await page.getByLabel('Usuario o correo *').fill('per-01.elena@persona.test');
  await page.getByLabel('Contraseña *').fill('fictional-password-not-a-secret');
  await page.getByRole('button', { name: 'Ingresar' }).click();
  await expect(page.getByRole('alert')).toContainText('Credenciales inválidas');
  expect(loginPayload).toMatchObject({ username: 'per-01.elena@persona.test' });
  await expectNoSeriousAxeViolations(page, testInfo);
  expect(runtime.failedRequests).toEqual([]);
  expect(runtime.consoleErrors).toEqual([]);
});

test('PW-PER-01-AUTH registers a fictional user through the UI', async ({ page }) => {
  let signupPayload;
  await page.route('**/signup', async (route) => {
    signupPayload = route.request().postDataJSON();
    await route.fulfill({
      json: { token: 'fictional-browser-token', partyId: 101, roles: ['Fan', 'Customer'], modules: [] },
    });
  });

  await page.goto('/login');
  await page.getByRole('button', { name: 'Crear cuenta general' }).click();
  await expect(page.getByRole('dialog', { name: /crear cuenta/i })).toBeVisible();
  await page.getByLabel('Nombre').fill('Elena');
  await page.getByLabel('Apellido').fill('Paredes');
  await page.getByRole('textbox', { name: 'Correo *', exact: true }).fill('per-01.elena@persona.test');
  await page.getByLabel('Contraseña *').last().fill('fictional-password-not-a-secret');
  await page.getByRole('button', { name: 'Crear e ingresar' }).click();

  await expect(page).toHaveURL(/\/fans$/);
  expect(signupPayload).toMatchObject({
    firstName: 'Elena', lastName: 'Paredes', email: 'per-01.elena@persona.test',
    password: 'fictional-password-not-a-secret',
  });
});

test('PW-PER-01-DISCOVERY completes public city discovery and preserves search state', async ({ page }, testInfo) => {
  const runtime = observeRuntime(page);
  await page.goto('/buscar');
  await expect(page.locator('main#main-content')).toHaveCount(1);
  await expect(page.getByRole('heading', { level: 1 })).toContainText('Encuentra a la gente');
  await expect(page.getByText('Bajista Sintética de Quito')).toBeVisible();
  await page.getByLabel('¿Qué necesitas?').fill('bajista');
  await page.getByRole('button', { name: 'Buscar' }).click();
  await expect(page).toHaveURL(new RegExp(`q=bajista.*cityId=${fictionalCityId}`));
  await page.getByRole('link', { name: 'Ver detalle' }).click();
  await expect(page).toHaveURL(/\/directorio\/synthetic-bassist/);
  await page.goBack();
  await expect(page.getByLabel('¿Qué necesitas?')).toHaveValue('bajista');
  await expectNoSeriousAxeViolations(page, testInfo);
  expect(runtime.failedRequests).toEqual([]);
  expect(runtime.consoleErrors).toEqual([]);
});

test('PW-PER-01-DISCOVERY reflows at an effective 320 CSS pixel viewport', async ({ page }, testInfo) => {
  test.skip(testInfo.project.name !== 'chromium-phone', 'Phone-specific zoom inspection.');
  // WCAG 1.4.10's 320 CSS pixel target represents 400% browser zoom on a
  // 1280-pixel desktop viewport while still exercising responsive breakpoints.
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto('/buscar');
  await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
  const overflow = await page.evaluate(() => ({ width: document.documentElement.clientWidth, scrollWidth: document.documentElement.scrollWidth }));
  expect(overflow.scrollWidth).toBeLessThanOrEqual(overflow.width + 1);
  await expectNoSeriousAxeViolations(page, testInfo);
});

test('PW-PER-01-TICKET-OFFER distinguishes a guest hold from payment and issuance', async ({ page }, testInfo) => {
  let checkoutRequest;
  let idempotencyKey;
  await page.route('**/public/events/41/tickets', (route) => route.fulfill({ json: publicTicketStorefront }));
  await page.route('**/public/events/41/ticket-orders', async (route) => {
    checkoutRequest = route.request().postDataJSON();
    idempotencyKey = route.request().headers()['idempotency-key'];
    await route.fulfill({ json: publicTicketCheckout });
  });
  await page.route('**/public/events/41/ticket-orders/91', (route) => route.fulfill({ json: publicTicketCheckout }));

  await page.goto('/eventos/41/entradas');
  await expect(page.locator('main#main-content')).toHaveCount(1);
  await expect(page.getByRole('heading', { level: 1 })).toHaveText('Festival Sintético TDF');
  await expect(page.getByText(/General.*25[,.]00/)).toBeVisible();
  await page.getByLabel('Nombre completo').fill('Elena Paredes');
  await page.getByLabel('Email').fill('per-01.elena@persona.test');
  await page.getByLabel(/Acepto los términos versionados/).check();
  await page.getByRole('button', { name: 'Retener entradas y revisar total' }).click();

  await expect(page).toHaveURL(/\/eventos\/41\/orden\/91$/);
  await expect(page.getByText(/La orden no está pagada/)).toBeVisible();
  await expect(page.getByText(/No hay un proveedor real habilitado/)).toBeVisible();
  await expect(page.getByText(/Total:.*25[,.]50/)).toBeVisible();
  expect(checkoutRequest).toMatchObject({
    tierId: 8, quantity: 1, buyerName: 'Elena Paredes',
    buyerEmail: 'per-01.elena@persona.test', termsAccepted: true,
  });
  expect(idempotencyKey).toMatch(/^event-ticket-checkout-/);
  await expectNoSeriousAxeViolations(page, testInfo);
});
