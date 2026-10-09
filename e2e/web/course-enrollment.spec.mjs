import { expect, test } from '@playwright/test';

// Course enrollment as reported from a real 360px Android (Chrome) user:
// "Inscribirme" must open the form immediately, and an Ecuador local mobile
// (0988384849) must be accepted. Every API request is answered with synthetic
// fixtures; foreign origins are aborted, so nothing reaches a real service.
const VIEWPORT = { width: 360, height: 740 };
const SLUG = 'bateria-guillermo-diaz-abr-2026';

const courseMetadata = {
  slug: SLUG,
  title: 'Curso de Batería con Guillermo Díaz',
  subtitle: 'Programa presencial de 8 niveles.',
  format: 'Presencial',
  duration: '8 sesiones sabatinas (16 horas en total)',
  price: 240,
  currency: 'USD',
  capacity: 12,
  remaining: 12,
  locationLabel: 'TDF Records - Quito',
  locationMapUrl: 'https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6',
  whatsappCtaUrl: 'https://wa.me/?text=INSCRIBIRME',
  landingUrl: `https://tdf-app.pages.dev/curso/${SLUG}`,
  daws: ['Batería acústica', 'Groove'],
  includes: ['8 sesiones presenciales'],
  sessions: [{ label: 'Nivel 1', date: '2026-11-07' }],
  syllabus: [
    { title: 'Nivel 1 - Fundamentos', topics: ['Postura', 'Pulso estable'] },
    { title: 'Nivel 8 - Proyecto final', topics: ['Ensamble'] },
  ],
  sessionStartHour: 10,
  sessionDurationHours: 2,
  instructorName: 'Guillermo Díaz',
  instructorBio: 'Baterista e instructor.',
  instructorAvatarUrl: null,
};

const leadReceived = {
  registrationId: 77,
  courseSlug: SLUG,
  checkoutId: null,
  lookupToken: null,
  paymentStatus: 'not_started',
  fulfillmentStatus: 'lead_received',
  holdExpiresAt: null,
  quote: null,
  paymentMethods: [],
  checkoutAvailable: false,
};

const signedInAccount = {
  username: 'llamaestepez@gmail.com', displayName: 'Llamaestepez', partyId: 77,
  roles: ['customer'], modules: [], featureFlags: [],
  preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
};

async function fixture(page, baseURL, { registrationFailure = null, authenticated = false } = {}) {
  const origin = new URL(baseURL).origin;
  const state = { registrations: [], idempotencyKeys: [] };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') {
      return authenticated
        ? route.fulfill({ json: signedInAccount })
        : route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    }
    if (path.startsWith('/radio/presence')) return route.fulfill({ json: null });
    if (path.startsWith('/radio/')) return route.fulfill({ json: [] });
    if (path === `/public/courses/${SLUG}`) return route.fulfill({ json: courseMetadata });
    if (path === `/public/courses/${SLUG}/registrations` && request.method() === 'POST') {
      state.registrations.push(request.postDataJSON());
      state.idempotencyKeys.push(request.headers()['idempotency-key'] ?? null);
      if (registrationFailure) return route.fulfill(registrationFailure);
      return route.fulfill({ json: leadReceived });
    }
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  return state;
}

const fitsViewport = (page) => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth);

async function expectRenderedRoot(page) {
  await expect.poll(() => page.evaluate(() => (document.querySelector('#root')?.innerText ?? '').trim().length))
    .toBeGreaterThan(40);
}

async function expectInViewport(page, locator) {
  await expect(locator).toBeVisible();
  await expect.poll(async () => {
    const box = await locator.boundingBox();
    const viewport = page.viewportSize();
    if (!box || !viewport) return false;
    return box.y >= 0 && box.x >= 0
      && box.y + box.height <= viewport.height
      && box.x + box.width <= viewport.width;
  }).toBe(true);
}

async function openEnrollment(page) {
  await page.goto(`/curso/${SLUG}`, { waitUntil: 'domcontentloaded' });
  await expectRenderedRoot(page);
  await expect(page.getByRole('heading', { name: 'Curso de Batería con Guillermo Díaz' })).toBeVisible();
  await page.getByRole('button', { name: 'Inscribirme', exact: true }).first().click();
  const dialog = page.getByRole('dialog', { name: 'Inscríbete en Curso de Batería con Guillermo Díaz' });
  await expect(dialog).toBeVisible();
  return dialog;
}

async function fillEnrollment(dialog, { phone }) {
  await dialog.getByLabel('Nombre completo').fill('Juan');
  await dialog.getByLabel('Correo').fill('juan.synthetic@gmail.com');
  await dialog.getByLabel('WhatsApp (opcional)').fill(phone);
  await dialog.getByLabel('¿Cómo te enteraste del curso? (opcional)').fill('Instagram');
  await dialog.getByRole('checkbox', { name: /Acepto los términos y la política de cancelación/ }).check();
}

test.describe('Course enrollment on a small Android phone', () => {
  test.beforeEach(async ({ page }) => {
    await page.setViewportSize(VIEWPORT);
  });

  test('Inscribirme opens the form in view and accepts an Ecuador local mobile @mobile-flow', async ({ page, baseURL }) => {
    const state = await fixture(page, baseURL);
    const dialog = await openEnrollment(page);

    // The first field is on screen and focused immediately — no scrolling to find the form.
    const nameField = dialog.getByLabel('Nombre completo');
    await expectInViewport(page, nameField);
    await expect(nameField).toBeFocused();
    expect(await fitsViewport(page)).toBe(true);

    await fillEnrollment(dialog, { phone: '0988384849' });
    await expect(dialog.getByText('Lo enviaremos como +593988384849.')).toBeVisible();
    await dialog.getByRole('button', { name: 'Enviar inscripción' }).click();

    await expect(page.getByRole('dialog', { name: 'Solicitud recibida' })).toBeVisible();
    await expect(page.getByText('Te escribiremos a juan.synthetic@gmail.com y por WhatsApp')).toBeVisible();
    expect(state.registrations).toHaveLength(1);
    expect(state.registrations[0]).toMatchObject({
      fullName: 'Juan',
      email: 'juan.synthetic@gmail.com',
      phoneE164: '+593988384849',
      source: 'landing',
      howHeard: 'Instagram',
      termsAccepted: true,
    });
    expect(state.idempotencyKeys[0]).toMatch(/^course-checkout-/);
    expect(await fitsViewport(page)).toBe(true);
    await expectRenderedRoot(page);
  });

  test('Server phone rejection is shown next to the field and a corrected retry succeeds @mobile-flow', async ({ page, baseURL }) => {
    const state = await fixture(page, baseURL, {
      registrationFailure: { status: 400, contentType: 'text/plain', body: 'phoneE164 inválido' },
    });
    const dialog = await openEnrollment(page);
    // Valid shape locally, rejected by the (synthetic) server.
    await fillEnrollment(dialog, { phone: '+44 7700 900123' });
    await dialog.getByRole('button', { name: 'Enviar inscripción' }).click();

    const phoneField = dialog.getByLabel('WhatsApp (opcional)');
    await expect(phoneField).toHaveAttribute('aria-invalid', 'true');
    await expect(dialog.getByText(/Revisa tu número de WhatsApp\. Usa un número como 0991234567/)).toBeVisible();
    await expect(phoneField).toBeFocused();
    await expect(dialog.getByText('phoneE164')).toHaveCount(0);
    expect(state.registrations).toHaveLength(1);

    // Local validation: an unusable number never reaches the server.
    await phoneField.fill('12345');
    await dialog.getByRole('button', { name: 'Enviar inscripción' }).click();
    await expect(dialog.getByText(/Revisa el número\. Usa un número como 0991234567/)).toBeVisible();
    expect(state.registrations).toHaveLength(1);
    expect(await fitsViewport(page)).toBe(true);

    // Correcting the number sends a new payload with a fresh Idempotency-Key.
    await page.unroute('**/*');
    const retry = await fixture(page, baseURL);
    await phoneField.fill('0988384849');
    await dialog.getByRole('button', { name: 'Enviar inscripción' }).click();
    await expect(page.getByRole('dialog', { name: 'Solicitud recibida' })).toBeVisible();
    expect(retry.registrations[0]).toMatchObject({ phoneE164: '+593988384849' });
    expect(retry.idempotencyKeys[0]).not.toBe(state.idempotencyKeys[0]);
    await expectRenderedRoot(page);
  });

  test('Sticky CTA appears after the hero, opens the form and never covers the page end @mobile-flow', async ({ page, baseURL }) => {
    await fixture(page, baseURL);
    await page.goto(`/curso/${SLUG}`, { waitUntil: 'domcontentloaded' });
    await expectRenderedRoot(page);
    const stickyBar = page.getByRole('region', { name: 'Inscripción rápida' });
    await expect(stickyBar).toBeHidden();

    await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
    await expect(stickyBar).toBeVisible();
    const lastLink = page.getByRole('link', { name: 'Ver mapa' });
    await expect(lastLink).toBeVisible();
    const [barBox, linkBox] = await Promise.all([stickyBar.boundingBox(), lastLink.boundingBox()]);
    expect(barBox && linkBox && linkBox.y + linkBox.height <= barBox.y).toBe(true);
    expect(await fitsViewport(page)).toBe(true);

    await stickyBar.getByRole('button', { name: 'Inscribirme' }).click();
    const dialog = page.getByRole('dialog', { name: 'Inscríbete en Curso de Batería con Guillermo Díaz' });
    await expectInViewport(page, dialog.getByLabel('Nombre completo'));
    await dialog.getByRole('button', { name: 'Cerrar' }).click();
    await expect(dialog).toBeHidden();
  });

  test('Signed-in visitors see the sticky CTA above the docked radio bar @mobile-flow', async ({ page, baseURL }) => {
    await fixture(page, baseURL, { authenticated: true });
    await page.goto(`/curso/${SLUG}`, { waitUntil: 'domcontentloaded' });
    await expectRenderedRoot(page);
    const radioBar = page.getByTestId('radio-docked-bar');
    await expect(radioBar).toBeVisible({ timeout: 15_000 });
    await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
    const stickyBar = page.getByRole('region', { name: 'Inscripción rápida' });
    if ((page.viewportSize()?.width ?? 0) >= 900) {
      await expect(stickyBar).toBeHidden();
      return;
    }
    await expect(stickyBar).toBeVisible();
    const [stickyBox, radioBox] = await Promise.all([stickyBar.boundingBox(), radioBar.boundingBox()]);
    // Stacked, not overlapping: the enrollment CTA stays fully tappable.
    expect(stickyBox.y + stickyBox.height).toBeLessThanOrEqual(radioBox.y + 0.5);
    await stickyBar.getByRole('button', { name: 'Inscribirme' }).click();
    await expect(page.getByRole('dialog', { name: 'Inscríbete en Curso de Batería con Guillermo Díaz' })).toBeVisible();
  });

  test('Deep link ?inscribirme=1 opens the form and closing removes the param @mobile-flow', async ({ page, baseURL }) => {
    await fixture(page, baseURL);
    await page.goto(`/curso/${SLUG}?utm_source=ig&inscribirme=1`, { waitUntil: 'domcontentloaded' });
    const dialog = page.getByRole('dialog', { name: 'Inscríbete en Curso de Batería con Guillermo Díaz' });
    await expectInViewport(page, dialog.getByLabel('Nombre completo'));
    // The login round-trip resumes enrollment and keeps campaign attribution.
    await expect(dialog.getByRole('link', { name: 'Inicia sesión para autocompletar' }))
      .toHaveAttribute('href', `/login?redirect=${encodeURIComponent(`/curso/${SLUG}?utm_source=ig&inscribirme=1`)}`);
    await dialog.getByRole('button', { name: 'Cerrar' }).click();
    await expect(dialog).toBeHidden();
    await expect(page).toHaveURL(new RegExp(`/curso/${SLUG}\\?utm_source=ig$`));
  });
});
