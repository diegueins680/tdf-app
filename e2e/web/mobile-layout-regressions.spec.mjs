import { expect, test } from '@playwright/test';

// Layout regressions reported from a real 360px Android (Chrome) user test.
// Every API request is answered with synthetic read-only fixtures; foreign
// origins are aborted, so nothing reaches a real service.
const VIEWPORT = { width: 360, height: 760 };
const DJ_PRACTICE_ID = '22222222-2222-4222-8222-222222222222';

const djPracticeService = {
  scId: DJ_PRACTICE_ID,
  scCode: 'dj-booth-practice',
  scName: 'Práctica en DJ Booth',
  scNameEs: 'Práctica en DJ Booth',
  scNameEn: 'DJ Booth practice',
  scCategoryId: '77777777-7777-4777-8777-777777777777',
  scKind: 'dj-practice',
  scPricingModelId: '44444444-4444-4444-8444-444444444444',
  scPricingModel: 'hourly',
  scRateCents: 5000,
  scCurrency: 'USD',
  scCurrencyId: '55555555-5555-4555-8555-555555555555',
  scBillingUnit: 'hour',
  scTaxRateCode: 'ec-iva-standard',
  scDefaultDurationMinutes: 60,
  scRequiresEngineer: false,
  scDefaultResources: [{
    sdrResourceId: '3',
    sdrResourceName: 'DJ Booth',
    sdrSelectionModeId: '88888888-8888-4888-8888-888888888888',
    sdrSelectionMode: 'first-available',
    sdrSortOrder: 10,
  }],
  scSortOrder: 20,
  scActive: true,
};

const syntheticAccount = {
  username: 'synthetic-listener',
  displayName: 'Cuenta Sintética',
  partyId: 42,
  roles: ['customer'],
  modules: [],
  featureFlags: [],
  preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
};

async function fixture(page, baseURL, { authenticated = false } = {}) {
  const origin = new URL(baseURL).origin;
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (path === '/session') {
      return authenticated
        ? route.fulfill({ json: syntheticAccount })
        : route.fulfill({ status: 401, json: { error: 'unauthenticated' } });
    }
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/services/catalog/public') {
      return route.fulfill({ json: {
        sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [djPracticeService],
      } });
    }
    if (path === '/rooms/public') return route.fulfill({ json: [{ roomId: 'room-dj', rName: 'DJ Booth', rBookable: true }] });
    if (path === '/engineers') return route.fulfill({ json: [] });
    if (path === '/bookings/public/availability') return route.fulfill({ json: { available: true } });
    if (path.startsWith('/reviews/service_offering/')) {
      return route.fulfill({ json: {
        items: [{
          id: 'review-1',
          targetKind: 'service_offering',
          targetId: DJ_PRACTICE_ID,
          rating: 5,
          body: 'Muy buena sala para practicar, el equipo estaba listo a tiempo.',
          status: 'published',
          createdAt: '2026-09-01T12:00:00Z',
          verified: true,
          sourceKind: 'service_booking',
          author: { name: 'Persona Sintética', avatarUrl: null },
        }],
        summary: { targetKind: 'service_offering', targetId: DJ_PRACTICE_ID, count: 1, average: 5 },
        nextCursor: null,
      } });
    }
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: {} });
    if (path.startsWith('/radio/presence')) return route.fulfill({ json: null });
    if (path.startsWith('/radio/')) return route.fulfill({ json: [] });
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
}

async function expectRootHasVisibleText(page) {
  await expect.poll(() => page.evaluate(() => (document.querySelector('#root')?.innerText ?? '').trim().length))
    .toBeGreaterThan(40);
}

const fitsViewport = (page) => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth);

const relativeLuminance = (rgb) => {
  const [r, g, b] = rgb.map((value) => {
    const channel = value / 255;
    return channel <= 0.03928 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4;
  });
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
};
const parseRgb = (css) => (css.match(/[\d.]+/g) ?? []).slice(0, 3).map(Number);

test('Booking keeps the card full width and stacks service reviews below it at 360px @mobile-flow', async ({ page, baseURL }) => {
  await page.setViewportSize(VIEWPORT);
  await fixture(page, baseURL);
  // The public service query parameter selects the offering exactly like the
  // service picker does; the reviews card only renders once one is selected.
  await page.goto('/reservar?service=dj-booth-practice');
  await expectRootHasVisibleText(page);

  const card = page.getByTestId('public-booking-card');
  const reviews = page.getByTestId('public-booking-reviews');
  await expect(card).toBeVisible();
  await expect(reviews.getByRole('heading', { name: 'Reseñas del servicio' })).toBeVisible();

  const viewportWidth = page.viewportSize().width;
  const cardBox = await card.boundingBox();
  const reviewsBox = await reviews.boundingBox();
  expect(cardBox.width).toBeGreaterThanOrEqual(0.85 * viewportWidth);
  expect(reviewsBox.y).toBeGreaterThanOrEqual(cardBox.y + cardBox.height - 1);
  expect(reviewsBox.width).toBeGreaterThanOrEqual(0.85 * viewportWidth);

  // Long copy must never be squeezed into a sliver (the bug broke words letter by letter).
  const squeezed = await card.evaluate((root) => Array.from(root.querySelectorAll('p'))
    .filter((el) => (el.textContent ?? '').trim().length >= 40)
    .map((el) => ({ text: (el.textContent ?? '').trim().slice(0, 40), width: el.getBoundingClientRect().width }))
    .filter((entry) => entry.width > 0 && entry.width < 160));
  expect(squeezed).toEqual([]);

  // Intro step chips wrap by words inside the viewport instead of being clipped.
  const chipOverflow = await card.evaluate((root) => Array.from(root.querySelectorAll('.MuiChip-root'))
    .map((chip) => chip.getBoundingClientRect())
    .filter((rect) => rect.width > 0 && rect.right > window.innerWidth + 0.5).length);
  expect(chipOverflow).toBe(0);
  await expect.poll(() => fitsViewport(page)).toBe(true);
});

test('Live session registration fits 360px and keeps the access-code toggle legible on navy @mobile-flow', async ({ page, baseURL }) => {
  await page.setViewportSize(VIEWPORT);
  await fixture(page, baseURL);
  await page.goto('/live-sessions/registro');
  await expectRootHasVisibleText(page);

  const project = page.locator('.MuiPaper-root')
    .filter({ has: page.getByRole('heading', { name: 'Proyecto', exact: true }) })
    .last();
  await expect(project).toBeVisible();
  await expect.poll(() => fitsViewport(page)).toBe(true);

  const overflowing = await project.evaluate((paper) => {
    const paperRight = paper.getBoundingClientRect().right;
    return Array.from(paper.querySelectorAll('input, textarea'))
      .filter((field) => field.getAttribute('aria-hidden') !== 'true' && field.type !== 'hidden')
      .map((field) => ({ name: field.getAttribute('aria-label') ?? field.id, right: field.getBoundingClientRect().right }))
      .filter((field) => field.right > paperRight + 0.5);
  });
  expect(overflowing).toEqual([]);

  // Outlined field roots (the visible boxes) must also stay inside the Paper.
  const outlinesOverflowing = await project.evaluate((paper) => {
    const paperRight = paper.getBoundingClientRect().right;
    return Array.from(paper.querySelectorAll('.MuiInputBase-root'))
      .filter((root) => root.getBoundingClientRect().right > paperRight + 0.5).length;
  });
  expect(outlinesOverflowing).toBe(0);

  const toggle = page.getByRole('button', { name: 'Mostrar código' });
  await expect(toggle).toBeVisible();
  const toggleBox = await toggle.boundingBox();
  expect(toggleBox.width).toBeGreaterThanOrEqual(44);
  expect(toggleBox.height).toBeGreaterThanOrEqual(44);
  const toggleColor = parseRgb(await toggle.evaluate((el) => getComputedStyle(el).color));
  // Effective background: the nearest ancestor with a mostly opaque fill (the navy Paper).
  const shellBackground = parseRgb(await toggle.evaluate((el) => {
    for (let node = el.parentElement; node; node = node.parentElement) {
      const color = getComputedStyle(node).backgroundColor;
      const parts = (color.match(/[\d.]+/g) ?? []).map(Number);
      const alpha = parts.length > 3 ? parts[3] : 1;
      if (parts.length >= 3 && alpha >= 0.5) return color;
    }
    return getComputedStyle(document.body).backgroundColor;
  }));
  const lighter = Math.max(relativeLuminance(toggleColor), relativeLuminance(shellBackground));
  const darker = Math.min(relativeLuminance(toggleColor), relativeLuminance(shellBackground));
  expect((lighter + 0.05) / (darker + 0.05)).toBeGreaterThanOrEqual(3);
  expect(relativeLuminance(toggleColor)).toBeGreaterThan(0.5);

  await toggle.click();
  await expect(page.getByRole('button', { name: 'Ocultar código' })).toBeVisible();
});

test('Docked radio bar fits 360px and never hides the bottom of the page @mobile-flow', async ({ page, baseURL }) => {
  await page.setViewportSize(VIEWPORT);
  await fixture(page, baseURL, { authenticated: true });
  await page.goto('/live-sessions/registro');
  await expectRootHasVisibleText(page);

  const bar = page.getByTestId('radio-docked-bar');
  await expect(bar).toBeVisible({ timeout: 15_000 });
  await expect.poll(() => fitsViewport(page)).toBe(true);

  const barBox = await bar.boundingBox();
  expect(barBox.x).toBeGreaterThanOrEqual(0);
  expect(barBox.x + barBox.width).toBeLessThanOrEqual(VIEWPORT.width + 0.5);
  for (const label of ['Silenciar radio', 'Ocultar barra de radio', 'Expandir radio']) {
    const control = bar.getByRole('button', { name: label });
    await expect(control).toBeVisible();
    const box = await control.boundingBox();
    expect(box.x + box.width).toBeLessThanOrEqual(VIEWPORT.width + 0.5);
  }
  // Prev/next stay in the expanded panel; the phone bar drops them for room.
  await expect(bar.getByRole('button', { name: 'Saltar a la estación anterior' })).toHaveCount(0);

  const reserved = await page.evaluate(() => ({
    variable: getComputedStyle(document.documentElement).getPropertyValue('--tdf-radio-bar-height').trim(),
    padding: parseFloat(getComputedStyle(document.body).paddingBottom),
  }));
  expect(reserved.variable).toMatch(/^\d+px$/);
  expect(reserved.padding).toBeGreaterThanOrEqual(barBox.height - 1);

  // The final action of the form scrolls clear of the docked bar.
  await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
  const submit = page.getByRole('button', { name: 'Enviar Live Session' });
  await expect(submit).toBeVisible();
  const submitBox = await submit.boundingBox();
  const barTop = (await bar.boundingBox()).y;
  expect(submitBox.y + submitBox.height).toBeLessThanOrEqual(barTop + 0.5);

  // Hiding the bar releases the reserved space.
  await bar.getByRole('button', { name: 'Ocultar barra de radio' }).click();
  await expect.poll(() => page.evaluate(
    () => getComputedStyle(document.documentElement).getPropertyValue('--tdf-radio-bar-height').trim(),
  )).toBe('');
});
