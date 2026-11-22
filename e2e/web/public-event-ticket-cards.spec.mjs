import { expect, test } from '@playwright/test';
import { localApiFixturePattern } from './helpers/local-api-fixture.mjs';

test.use({ serviceWorkers: 'block' });

// Synthetic rendering coverage; the separate compiled-API fixture exercises
// canonical tier/stock selection and public visibility on the complete schema.
test('public cards show supported ticket prices without guessing free entry @mobile-flow @critical', async ({ page, baseURL }, testInfo) => {
  const origin = new URL(baseURL).origin;
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const base = { publicUpcomingEventStart: '2099-08-20T22:00:00Z', publicUpcomingEventWorkflowStateCode: 'on_sale' };
  const rows = [
    { ...base, publicUpcomingEventId: '918758001', publicUpcomingEventTitle: 'Con entradas disponibles',
      publicUpcomingEventVenueName: 'Sala de prueba', publicUpcomingEventCity: 'Quito',
      publicUpcomingEventTicketFromPriceCents: 1500, publicUpcomingEventTicketCurrency: 'USD',
      publicUpcomingEventTicketsSoldOut: false },
    { ...base, publicUpcomingEventId: '918758002', publicUpcomingEventTitle: 'Sin cupos disponibles',
      publicUpcomingEventTicketsSoldOut: true, publicUpcomingEventTicketFromPriceCents: null },
    { ...base, publicUpcomingEventId: '918758003', publicUpcomingEventTitle: 'Precio de referencia solamente',
      publicUpcomingEventPriceCents: 0, publicUpcomingEventCurrency: 'USD' },
    { ...base, publicUpcomingEventId: '918758004', publicUpcomingEventTitle: 'Moneda desconocida',
      publicUpcomingEventTicketFromPriceCents: 1500, publicUpcomingEventTicketCurrency: 'not-a-currency' },
  ];
  await page.route(localApiFixturePattern(baseURL), async route => {
    const request = route.request(), url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    const path = url.pathname;
    if (path === '/session') return route.fulfill({ status: 200, json: null });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/session/preferences') return route.fulfill({ json: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } });
    if (path === '/social-events/upcoming') {
      expect(request.method()).toBe('GET');
      return route.fulfill({ json: rows });
    }
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture' } });
  });
  await page.goto('/social/eventos');
  await expect(page.getByRole('heading', { name: 'Próximos eventos', exact: true })).toBeVisible();
  const available = page.getByRole('link', { name: /Con entradas disponibles/ });
  await expect(available).toContainText('Sala de prueba · Quito');
  await expect(available).toContainText(/Entradas desde\s*\$\s*15,00/);
  await expect(available).toHaveAttribute('href', '/eventos/918758001');
  const soldOut = page.getByRole('link', { name: /Sin cupos disponibles/ });
  await expect(soldOut).toContainText('Agotado');
  await expect(soldOut).not.toContainText('Entradas desde');
  await expect(page.getByRole('link', { name: /Precio de referencia solamente/ })).not.toContainText(/Entradas desde|Agotado|Gratis/);
  await expect(page.getByRole('link', { name: /Moneda desconocida/ })).not.toContainText(/Entradas desde|Agotado|Gratis/);
  await available.focus();
  await expect(available).toBeFocused();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  expect(errors).toEqual([]);
  await page.screenshot({ path: testInfo.outputPath('public-ticket-card-prices.png'), fullPage: true });
});
