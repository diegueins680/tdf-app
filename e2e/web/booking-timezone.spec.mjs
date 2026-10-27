import { expect, test } from '@playwright/test';

const offeringId = '22222222-2222-4222-8222-222222222222';
const offering = {
  scId: offeringId, scCode: 'dj-booth-practice', scName: 'Práctica en DJ Booth',
  scNameEs: 'Práctica en DJ Booth', scNameEn: 'DJ Booth practice',
  scCategoryId: '77777777-7777-4777-8777-777777777777', scKind: 'dj-practice',
  scPricingModelId: '44444444-4444-4444-8444-444444444444', scPricingModel: 'hourly',
  scRateCents: 5000, scCurrency: 'USD', scCurrencyId: '55555555-5555-4555-8555-555555555555',
  scBillingUnit: 'hour', scTaxRateCode: 'ec-iva-standard', scDefaultDurationMinutes: 30,
  scRequiresEngineer: false, scDefaultResources: [], scSortOrder: 20, scActive: true,
};

// Both a UTC visitor and a visitor observing summer daylight saving must see
// the same physical Quito hours. All API traffic is synthetic, including POSTs.
for (const scenario of [
  { zone: 'UTC', expectedLabel: 'UTC', day: '2030-01-15', early: '12:45', open: '13:00', late: '02:30', close: '03:00', nextDay: '2030-01-16' },
  { zone: 'Europe/Madrid', expectedLabel: 'GMT+2', day: '2030-07-15', early: '14:45', open: '15:00', late: '04:30', close: '05:00', nextDay: '2030-07-16' },
]) {
  test.describe(`Studio hours for ${scenario.zone}`, () => {
    test.use({ timezoneId: scenario.zone });
    test('Quito boundaries and UTC submission survive a refused synthetic booking @critical @mobile-flow', async ({ page, baseURL }, testInfo) => {
      const origin = new URL(baseURL).origin;
      const writes = [];
      const availability = [];
      const errors = [];
      page.on('pageerror', (error) => errors.push(error.message));
      await page.clock.setFixedTime(new Date('2029-12-01T12:00:00Z'));
      await page.addInitScript(() => { window.__ENV__ = { VITE_DEFAULT_TIMEZONE: 'UTC' }; });
      await page.route('**/*', async (route) => {
        const req = route.request();
        const url = new URL(req.url());
        if (![origin, 'https://api.tdfrecords.net'].includes(url.origin)) return route.abort('blockedbyclient');
        if (!['fetch', 'xhr'].includes(req.resourceType())) {
          return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
        }
        const path = url.pathname.replace(/^\/api(?=\/)/, '');
        if (req.method() !== 'GET') {
          writes.push({ path, method: req.method(), body: req.postDataJSON() });
          return route.fulfill({ status: 409, json: { error: 'Horario sintético ocupado; elige otro.' } });
        }
        if (path === '/session') return route.fulfill({ status: 401, json: {} });
        if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
        if (path === '/services/catalog/public') return route.fulfill({ json: {
          sceSchemaVersion: 1, sceRevision: 1, sceLocale: 'es', sceItems: [offering],
        } });
        if (path === '/bookings/public/availability') {
          availability.push(Object.fromEntries(url.searchParams));
          return route.fulfill({ json: { available: true } });
        }
        if (path === '/engineers') return route.fulfill({ json: [] });
        if (path.startsWith('/reviews/')) return route.fulfill({ json: { items: [], summary: { count: 0, average: 0 }, nextCursor: null } });
        if (path.startsWith('/catalogs/')) return route.fulfill({ json: {} });
        return route.fulfill({ status: 404, json: {} });
      });
      await page.goto('/reservar?service=dj-booth-practice');
      const card = page.getByTestId('public-booking-card');
      await expect(card).toContainText('08:00–22:00, Quito (GMT-5)');
      await page.getByLabel('Nombre completo').fill('Persona Sintética');
      await page.getByRole('textbox', { name: 'Correo', exact: true }).fill('synthetic@example.invalid');
      await page.getByRole('button', { name: 'Continuar', exact: true }).click();
      const date = page.getByLabel(/^Fecha y hora/);
      await page.getByLabel('Duración (min)', { exact: true }).fill('30');
      await date.fill(`${scenario.day}T${scenario.early}`); // 07:45 in Quito
      // Existing input normalization brings 07:45 forward to opening.
      await expect(date).toHaveValue(`${scenario.day}T${scenario.open}`);
      await expect.poll(() => availability.at(-1)?.startsAt).toBe(`${scenario.day}T13:00:00.000Z`);
      await page.getByRole('button', { name: 'Revisar reserva', exact: true }).click();
      await expect(page.getByRole('button', { name: 'Confirmar reserva', exact: true })).toBeVisible();
      await page.getByRole('button', { name: 'Volver', exact: true }).click();
      await date.fill(`${scenario.nextDay}T${scenario.close}`); // 22:00 in Quito
      // Existing normalization moves a start at closing back to the latest
      // valid 30-minute interval, 21:30–22:00 in Quito.
      await expect(date).toHaveValue(`${scenario.nextDay}T${scenario.late}`);
      await expect(page.getByLabel('Duración (min)', { exact: true })).toHaveValue('30');
      const expectedUtc = `${scenario.nextDay}T02:30:00.000Z`;
      await expect.poll(() => availability.at(-1)?.startsAt).toBe(expectedUtc);
      await page.getByRole('button', { name: 'Revisar reserva', exact: true }).click();
      await expect(card).toContainText(`Zona: Estudio GMT-5 · Tú ${scenario.expectedLabel}`);
      expect(writes).toEqual([]);
      await page.getByRole('button', { name: 'Confirmar reserva', exact: true }).click();
      await expect(card.getByRole('alert').filter({ hasText: 'Horario sintético ocupado' })).toBeVisible();
      expect(writes).toHaveLength(1);
      expect(writes[0]).toMatchObject({ path: '/bookings/public', method: 'POST', body: {
        pbStartsAt: expectedUtc, pbDurationMinutes: 30, pbServiceOfferingId: offeringId,
        pbEmail: 'synthetic@example.invalid',
      } });
      await expect(card).not.toContainText('Solicitud registrada');
      expect(errors).toEqual([]);
      await page.screenshot({ path: testInfo.outputPath('quito-booking.png'), fullPage: true });
    });
  });
}
