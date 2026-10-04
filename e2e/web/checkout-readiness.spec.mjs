import { expect, test } from '@playwright/test';

// Synthetic transport: no payment, inventory mutation or account request reaches a service.
for (const locale of ['es', 'en']) {
  test(`Checkout preserves input and permits retry/cancel without payment readiness (${locale}) @critical`, async ({ page, baseURL }) => {
    let intents = 0;
    const origin = new URL(baseURL).origin;
    const preferences = { locale, currency: 'USD', timezone: 'America/Guayaquil' };
    await page.addInitScript((language) => localStorage.setItem('tdf-locale', language), locale);
    await page.route('**/*', async (route) => {
      const request = route.request();
      const url = new URL(request.url());
      if (!['fetch', 'xhr'].includes(request.resourceType())) {
        return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
      }
      const path = url.pathname.replace(/^\/api(?=\/)/, '');
      if (path === '/session') return route.fulfill({ json: {
        username: 'synthetic-buyer', displayName: 'Comprador sintético', partyId: 42,
        roles: ['Admin', 'Customer'], modules: [], featureFlags: [], preferences,
      } });
      if (path === '/session/preferences') return route.fulfill({ json: preferences });
      if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
      if (path === '/social-events/events') return route.fulfill({ json: [{
        eventId: '9999', eventOrganizerPartyId: '999', eventTitle: 'Synthetic Checkout',
        eventStart: '2030-12-01T20:00:00Z', eventArtists: [], eventPublicListable: true,
        eventTicketPurchaseEnabled: true, eventCurrency: 'USD',
      }] });
      if (path === '/social-events/events/9999/ticket-tiers') return route.fulfill({ json: [{
        ticketTierId: '9999', ticketTierEventId: '9999', ticketTierCode: 'GENERAL',
        ticketTierName: 'General', ticketTierPriceCents: 500, ticketTierCurrency: 'USD',
        ticketTierQuantityTotal: 100, ticketTierQuantitySold: 0, ticketTierActive: true,
      }] });
      if (path === '/social-events/stripe/create-payment-intent') {
        intents++;
        return route.fulfill({ status: 503, json: { error: 'Unexpected payment request' } });
      }
      if (path.startsWith('/catalogs/')) return route.fulfill({ json: { catalogs: [], items: [] } });
      return route.fulfill({ json: [] });
    });
    await page.goto('/social/eventos');
    await page.getByRole('button', { name: 'Comprar con tarjeta' }).click();
    const dialog = page.getByRole('dialog');
    const name = dialog.getByLabel(locale === 'es' ? 'Tu nombre' : 'Your Name', { exact: false });
    await name.fill('Comprador sintético');
    await dialog.getByLabel(locale === 'es' ? 'Correo electrónico' : 'Email', { exact: false }).fill('buyer@example.test');
    const next = dialog.getByRole('button', { name: locale === 'es' ? 'Continuar al pago' : 'Continue to Payment', exact: true });
    await next.click();
    await expect(dialog.getByRole('alert')).toContainText(locale === 'es' ? 'El pago no está disponible' : 'Payment is unavailable');
    expect(intents).toBe(0);
    await expect(name).toHaveValue('Comprador sintético');
    // Ordinary clicks detect a radio/player layer obscuring the modal footer.
    await next.click();
    expect(intents).toBe(0);
    await dialog.getByRole('button', { name: locale === 'es' ? 'Cancelar' : 'Cancel', exact: true }).click();
    await expect(dialog).toHaveCount(0);
  });
}
