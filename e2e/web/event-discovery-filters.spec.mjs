import { expect, test } from '@playwright/test';
import { localApiFixturePattern } from './helpers/local-api-fixture.mjs';

// Synthetic browser/API fixtures; database city matching is tested separately.
test.use({ serviceWorkers: 'block' });
test('event city, artist union and past filters work at narrow widths @mobile-flow @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const reads = [];
  const artists = [{ artistId: '7', artistName: 'Alpha' }, { artistId: '8', artistName: 'Beta' }];
  const events = [
    { eventId: '21', eventTitle: 'New York futuro', eventStart: '2099-05-03T20:00:00Z', eventArtists: [artists[0]] },
    { eventId: '22', eventTitle: 'New York anterior', eventStart: '2020-05-03T20:00:00Z', eventArtists: [artists[1]] },
  ];
  await page.route(localApiFixturePattern(baseURL), async route => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    if (url.pathname === '/session') return route.fulfill({ json: {
      username: 'synthetic-event-filter', displayName: 'Persona Sintética', partyId: 42,
      roles: ['Customer', 'Fan'], modules: [], featureFlags: [],
      preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
    } });
    if (url.pathname === '/health') return route.fulfill({ json: { status: 'ok' } });
    if (url.pathname === '/session/preferences') return route.fulfill({ json: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } });
    if (url.pathname === '/social-events/events') {
      expect(request.method()).toBe('GET');
      reads.push(Object.fromEntries(url.searchParams));
      const ids = (url.searchParams.get('artistId') ?? '').split(',').filter(Boolean);
      const rows = events.filter(event => (!url.searchParams.has('start_after') || event.eventStart > url.searchParams.get('start_after'))
        && (!ids.length || event.eventArtists.some(artist => ids.includes(artist.artistId))));
      return route.fulfill({ json: rows });
    }
    if (url.pathname === '/social-events/artists') {
      const name = (url.searchParams.get('name') ?? '').toLowerCase();
      return route.fulfill({ json: artists.filter(a => a.artistName.toLowerCase().includes(name)) });
    }
    if (url.pathname.startsWith('/social-events/artists/')) return route.fulfill({ json: artists.find(a => url.pathname.endsWith('/' + a.artistId)) });
    if (url.pathname.startsWith('/social-events/')) return route.fulfill({ json: [] });
    if (url.pathname.includes('workflow')) return route.fulfill({ json: { states: [] } });
    if (url.pathname.includes('catalog')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture' } });
  });
  await page.goto('/social/eventos');
  await page.getByRole('textbox', { name: 'Ciudad', exact: true }).fill('New York');
  await expect.poll(() => reads.at(-1)?.city).toBe('New York');
  await expect(page.getByText('1 evento en esta página', { exact: true })).toBeVisible();
  const input = page.getByRole('combobox', { name: 'Artistas', exact: true });
  await input.fill('Alpha');
  await page.getByRole('option', { name: 'Alpha · #7' }).click();
  await input.fill('Beta');
  await page.getByRole('option', { name: 'Beta · #8' }).click();
  await expect.poll(() => reads.at(-1)?.artistId).toBe('7,8');
  await page.getByRole('checkbox', { name: 'Incluir eventos pasados' }).check();
  await expect(page.getByText('2 eventos en esta página', { exact: true })).toBeVisible();
  await expect(page.getByText('Evento pasado', { exact: true })).toBeVisible();
  await expect(page).toHaveURL(/past=1/);
  await page.reload();
  await expect(page.getByRole('textbox', { name: 'Ciudad', exact: true })).toHaveValue('New York');
  await expect(page.getByRole('checkbox', { name: 'Incluir eventos pasados' })).toBeChecked();
  await expect(page.getByText('2 eventos en esta página', { exact: true })).toBeVisible();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth + 1)).toBe(true);
  await page.getByRole('button', { name: 'Limpiar filtros' }).focus();
  await page.keyboard.press('Enter');
  await expect(page.getByRole('textbox', { name: 'Ciudad', exact: true })).toHaveValue('');
  await expect(page.getByRole('checkbox', { name: 'Incluir eventos pasados' })).not.toBeChecked();
});
