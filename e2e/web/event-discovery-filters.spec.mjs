import { expect, test } from '@playwright/test';
import { localApiFixturePattern } from './helpers/local-api-fixture.mjs';

// Synthetic browser/API fixtures; database city matching is tested separately.
test.use({ serviceWorkers: 'block' });
test('event city, artist union and past filters work at narrow widths @mobile-flow @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const reads = [];
  const artists = [{ artistId: '7', artistName: 'Alpha' }, { artistId: '8', artistName: 'Beta' }];
  const events = [
    { eventId: '21', eventTitle: 'New York futuro', eventVenueId: '225', eventStart: '2099-05-03T20:00:00Z', eventArtists: [artists[0]] },
    { eventId: '22', eventTitle: 'New York anterior', eventVenueId: '250', eventStart: '2020-05-03T20:00:00Z', eventArtists: [artists[1]] },
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
    if (url.pathname === '/social-events/venues') throw new Error('The event workspace must not fetch the global venue catalog');
    if (url.pathname.startsWith('/social-events/venues/')) return route.fulfill({ json: { venueId: url.pathname.split('/').pop(), venueName: `Sala ${url.pathname.split('/').pop()}` } });
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
  // The selector's value comes from the `artists` URL parameter; wait for the
  // first pick to reach the request before adding the second one.
  await expect.poll(() => reads.at(-1)?.artistId).toBe('7');
  await input.fill('Beta');
  await page.getByRole('option', { name: 'Beta · #8' }).click();
  await expect.poll(() => reads.at(-1)?.artistId).toBe('7,8');
  // The switch is controlled by the `past` URL parameter, so its checked state
  // only flips after the router commits; click and let the assertion retry.
  const includePast = page.getByRole('checkbox', { name: 'Incluir eventos pasados' });
  await includePast.click();
  await expect(includePast).toBeChecked();
  await expect(page.getByText('2 eventos en esta página', { exact: true })).toBeVisible();
  await expect(page.getByText('Evento pasado', { exact: true })).toBeVisible();
  await expect(page.getByText('Sala 225', { exact: true }).first()).toBeVisible();
  await expect(page.getByText('Sala 250', { exact: true })).toBeVisible();
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

test('event cards and calendar resolve venues after row 200 across pages and cities @mobile-flow @critical', async ({ page, baseURL }) => {
  const origin = new URL(baseURL).origin;
  const venueReads = [];
  const eventReads = [];
  let globalReads = 0;
  const venues = Array.from({ length: 250 }, (_, index) => ({
    venueId: String(index + 1), venueName: `Sala ${String(index + 1).padStart(3, '0')}`,
    venueCity: index < 225 ? 'Quito' : 'New York',
  }));
  const events = Array.from({ length: 23 }, (_, index) => ({
    eventId: String(index + 1), eventTitle: `Concierto ${index + 1}`,
    eventStart: '2099-05-03T20:00:00Z', eventVenueId: index < 20 ? '225' : '250', eventArtists: [],
  }));
  await page.route(localApiFixturePattern(baseURL), async route => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.origin !== origin) return route.abort('blockedbyclient');
    if (!['fetch', 'xhr'].includes(request.resourceType())) return route.continue();
    if (url.pathname === '/session') return route.fulfill({ json: {
      username: 'synthetic-venue-reader', displayName: 'Persona Sintética', partyId: 42,
      roles: ['Customer', 'Fan'], modules: [], featureFlags: [],
      preferences: { locale: 'es', currency: 'USD', timeZone: 'America/Guayaquil' },
    } });
    if (url.pathname === '/health') return route.fulfill({ json: { status: 'ok' } });
    if (url.pathname === '/session/preferences') return route.fulfill({ json: { locale: 'es', currency: 'USD', timezone: 'America/Guayaquil' } });
    if (url.pathname === '/social-events/events') {
      const offset = Number(url.searchParams.get('offset') ?? 0);
      eventReads.push({ city: url.searchParams.get('city'), offset });
      const limit = Number(url.searchParams.get('limit') ?? 21);
      const matching = url.searchParams.get('city') === 'New York' ? events.slice(20) : events;
      return route.fulfill({ json: matching.slice(offset, offset + limit) });
    }
    if (url.pathname === '/social-events/venues') {
      globalReads += 1;
      return route.fulfill({ json: venues.slice(0, 200) });
    }
    if (url.pathname.startsWith('/social-events/venues/')) {
      const id = url.pathname.split('/').pop();
      venueReads.push(id);
      return route.fulfill({ json: venues.find(venue => venue.venueId === id) });
    }
    if (url.pathname.startsWith('/social-events/')) return route.fulfill({ json: [] });
    if (url.pathname.includes('workflow')) return route.fulfill({ json: { states: [] } });
    if (url.pathname.includes('catalog')) return route.fulfill({ json: { items: [] } });
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture' } });
  });
  await page.goto('/social/eventos');
  await expect(page.getByText('20 eventos en esta página', { exact: true })).toBeVisible();
  await expect(page.getByText('Sala 225', { exact: true })).toHaveCount(40);
  expect(venueReads).toEqual(['225']);
  await page.getByRole('button', { name: 'Siguiente', exact: true }).click();
  await expect(page.getByText('3 eventos en esta página', { exact: true })).toBeVisible();
  await expect(page.getByText('Sala 250', { exact: true })).toHaveCount(6);
  await expect(page.getByText('Sala 225', { exact: true })).toHaveCount(0);
  await page.getByRole('textbox', { name: 'Ciudad', exact: true }).fill('New York');
  await expect(page).not.toHaveURL(/page=2/);
  await expect.poll(() => eventReads.at(-1)).toEqual({ city: 'New York', offset: 0 });
  await expect(page.getByText('Sala 250', { exact: true })).toHaveCount(6);
  expect(venueReads).toEqual(['225', '250']);
  expect(globalReads).toBe(0);
  await expect(page.getByText('Venue por definir', { exact: true })).toHaveCount(0);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth + 1)).toBe(true);
});
