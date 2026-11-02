import { expect, test } from '@playwright/test';

// Synthetic API and PCM media; the browser's decoder, audio clock, router and
// sidebar are real. No production accounts or radio services are contacted.
const wav = (() => {
  const sampleRate = 8000;
  const size = sampleRate * 2 * 180;
  const bytes = Buffer.alloc(44 + size);
  bytes.write('RIFF'); bytes.writeUInt32LE(36 + size, 4);
  bytes.write('WAVEfmt ', 8); bytes.writeUInt32LE(16, 16);
  bytes.writeUInt16LE(1, 20); bytes.writeUInt16LE(1, 22);
  bytes.writeUInt32LE(sampleRate, 24); bytes.writeUInt32LE(sampleRate * 2, 28);
  bytes.writeUInt16LE(2, 32); bytes.writeUInt16LE(16, 34);
  bytes.write('data', 36); bytes.writeUInt32LE(size, 40);
  return bytes;
})();

test('@critical radio keeps native playback and controls across sidebar navigation and history', async ({ page, baseURL }, testInfo) => {
  test.setTimeout(120_000);
  page.setDefaultTimeout(15000);
  const origin = new URL(baseURL).origin;
  const account = { username: 'radio-navigation-fixture', displayName: 'Radio navigation', partyId: 42,
    roles: ['admin'], modules: ['admin', 'school', 'ops', 'crm', 'scheduling', 'invoicing', 'packages', 'label', 'internships'],
    featureFlags: [], preferences: { locale: 'es', currency: 'USD' } };
  await page.route('**/*', async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    const path = url.pathname.replace(/^\/api(?=\/)/, '');
    if (url.origin === origin && path === '/fixture-radio.wav') {
      return route.fulfill({ contentType: 'audio/wav', body: wav });
    }
    if (!['fetch', 'xhr'].includes(request.resourceType())) {
      return url.origin === origin ? route.continue() : route.abort('blockedbyclient');
    }
    if (path === '/session') return route.fulfill({ json: account });
    if (path === '/health') return route.fulfill({ json: { status: 'ok', db: 'ok' } });
    if (path === '/radio/streams') return route.fulfill({ json: [{ rsId: 1, rsName: 'Navigation fixture', rsActive: true, rsStreamUrl: `${origin}/fixture-radio.wav` }] });
    if (path === '/radio/auto-stop-options') return route.fulfill({ json: { options: [] } });
    if (path === '/radio/streams/now-playing') return route.fulfill({ json: {} });
    if (path === '/radio/presence') return route.fulfill({ json: null });
    if (path === '/catalogs/batch') return route.fulfill({ json: {} });
    if (path.startsWith('/catalogs/')) return route.fulfill({ json: { items: [], total: 0 } });
    if (path === '/access-requests') return route.fulfill({ json: [] });
    return route.fulfill({ status: 404, json: { error: 'No synthetic fixture for this API' } });
  });
  await page.addInitScript(() => {
    localStorage.setItem('radio-volume', '0.35');
    globalThis.__radioEvents = [];
    for (const type of ['playing', 'pause', 'emptied', 'error']) document.addEventListener(type, (event) => {
      if (event.target instanceof HTMLAudioElement) globalThis.__radioEvents.push({ type, trusted: event.isTrusted, time: event.target.currentTime });
    }, true);
  });
  await page.goto('/solicitudes-acceso/revision');
  const bar = page.getByTestId('radio-docked-bar');
  await expect(bar).toContainText('Navigation fixture');
  await bar.getByRole('button', { name: 'Reproducir radio', exact: true }).click();
  const audio = page.locator('audio[src$="/fixture-radio.wav"]');
  await expect.poll(() => audio.evaluate((a) => !a.paused && a.currentTime > 0.25)).toBe(true);
  await audio.evaluate((a) => { globalThis.__originalRadioAudio = a; globalThis.__radioEvents = []; });
  const checkpoints = [];
  const check = async (label) => {
    await expect(bar).toBeVisible();
    await expect(bar.getByRole('button', { name: 'Pausar radio', exact: true })).toBeVisible();
    const before = await page.evaluate(() => {
      const a = globalThis.__originalRadioAudio;
      return { connected: a.isConnected, paused: a.paused, volume: a.volume, time: a.currentTime,
        source: a.currentSrc, errors: a.error?.code ?? null };
    });
    expect(before.connected).toBe(true);
    expect(before.paused).toBe(false);
    expect(before.volume).toBeCloseTo(0.35, 6);
    if (checkpoints.length) expect(before.volume).toBe(checkpoints[0].volume);
    expect(before.source).toBe(`${origin}/fixture-radio.wav`);
    expect(before.errors).toBeNull();
    await expect.poll(() => page.evaluate(() => globalThis.__originalRadioAudio.currentTime)).toBeGreaterThan(before.time + 0.15);
    checkpoints.push({ label, ...before });
  };
  await check('started');
  const nav = page.getByRole('navigation', { name: 'Navegación de la aplicación' });
  const openSidebar = async () => {
    // History can report its new URL before React replaces the public layout.
    await expect(page.locator('#app-sidebar')).toBeAttached();
    if (!await nav.isVisible()) await page.getByRole('button', { name: 'Mostrar menú lateral', exact: true }).click();
    await expect(nav).toBeVisible();
  };
  await openSidebar();
  // Open/close group headings without navigating, then select actual links.
  const toggle = nav.getByRole('button', { name: /Mostrar|Ocultar/ }).first();
  await toggle.click();
  await check('menu section toggle');
  await toggle.click();
  await check('menu section toggle again');
  for (const [query, path] of [
    ['Inscripciones de cursos', '/configuracion/inscripciones-curso'],
    ['Inicio', '/inicio'],
    ['Tienda', '/marketplace'],
  ]) {
    await openSidebar();
    const search = nav.getByRole('textbox', { name: 'Buscar sección' });
    await search.fill(query);
    await nav.locator(`a[href="${path}"]`).last().click();
    await expect(page).toHaveURL(new RegExp(`${path}$`));
    await check(path);
    await page.goBack();
    await check(`Back from ${path}`);
    await page.goForward();
    await check(`Forward to ${path}`);
    await page.goBack();
  }
  // Repeat a previously visited destination after traversing browser history.
  await openSidebar();
  const search = nav.getByRole('textbox', { name: 'Buscar sección' });
  await search.fill('Solicitudes de acceso');
  await nav.locator('a[href="/solicitudes-acceso/revision"]').last().click();
  await check('return to access requests');
  const events = await page.evaluate(() => globalThis.__radioEvents);
  expect(events.filter((e) => ['pause', 'emptied', 'error'].includes(e.type))).toEqual([]);
  await testInfo.attach('native-radio-navigation.json', { body: JSON.stringify({ checkpoints, events }, null, 2), contentType: 'application/json' });
});
