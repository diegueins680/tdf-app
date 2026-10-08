import { expect, test } from '@playwright/test';
import axe from 'axe-core';

const releaseId = '11000000-0000-4000-8000-000000000001';
const versionId = '22000000-0000-4000-8000-000000000001';
const trackId = (i) => `33000000-0000-4000-8000-00000000000${i}`;
const recordingId = (i) => `44000000-0000-4000-8000-00000000000${i}`;
const assetId = (i, q) => `55000000-0000-4000-8000-0000000000${i}${q}`;
const qualities = ['low', 'high', 'lossless'];

async function prepare(page) {
  const fixture = JSON.parse(process.env.TDF_NATIVE_MUSIC_FIXTURE);
  const events = [], deniedNetwork = [];
  const summary = { id: releaseId, versionId, versionNumber: 1, artistPartyId: 101,
    slug: 'audio-nativo-sintetico', kind: 'album', title: 'Audio nativo sintético',
    displayArtist: 'Fixture TDF', releaseAtUtc: '2026-09-13T00:00:00Z', publishedAt: '2026-09-13T00:00:01Z',
    labelName: 'Prueba sintética', coverAssetId: null };
  const release = { ...summary, subtitle: null, versionTitle: null, catalogNumber: null,
    explicitContent: 'not_explicit', originalReleaseDate: '2026-09-13', coverAssets: [],
    tracks: [1, 2].map((i) => ({ trackId: trackId(i), recordingId: recordingId(i),
      discNumber: 1, trackNumber: i, title: `Pista nativa ${i}`, displayArtist: 'Fixture TDF', durationMs: 24000,
      explicitContent: 'not_explicit', sources: qualities.map((quality, q) => ({
        assetId: assetId(i, q), role: 'stream_audio', mediaType: fixture.files[quality].mediaType,
        technicalMetadata: { bitrate_kbps: [96, 256, 0][q], loudness_lufs: -14 },
      })) })),
    availability: [{ ruleId: '66000000-0000-4000-8000-000000000001', trackId: null,
      territoryMode: 'include', territories: ['EC'], startsAt: null, endsAt: null,
      listeningPolicy: 'full', downloadPolicy: 'none', purchasable: false, priceMinor: null, currency: null }],
  };
  // Metadata/auth are explicitly synthetic. Media bytes are never intercepted.
  await page.route('**/*', async (route) => {
    const url = new URL(route.request().url());
    if (!['http:', 'https:'].includes(url.protocol)) return route.continue();
    if (url.hostname !== '127.0.0.1' && url.hostname !== 'localhost') {
      deniedNetwork.push(url.origin); return route.abort('blockedbyclient');
    }
    if (url.pathname === '/session') return route.fulfill({ status: 401, json: { error: 'synthetic visitor' } });
    if (url.pathname === '/fans/artists') return route.fulfill({ json: [] });
    if (url.pathname === '/catalogs/batch') return route.fulfill({ json: {} });
    if (url.pathname === '/music/releases') return route.fulfill({ json: [summary] });
    if (url.pathname === `/music/releases/${summary.slug}`) return route.fulfill({ json: release });
    if (url.pathname === '/music/playback-events') {
      events.push(route.request().postDataJSON()); return route.fulfill({ status: 202, json: { accepted: true } });
    }
    const id = url.pathname.match(/^\/music\/assets\/([^/]+)\/access$/)?.[1];
    if (id) {
      for (const i of [1, 2]) for (const [q, quality] of qualities.entries()) if (id === assetId(i, q)) {
        return route.fulfill({ json: { ...fixture.files[quality],
          url: `${fixture.endpoint}/media/${i === 1 ? 'first' : 'second'}/${quality}`,
          expiresAt: new Date(Date.now() + 600000).toISOString(), acceptRanges: true } });
      }
      return route.fulfill({ status: 404, json: { error: 'unknown synthetic asset' } });
    }
    if (url.port === '19099') return route.fulfill({ status: 404, json: { error: 'unconfigured synthetic API route' } });
    return route.continue();
  });
  // Observe native events only: do not patch prototypes, clocks or play/pause.
  await page.addInitScript(() => {
    globalThis.__nativeMediaEvents = [];
    for (const type of ['playing', 'pause', 'timeupdate', 'seeking', 'seeked', 'ended', 'error', 'loadedmetadata']) {
      document.addEventListener(type, (event) => {
        if (event.target instanceof HTMLAudioElement) globalThis.__nativeMediaEvents.push({
          type, trusted: event.isTrusted, time: event.target.currentTime,
          source: event.target.currentSrc, error: event.target.error?.code ?? null,
        });
      }, true);
    }
  });
  await page.goto('/musica', { waitUntil: 'domcontentloaded' });
  await page.getByRole('link', { name: /Audio nativo sintético/ }).click();
  await page.getByRole('button', { name: 'Reproducir release' }).click();
  const player = page.getByRole('region', { name: 'Reproductor global' });
  const audio = page.locator('audio');
  await expect(audio).toHaveCount(1);
  await expect.poll(() => audio.evaluate((a) => a.currentTime), { message: 'Native decoder must advance the real media clock', timeout: 15000 }).toBeGreaterThan(0.5);
  await expect(player.getByRole('button', { name: 'Pausar' })).toBeVisible();
  await audio.evaluate((a) => { globalThis.__originalNativeAudio = a; });
  return { player, audio, events, fixture, deniedNetwork };
}

async function seek(player, value) {
  // Pause while operating the control so slow automation cannot naturally
  // finish this track and accidentally seek a different queue item mid-gesture.
  // Dialog exit restores the accessibility tree asynchronously. Do not mistake
  // a still-modal background for a paused player and seek while it is playing.
  await expect(player).toBeVisible();
  const resume = await player.getByRole('button', { name: 'Pausar' }).isVisible();
  if (resume) await player.getByRole('button', { name: 'Pausar' }).click();
  const slider = player.getByRole('slider', { name: 'Posición de reproducción' });
  await slider.focus();
  await slider.press('Home');
  for (let i = 0; i < Math.floor(value / 10); i++) await slider.press('PageUp');
  for (let i = 0; i < value % 10; i++) await slider.press('ArrowRight');
  await expect(slider).toHaveValue(String(value));
  if (resume) await player.getByRole('button', { name: 'Reproducir', exact: true }).click();
}

async function openOptions(page, player) {
  await player.getByRole('button', { name: 'Opciones de reproducción' }).click();
  const options = page.getByRole('dialog', { name: 'Opciones de reproducción' });
  await expect(options).toBeVisible();
  return options;
}

test.afterEach(async ({ page }, testInfo) => {
  if (!page || page.isClosed()) return;
  const observations = await page.evaluate(() => ({
    events: globalThis.__nativeMediaEvents ?? [],
    audio: [...document.querySelectorAll('audio')].map((a) => ({
      src: a.currentSrc, time: a.currentTime, duration: a.duration, paused: a.paused,
      error: a.error?.code, readyState: a.readyState, aac: a.canPlayType('audio/mp4'), flac: a.canPlayType('audio/flac'),
    })),
  }));
  await testInfo.attach('native-audio-observations.json', { body: JSON.stringify(observations, null, 2), contentType: 'application/json' });
});

test('@critical PW-MUSIC-NATIVE-01 decodes real derivatives and keeps playback across navigation, seeking and quality', async ({ page }) => {
  const { player, audio, events } = await prepare(page);
  await expect.poll(() => audio.evaluate((a) => a.duration)).toBeGreaterThan(23.9);
  await page.goBack();
  await expect(page).toHaveURL(/\/musica$/);
  expect(await audio.evaluate((a) => a === globalThis.__originalNativeAudio)).toBe(true);
  const before = await audio.evaluate((a) => a.currentTime);
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThan(before + 0.5);
  await seek(player, 8);
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThanOrEqual(8);
  const options = await openOptions(page, player);
  await options.getByRole('combobox', { name: 'Calidad de audio' }).click();
  await page.getByRole('option', { name: 'Lossless' }).click();
  await expect.poll(() => audio.evaluate((a) => a.currentSrc)).toMatch(/\/lossless$/);
  await expect.poll(() => audio.evaluate((a) => !a.paused && a.currentTime >= 8)).toBe(true);
  await options.getByRole('button', { name: 'Cerrar opciones' }).click();
  await player.getByRole('button', { name: 'Pausar' }).click();
  await expect.poll(() => audio.evaluate((a) => a.paused)).toBe(true);
  await player.getByRole('button', { name: 'Reproducir', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => !a.paused && a.currentTime > 8)).toBe(true);
  await player.getByRole('button', { name: 'Pista siguiente' }).click();
  await expect(player.getByText('Pista nativa 2', { exact: true })).toBeVisible();
  await expect.poll(() => audio.evaluate((a) => a.currentSrc)).toMatch(/\/second\//);
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThan(0.5);
  const initialSecondPlayback = await page.evaluate(() => globalThis.__nativeMediaEvents.find(
    (e) => e.type === 'playing' && e.trusted && e.source.includes('/media/second/')));
  expect(initialSecondPlayback?.time, 'A new track must not inherit the previous track position').toBeLessThan(2);
  expect(await audio.evaluate((a) => a === globalThis.__originalNativeAudio && !a.error)).toBe(true);
  expect(await page.evaluate(() => globalThis.__nativeMediaEvents.filter((e) => e.type === 'playing' && e.trusted).length)).toBeGreaterThanOrEqual(3);
  expect(events.some((e) => e.eventType === 'play_start')).toBe(true);
});

test('@critical PW-MUSIC-NATIVE-02 native ended advances the queue and repetition actually restarts audio', async ({ page }) => {
  const { player, audio } = await prepare(page);
  await seek(player, 23);
  await expect(player.getByText('Pista nativa 2', { exact: true })).toBeVisible({ timeout: 15000 });
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThan(0.5);
  const options = await openOptions(page, player);
  await options.getByRole('combobox', { name: 'Repetición', exact: true }).click();
  await page.getByRole('option', { name: 'Repetir pista', exact: true }).click();
  await options.getByRole('button', { name: 'Cerrar opciones' }).click();
  await seek(player, 23);
  await expect.poll(() => page.evaluate(() => globalThis.__nativeMediaEvents.filter((e) => e.type === 'ended' && e.trusted).length)).toBeGreaterThanOrEqual(2);
  await expect.poll(() => audio.evaluate((a) => !a.paused && a.currentTime > 0 && a.currentTime < 10)).toBe(true);
  await expect(player.getByText('Pista nativa 2', { exact: true })).toBeVisible();
});

test('@critical PW-MUSIC-NATIVE-03 rapid switching during a pending native play keeps the latest source', async ({ page }) => {
  const { player, audio, fixture } = await prepare(page);
  let releaseRequest, requestStarted;
  const gate = new Promise((resolve) => { releaseRequest = resolve; });
  const started = new Promise((resolve) => { requestStarted = resolve; });
  // Delay only this request; do not replace media bytes, play() or its promise.
  await page.route(`${fixture.endpoint}/media/second/**`, async (route) => {
    requestStarted();
    await gate;
    await route.continue().catch(() => undefined); // superseded request can be aborted by the browser
  });
  try {
    await player.getByRole('button', { name: 'Pista siguiente' }).click();
    await started;
    await player.getByRole('button', { name: 'Pista anterior' }).click();
  } finally { releaseRequest(); }
  await expect(player.getByText('Pista nativa 1', { exact: true })).toBeVisible();
  await expect.poll(() => audio.evaluate((a) => a.currentSrc)).toMatch(/\/first\//);
  await expect.poll(() => audio.evaluate((a) => !a.paused && !a.error && a.currentTime > 0.5)).toBe(true);
  await expect(player.getByRole('button', { name: 'Pausar' })).toBeVisible();
  await expect(page.locator('audio')).toHaveCount(1);
});

test('@critical PW-MUSIC-NATIVE-04 compact options are keyboard accessible and control the same native engine', async ({ page }, testInfo) => {
  const { player, audio } = await prepare(page);
  await player.getByRole('button', { name: 'Pausar' }).click();
  await page.setViewportSize({ width: 320, height: 568 });
  const geometry = await player.getByRole('button').evaluateAll((buttons) => buttons
    .filter((button) => button.getClientRects().length > 0)
    .map((button) => { const r = button.getBoundingClientRect(); return { name: button.getAttribute('aria-label'), width: r.width, height: r.height, left: r.left, right: r.right }; }));
  expect(geometry).toHaveLength(5);
  for (const r of geometry) {
    expect(r.width, r.name).toBeGreaterThanOrEqual(44);
    expect(r.height, r.name).toBeGreaterThanOrEqual(44);
    expect(r.left, r.name).toBeGreaterThanOrEqual(0);
    expect(r.right, r.name).toBeLessThanOrEqual(320);
  }
  const opener = player.getByRole('button', { name: 'Opciones de reproducción' });
  await opener.focus();
  await opener.press('Space');
  const options = page.getByRole('dialog', { name: 'Opciones de reproducción' });
  await expect(options).toBeVisible();
  await expect(options.getByRole('button', { name: 'Cerrar opciones' })).toBeFocused();
  expect(await audio.evaluate((a) => a.paused)).toBe(true);
  await options.getByRole('combobox', { name: 'Calidad de audio' }).click();
  await page.getByRole('option', { name: 'Lossless' }).click();
  await expect.poll(() => audio.evaluate((a) => a.currentSrc)).toMatch(/\/lossless$/);
  await expect.poll(() => audio.evaluate((a) => a.readyState)).toBeGreaterThanOrEqual(1);
  // This bar is aria-hidden while the modal is open; inspect the status DOM
  // without treating the modal background as keyboard-accessible content.
  await expect(page.locator('section[aria-label="Reproductor global"] [role="status"]')).toHaveCount(0);
  for (const name of ['Repetir pista', 'Repetir cola', 'Repetición desactivada', 'Repetir lanzamiento']) {
    await options.getByRole('combobox', { name: 'Repetición', exact: true }).click();
    if (name === 'Repetir pista') {
      // Select's listbox is portalled outside the dialog. Unmatched typeahead
      // must not leak to the global M shortcut and silently mute the engine.
      await page.getByRole('option', { name: 'Repetición desactivada', exact: true }).press('m');
      expect(await audio.evaluate((a) => a.muted)).toBe(false);
    }
    await page.getByRole('option', { name, exact: true }).click();
    await expect(options.getByRole('combobox', { name: 'Repetición', exact: true })).toHaveText(name);
  }
  await options.getByRole('checkbox', { name: 'Reproducción aleatoria' }).check();
  const volume = options.getByRole('slider', { name: 'Volumen' });
  await volume.focus();
  await volume.press('Home');
  await volume.press('ArrowRight');
  await expect(volume).toHaveValue('0.01');
  await expect.poll(() => audio.evaluate((a) => a.volume)).toBeCloseTo(0.01, 3);
  await options.getByRole('button', { name: 'Silenciar', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => a.muted)).toBe(true);
  await options.getByRole('button', { name: 'Activar sonido', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => a.muted)).toBe(false);
  await page.addScriptTag({ content: axe.source });
  const violations = await options.evaluate(async (root) => (await globalThis.axe.run(root)).violations
    .filter((v) => ['critical', 'serious'].includes(v.impact)).map(({ id, impact, help }) => ({ id, impact, help })));
  await testInfo.attach('music-options-axe.json', { body: JSON.stringify(violations), contentType: 'application/json' });
  expect(violations).toEqual([]);
  if (testInfo.project.name === 'chromium-phone') await testInfo.attach('compact-options.png', {
    body: await page.screenshot(), contentType: 'image/png',
  });
  // Focus cycles within the modal, including backwards from its first control.
  await options.getByRole('button', { name: 'Cerrar opciones' }).focus();
  await page.keyboard.press('Shift+Tab');
  expect(await options.evaluate((root) => root.contains(document.activeElement))).toBe(true);
  await page.keyboard.press('Tab');
  await expect(options.getByRole('button', { name: 'Cerrar opciones' })).toBeFocused();
  await page.keyboard.press('Escape');
  await expect(options).toBeHidden();
  await expect(opener).toBeFocused();
  await expect.poll(() => page.evaluate(() => {
    const saved = JSON.parse(localStorage.getItem('tdf-global-player/v1') ?? '{}');
    return { quality: saved.quality, repeat: saved.queue?.repeat, shuffle: saved.queue?.shuffle, volume: saved.volume };
  })).toEqual({ quality: 'lossless', repeat: 'release', shuffle: true, volume: 0.01 });
  // Landscape keeps a scrollable, dismissible panel without a second engine.
  await page.setViewportSize({ width: 568, height: 320 });
  if (testInfo.project.use.hasTouch) await opener.tap(); else await opener.click();
  await expect(options).toBeVisible();
  await options.getByRole('slider', { name: 'Volumen' }).scrollIntoViewIfNeeded();
  await page.keyboard.press('Escape');
  await expect(options).toBeHidden();
  await expect(opener).toBeFocused();
  expect(await audio.evaluate((a) => a === globalThis.__originalNativeAudio && a.paused && !a.error)).toBe(true);
  await player.getByRole('button', { name: 'Reproducir', exact: true }).press('Space');
  await expect.poll(() => audio.evaluate((a) => !a.paused && a.currentTime > 0)).toBe(true);
  await expect(page.locator('audio')).toHaveCount(1);
});
