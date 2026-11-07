import { expect, test } from '@playwright/test';
import axe from 'axe-core';

test.setTimeout(60_000);

const releaseId = '10000000-0000-4000-8000-000000000001';
const versionId = '20000000-0000-4000-8000-000000000001';
const firstTrackId = '30000000-0000-4000-8000-000000000001';
const secondTrackId = '30000000-0000-4000-8000-000000000002';
const firstRecordingId = '40000000-0000-4000-8000-000000000001';
const secondRecordingId = '40000000-0000-4000-8000-000000000002';
const firstAssetId = '50000000-0000-4000-8000-000000000001';
const secondAssetId = '50000000-0000-4000-8000-000000000002';
const coverAssetId = '60000000-0000-4000-8000-000000000001';
const transparentGif = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==';

function buildSilentWavDataUrl() {
  const sampleRate = 8_000;
  const dataSize = sampleRate * 2;
  const wav = Buffer.alloc(44 + dataSize);
  wav.write('RIFF', 0);
  wav.writeUInt32LE(36 + dataSize, 4);
  wav.write('WAVEfmt ', 8);
  wav.writeUInt32LE(16, 16);
  wav.writeUInt16LE(1, 20);
  wav.writeUInt16LE(1, 22);
  wav.writeUInt32LE(sampleRate, 24);
  wav.writeUInt32LE(sampleRate * 2, 28);
  wav.writeUInt16LE(2, 32);
  wav.writeUInt16LE(16, 34);
  wav.write('data', 36);
  wav.writeUInt32LE(dataSize, 40);
  return `data:audio/wav;base64,${wav.toString('base64')}`;
}

const silentWav = buildSilentWavDataUrl();

const summary = {
  id: releaseId,
  artistPartyId: 101,
  slug: 'album-sintetico-player',
  kind: 'album',
  versionId,
  versionNumber: 1,
  title: 'Álbum sintético del player',
  displayArtist: 'Artista sintética TDF',
  releaseAtUtc: '2026-09-13T05:00:00Z',
  labelName: 'Sello sintético',
  publishedAt: '2026-09-13T05:00:01Z',
  coverAssetId,
};

const release = {
  ...summary,
  subtitle: null,
  versionTitle: null,
  explicitContent: 'not_explicit',
  originalReleaseDate: '2026-09-13',
  catalogNumber: 'SYN-PLAYER-001',
  tracks: [
    {
      trackId: firstTrackId,
      recordingId: firstRecordingId,
      discNumber: 1,
      trackNumber: 1,
      title: 'Primera pista sintética',
      displayArtist: 'Artista sintética TDF',
      durationMs: 90_000,
      explicitContent: 'not_explicit',
      sources: [{
        assetId: firstAssetId,
        role: 'stream_audio',
        mediaType: 'audio/wav',
        technicalMetadata: { bitrate_kbps: 256, loudness_lufs: -10 },
      }],
    },
    {
      trackId: secondTrackId,
      recordingId: secondRecordingId,
      discNumber: 1,
      trackNumber: 2,
      title: 'Segunda pista sintética',
      displayArtist: 'Artista sintética TDF',
      durationMs: 120_000,
      explicitContent: 'not_explicit',
      sources: [{
        assetId: secondAssetId,
        role: 'stream_audio',
        mediaType: 'audio/wav',
        technicalMetadata: { bitrate_kbps: 128, loudness_lufs: -16 },
      }],
    },
  ],
  coverAssets: [{
    assetId: coverAssetId,
    role: 'cover_display',
    mediaType: 'image/gif',
    technicalMetadata: { width: 1, height: 1 },
  }],
  availability: [{
    ruleId: '70000000-0000-4000-8000-000000000001',
    trackId: null,
    territoryMode: 'include',
    territories: ['EC'],
    startsAt: '2026-09-13T05:00:00Z',
    endsAt: null,
    listeningPolicy: 'full',
    downloadPolicy: 'none',
    purchasable: false,
    priceMinor: null,
    currency: null,
  }],
};

async function installDeterministicMediaEngine(page) {
  await page.addInitScript(() => {
    const state = new WeakMap();
    const stateFor = (element) => {
      const current = state.get(element) ?? { paused: true, currentTime: 0, duration: 180 };
      state.set(element, current);
      return current;
    };
    Object.defineProperties(HTMLMediaElement.prototype, {
      paused: { configurable: true, get() { return stateFor(this).paused; } },
      currentTime: {
        configurable: true,
        get() { return stateFor(this).currentTime; },
        set(value) { stateFor(this).currentTime = Number.isFinite(value) ? value : 0; },
      },
      duration: { configurable: true, get() { return stateFor(this).duration; } },
    });
    HTMLMediaElement.prototype.load = function load() {
      queueMicrotask(() => {
        this.dispatchEvent(new Event('durationchange'));
      });
    };
    HTMLMediaElement.prototype.play = function play() {
      stateFor(this).paused = false;
      queueMicrotask(() => this.dispatchEvent(new Event('playing')));
      return Promise.resolve();
    };
    HTMLMediaElement.prototype.pause = function pause() {
      const mediaState = stateFor(this);
      const wasPaused = mediaState.paused;
      mediaState.paused = true;
      if (!wasPaused) queueMicrotask(() => this.dispatchEvent(new Event('pause')));
    };
  });
}

async function installSyntheticMusicApi(page, analyticsEvents) {
  await page.route('**/session', (route) => route.fulfill({
    status: 401,
    contentType: 'application/json',
    body: '{"error":"unauthenticated"}',
  }));
  await page.route('**/fans/artists', (route) => route.fulfill({ json: [] }));
  await page.route('**/catalogs/batch?*', (route) => route.fulfill({ json: {} }));
  await page.route('**/music/**', async (route) => {
    const url = new URL(route.request().url());
    if (url.pathname === '/music/releases') return route.fulfill({ json: [summary] });
    if (url.pathname === `/music/releases/${release.slug}`) return route.fulfill({ json: release });
    if (url.pathname === '/music/playback-events') {
      analyticsEvents.push(route.request().postDataJSON());
      return route.fulfill({ status: 202, json: { accepted: true } });
    }
    const assetId = url.pathname.match(/^\/music\/assets\/([^/]+)\/access$/)?.[1];
    if (assetId) {
      const isCover = assetId === coverAssetId;
      return route.fulfill({
        json: {
          url: isCover ? transparentGif : silentWav,
          expiresAt: '2030-01-01T00:00:00Z',
          mediaType: isCover ? 'image/gif' : 'audio/wav',
          byteSize: isCover ? 43 : 16_044,
          sha256: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
          acceptRanges: !isCover,
        },
      });
    }
    return route.fulfill({ status: 404, json: { error: `Synthetic music route not configured: ${url.pathname}` } });
  });
}

async function expectAccessiblePlayer(page, testInfo) {
  await page.addScriptTag({ content: axe.source });
  const violations = await page.getByRole('region', { name: 'Reproductor global' }).evaluate(async (root) => {
    const result = await globalThis.axe.run(root, { resultTypes: ['violations'] });
    return result.violations
      .filter((violation) => violation.impact === 'critical' || violation.impact === 'serious')
      .map(({ id, impact, help, nodes }) => ({ id, impact, help, nodes: nodes.map((node) => node.target) }));
  });
  await testInfo.attach('music-player-axe.json', {
    body: JSON.stringify(violations, null, 2),
    contentType: 'application/json',
  });
  expect(violations).toEqual([]);
}

test('@critical PW-MUSIC-01 keeps one accessible global player alive across client-side navigation', async ({ page }, testInfo) => {
  const analyticsEvents = [];
  await page.route('**/*', (route) => {
    const url = new URL(route.request().url());
    return ['http:', 'https:'].includes(url.protocol) && !['127.0.0.1', 'localhost'].includes(url.hostname)
      ? route.abort('blockedbyclient') : route.continue();
  });
  await installDeterministicMediaEngine(page);
  await installSyntheticMusicApi(page, analyticsEvents);

  await page.goto('/musica', { waitUntil: 'domcontentloaded' });
  await expect(page.getByRole('heading', { name: 'Música en TDF' })).toBeVisible();
  await page.getByRole('link', { name: /Álbum sintético del player/ }).click();
  await expect(page).toHaveURL(/\/musica\/album-sintetico-player$/);
  await expect(page.getByRole('heading', { name: 'Álbum sintético del player' })).toBeVisible();

  await page.getByRole('button', { name: 'Reproducir release' }).click();
  const player = page.getByRole('region', { name: 'Reproductor global' });
  await expect(player).toBeVisible();
  await expect(player.getByText('Primera pista sintética')).toBeVisible();
  await expect(player.getByRole('button', { name: 'Pausar' })).toBeVisible();
  await expect(page.locator('audio')).toHaveCount(1);
  await page.locator('audio').evaluate((audio) => { globalThis.__tdfMusicPlayerAudio = audio; });

  await player.getByRole('button', { name: 'Pista siguiente' }).click();
  await expect(player.getByText('Segunda pista sintética')).toBeVisible();
  await player.getByRole('button', { name: 'Abrir cola, 2 pistas' }).click();
  const queue = page.getByRole('dialog', { name: 'Cola de reproducción' });
  await expect(queue.getByText('Primera pista sintética')).toBeVisible();
  await expect(queue.getByText('Segunda pista sintética')).toBeVisible();
  await queue.getByRole('button', { name: 'Cerrar cola' }).click();

  const supportsWideControls = !testInfo.project.name.includes('phone') && !testInfo.project.name.includes('tablet');
  if (supportsWideControls) {
    await player.getByRole('button', { name: 'Activar reproducción aleatoria' }).click();
    await expect(player.getByRole('button', { name: 'Desactivar reproducción aleatoria' })).toBeVisible();
    await player.getByRole('button', { name: 'Desactivar reproducción aleatoria' }).click();
    await player.getByRole('button', { name: 'Repetición desactivada' }).click();
    await expect(player.getByRole('button', { name: 'Repetir pista' })).toBeVisible();
    await player.getByRole('button', { name: 'Repetir pista' }).click();
    await player.getByRole('button', { name: 'Repetir cola' }).click();
    await player.getByRole('button', { name: 'Repetir lanzamiento' }).click();
    await expect(player.getByRole('button', { name: 'Repetición desactivada' })).toBeVisible();
    await player.getByRole('combobox', { name: 'Calidad de audio' }).click();
    await page.getByRole('option', { name: 'Alta' }).click();
    await expect(player.getByRole('combobox', { name: 'Calidad de audio' })).toHaveText('Alta');
  }

  await page.locator('main#main-content').focus();
  await page.keyboard.press('Space');
  await expect(player.getByRole('button', { name: 'Reproducir' })).toBeVisible();
  await page.keyboard.press('Space');
  await expect(player.getByRole('button', { name: 'Pausar' })).toBeVisible();
  await page.keyboard.press('m');
  if (supportsWideControls) {
    await expect(player.getByRole('button', { name: 'Activar sonido' })).toBeVisible();
  } else {
    await player.getByRole('button', { name: 'Opciones de reproducción' }).click();
    const options = page.getByRole('dialog', { name: 'Opciones de reproducción' });
    await expect(options.getByRole('button', { name: 'Activar sonido' })).toBeVisible();
    await options.getByRole('button', { name: 'Cerrar opciones' }).click();
    await page.locator('main#main-content').focus();
  }
  await page.keyboard.press('Alt+ArrowLeft');
  await expect(player.getByText('Primera pista sintética')).toBeVisible();

  await expectAccessiblePlayer(page, testInfo);
  await page.goBack();
  await expect(page).toHaveURL(/\/musica$/);
  await expect(page.getByRole('heading', { name: 'Música en TDF' })).toBeVisible();
  await expect(player).toBeVisible();
  await expect(player.getByText('Primera pista sintética')).toBeVisible();
  await expect(page.locator('audio')).toHaveCount(1);
  expect(await page.locator('audio').evaluate((audio) => audio === globalThis.__tdfMusicPlayerAudio)).toBe(true);
  expect(analyticsEvents.filter((event) => event?.eventType === 'play_start').length).toBeGreaterThanOrEqual(1);
});
