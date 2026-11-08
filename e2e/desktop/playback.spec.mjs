import { expect, test } from '@playwright/test';
import { clickPlaybackMenu, launchShell, playbackMenu } from './helpers/shell.mjs';
import { RELEASE, STAFF_ACCOUNT, createApiFixture } from './helpers/api-fixture.mjs';

// Flow 2: the shell's native playback controls and the Media Session act on
// the same playback session as the in-page player, for songs and radio, with
// explicit pause, artist changes and a failing source. Audio is real Chromium
// decoding of generated PCM; API is synthetic.

async function signIn(window) {
  await window.getByLabel('Usuario o correo').fill('staff@example.invalid');
  await window.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
  await window.getByRole('button', { name: 'Ingresar', exact: true }).click();
  await expect(window).not.toHaveURL(/\/login/);
}

const navigate = (window, path) => window.evaluate((target) => { window.history.pushState({}, '', target); window.dispatchEvent(new PopStateEvent('popstate')); }, path);
const mediaSession = (window) => window.evaluate(() => ({
  title: navigator.mediaSession.metadata?.title ?? null,
  artist: navigator.mediaSession.metadata?.artist ?? null,
  state: navigator.mediaSession.playbackState,
}));

test('system media controls follow songs, radio, explicit pause, artist changes and failing content', async ({ baseURL }) => {
  test.setTimeout(150_000);
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    const { window, app } = shell;
    await shell.openLogin();
    await signIn(window);
    expect(await playbackMenu(app)).toEqual([
      { label: 'Nada en reproducción', enabled: false },
      { label: 'Reproducir', enabled: false }, { label: 'Siguiente', enabled: false }, { label: 'Anterior', enabled: false },
    ]);

    await navigate(window, `/musica/${RELEASE.slug}`);
    await window.getByRole('button', { name: 'Reproducir release' }).click();
    const player = window.getByRole('region', { name: 'Reproductor global' });
    await expect(player.getByRole('button', { name: 'Pausar' })).toBeVisible({ timeout: 30_000 });
    const musicAudio = window.locator('audio[preload="metadata"]');
    await expect.poll(() => musicAudio.evaluate((element) => !element.paused)).toBe(true);
    await expect.poll(() => mediaSession(window)).toEqual({ title: 'Primera pista sintética', artist: 'Artista sintética TDF', state: 'playing' });
    await expect.poll(() => playbackMenu(app)).toEqual([
      { label: 'Primera pista sintética · Artista sintética TDF', enabled: false },
      { label: 'Pausar', enabled: true }, { label: 'Siguiente', enabled: true }, { label: 'Anterior', enabled: true },
    ]);

    // Explicit pause from the native menu reaches the same audio element.
    expect(await clickPlaybackMenu(app, 'Pausar')).toBe(true);
    await expect.poll(() => musicAudio.evaluate((element) => element.paused)).toBe(true);
    await expect(player.getByRole('button', { name: 'Reproducir' })).toBeVisible();
    await expect.poll(async () => (await playbackMenu(app))[1].label).toBe('Reproducir');
    await expect.poll(async () => (await mediaSession(window)).state).toBe('paused');

    // Artist change through the native "next" command.
    expect(await clickPlaybackMenu(app, 'Siguiente')).toBe(true);
    await expect.poll(async () => (await playbackMenu(app))[0].label).toBe('Segunda pista sintética · Otra artista sintética');
    await expect.poll(() => mediaSession(window)).toEqual({ title: 'Segunda pista sintética', artist: 'Otra artista sintética', state: 'playing' });

    // Failing content: the third source is refused by the server; the player
    // reports it and the native controls still move to a valid track.
    expect(await clickPlaybackMenu(app, 'Siguiente')).toBe(true);
    await expect(window.getByText('El servidor no autorizó una fuente vigente para esta pista.')).toBeVisible({ timeout: 30_000 });
    await expect.poll(async () => (await playbackMenu(app))[0].label).toBe('Pista con fuente caída · Artista sintética TDF');
    expect(await clickPlaybackMenu(app, 'Anterior')).toBe(true);
    await expect.poll(() => mediaSession(window)).toEqual({ title: 'Segunda pista sintética', artist: 'Otra artista sintética', state: 'playing' });
    await expect.poll(() => musicAudio.evaluate((element) => !element.paused)).toBe(true);

    // Radio takes over the single playback session; the release player pauses.
    await navigate(window, '/inicio#radio');
    const bar = window.getByTestId('radio-docked-bar');
    await expect(bar).toContainText('Emisora sintética', { timeout: 30_000 });
    await bar.getByRole('button', { name: 'Reproducir radio', exact: true }).click();
    const radioAudio = window.locator('audio[src$="/fixture-radio.wav"]');
    await expect.poll(() => radioAudio.evaluate((element) => !element.paused && element.currentTime > 0.1)).toBe(true);
    await expect.poll(() => musicAudio.evaluate((element) => element.paused)).toBe(true);
    await expect.poll(() => playbackMenu(app)).toEqual([
      { label: 'Emisora sintética · Radio TDF', enabled: false },
      { label: 'Pausar', enabled: true }, { label: 'Siguiente', enabled: true }, { label: 'Anterior', enabled: true },
    ]);
    await expect.poll(() => mediaSession(window)).toEqual({ title: 'Emisora sintética', artist: 'Radio TDF', state: 'playing' });

    // Native pause/resume act on the radio, not on the paused release player.
    expect(await clickPlaybackMenu(app, 'Pausar')).toBe(true);
    await expect(bar.getByRole('button', { name: 'Reproducir radio', exact: true })).toBeVisible();
    await expect.poll(() => radioAudio.evaluate((element) => element.paused)).toBe(true);
    expect(await musicAudio.evaluate((element) => element.paused)).toBe(true);
    expect(await clickPlaybackMenu(app, 'Reproducir')).toBe(true);
    await expect(bar.getByRole('button', { name: 'Pausar radio', exact: true })).toBeVisible();
    await expect.poll(() => radioAudio.evaluate((element) => !element.paused)).toBe(true);
    expect(await musicAudio.evaluate((element) => element.paused)).toBe(true);
  } finally { await shell.close(); }
});
