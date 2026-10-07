import { expect, test } from '@playwright/test';

const api = process.env.TDF_MUSIC_BROWSER_API;
const storage = process.env.TDF_MUSIC_BROWSER_STORAGE;
const releasePath = '/musica/single-musica-e2e';
const observations = new WeakMap();

test.beforeEach(async ({ context, page, baseURL }) => {
  const network = [];
  observations.set(page, network);
  // No request bodies, cookies, signed query strings or bearer tokens in reports.
  page.on('response', (res) => {
    const url = new URL(res.url());
    if (url.origin === api || url.origin === storage) network.push({ origin: url.origin, path: url.pathname, status: res.status() });
  });
  page.on('requestfailed', (req) => {
    const url = new URL(req.url());
    network.push({ origin: url.origin, path: url.pathname, failure: req.failure()?.errorText });
  });
  const allowed = new Set([baseURL, api, storage]);
  await context.route('**/*', (route) => {
    const url = new URL(route.request().url());
    return allowed.has(url.origin) ? route.continue() : route.abort('blockedbyclient');
  });
});

test.afterEach(async ({ page }, testInfo) => {
  await testInfo.attach('real-network-observations.json', { body: JSON.stringify(observations.get(page) ?? [], null, 2), contentType: 'application/json' });
  const audio = await page.locator('audio').evaluateAll((nodes) => nodes.map((node) => ({
    source: node.src ? new URL(node.src).pathname : null,
    currentSource: node.currentSrc ? new URL(node.currentSrc).pathname : null,
    time: node.currentTime, duration: node.duration, paused: node.paused,
    readyState: node.readyState, error: node.error?.code ?? null,
  })));
  await testInfo.attach('real-audio-state.json', { body: JSON.stringify(audio, null, 2), contentType: 'application/json' });
});

async function playReal(page) {
  const access = page.waitForResponse((res) => res.url().startsWith(`${api}/music/assets/`) && res.status() === 200);
  await page.getByRole('button', { name: 'Reproducir release', exact: true }).click();
  await access;
  const audio = page.locator('audio');
  await expect(audio).toHaveCount(1);
  await expect.poll(() => audio.evaluate((a) => a.currentTime), { timeout: 15000 }).toBeGreaterThan(0.5);
  await expect.poll(() => audio.evaluate((a) => a.duration)).toBeGreaterThan(61);
  expect(await audio.evaluate((a) => a.currentSrc)).toMatch(new RegExp(`^${storage}/music-e2e-derivative/`));
  return audio;
}

test('@critical PW-MUSIC-REAL-01 visitor decodes authorized S3 bytes across real catalogue navigation', async ({ page }) => {
  const mediaResponses = [];
  page.on('response', (res) => {
    if (res.url().startsWith(storage)) mediaResponses.push({ status: res.status(), path: new URL(res.url()).pathname });
  });
  await page.goto('/musica');
  await page.getByRole('link', { name: /Single Música E2E/ }).click();
  const cover = page.getByRole('img', { name: 'Portada de Single Música E2E' });
  await expect.poll(() => cover.evaluate((img) => img.naturalWidth)).toBeGreaterThan(0);
  const audio = await playReal(page);
  const before = await audio.evaluate((a) => { globalThis.__realAudio = a; return a.currentTime; });
  await page.goBack();
  await expect(page).toHaveURL(/\/musica$/);
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThan(before + 0.5);
  expect(await audio.evaluate((a) => a === globalThis.__realAudio)).toBe(true);
  await page.getByRole('region', { name: 'Reproductor global' }).getByRole('button', { name: 'Pausar', exact: true }).click();
  await page.getByRole('button', { name: 'Opciones de reproducción', exact: true }).click();
  const options = page.getByRole('dialog', { name: 'Opciones de reproducción' });
  await options.getByRole('combobox', { name: 'Calidad de audio', exact: true }).click();
  await page.getByRole('option', { name: 'Baja', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => new URL(a.currentSrc).pathname)).toMatch(/stream-low\.m4a$/);
  await expect.poll(() => audio.evaluate((a) => a.duration)).toBeGreaterThan(61);
  await options.getByRole('combobox', { name: 'Calidad de audio', exact: true }).click();
  await page.getByRole('option', { name: 'Lossless', exact: true }).click();
  await options.getByRole('button', { name: 'Cerrar opciones', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => new URL(a.currentSrc).pathname)).toMatch(/stream-lossless\.flac$/);
  const pausedPosition = await audio.evaluate((a) => a.currentTime);
  await page.getByRole('region', { name: 'Reproductor global' }).getByRole('button', { name: 'Reproducir', exact: true }).click();
  await expect.poll(() => audio.evaluate((a) => a.currentTime)).toBeGreaterThan(pausedPosition + 0.5);
  const denied = await page.evaluate(async ({ api, master }) => {
    const res = await fetch(`${api}/music/assets/${master}/access`); return res.status;
  }, { api, master: process.env.TDF_MUSIC_BROWSER_MASTER });
  expect(denied).toBe(404);
  expect(mediaResponses.some((res) => res.status === 206)).toBe(true);
  expect(mediaResponses.every((res) => res.path.startsWith('/music-e2e-derivative/'))).toBe(true);
});

async function loginFromRelease(page) {
  await page.goto(releasePath);
  await page.getByRole('button', { name: /^Guardar .* en favoritos$/ }).click();
  await expect(page).toHaveURL(/\/login\?redirect=/);
  await page.getByLabel('Usuario o correo *', { exact: true }).fill('music.artist@persona.test');
  await page.getByLabel('Contraseña *', { exact: true }).fill(process.env.TDF_MUSIC_BROWSER_PASSWORD);
  const login = page.waitForResponse((res) => res.url() === `${api}/login` && res.request().method() === 'POST');
  await page.getByRole('button', { name: 'Ingresar', exact: true }).click();
  expect((await login).status()).toBe(200);
  await page.waitForURL(`**${releasePath}`);
  await expect(page).toHaveURL(new RegExp(`${releasePath}$`));
}

async function openLibrary(page) {
  await page.getByRole('link', { name: 'Abrir mi biblioteca' }).click();
  await page.waitForURL('**/musica/biblioteca');
  // The route URL can change before its lazy module has mounted. Keep this
  // readiness phase within the case budget, then apply the 8s UI assertions.
  await page.getByRole('heading', { name: 'Mi biblioteca musical', exact: true }).waitFor();
}

test('@critical PW-MUSIC-REAL-02 real login saves favorites, playlist and playback history in PostgreSQL', async ({ page }, testInfo) => {
  await loginFromRelease(page);
  const corsStatus = await page.evaluate(async (api) => (await fetch(`${api}/music/releases`, {
    credentials: 'include', headers: { 'Idempotency-Key': 'browser-cors-probe' },
  })).status, api);
  expect(corsStatus).toBe(200);
  // A prior failed project may have left this shared synthetic user's favorite.
  // Normalize via the visible controls after the server snapshot, not a DB edit.
  const toggle = page.getByRole('button', { name: /^(Guardar|Quitar) .* favoritos$/ });
  await expect(toggle).toBeEnabled();
  if ((await toggle.getAttribute('aria-label')).startsWith('Quitar')) await toggle.click();
  const favorite = page.getByRole('button', { name: /^Guardar .* en favoritos$/ });
  await expect(favorite).toBeEnabled();
  await favorite.click();
  await expect(page.getByRole('button', { name: /^Quitar .* de favoritos$/ })).toBeVisible();
  const event = page.waitForResponse((res) => res.url() === `${api}/music/me/playback-events` && res.status() === 200);
  await playReal(page); await event;
  await openLibrary(page);
  await expect(page.getByRole('button', { name: 'Reproducir favoritos' })).toBeEnabled();
  await expect(page.getByRole('button', { name: 'Continuar', exact: true })).toBeVisible();
  const name = `Browser ${testInfo.project.name}`;
  await page.getByLabel('Nueva playlist', { exact: true }).fill(name);
  await page.getByRole('button', { name: 'Crear', exact: true }).click();
  await expect(page.getByText(name, { exact: true })).toBeVisible();
  await page.getByRole('link', { name: 'Explorar catálogo' }).click();
  await page.getByRole('link', { name: /Single Música E2E/ }).click();
  await page.getByRole('button', { name: /^Añadir .* a una playlist$/ }).click();
  await page.getByRole('button', { name: `${name} · 0 pistas`, exact: true }).click();
  await openLibrary(page);
  const playlistSummary = page.getByText(name, { exact: true }).locator('..').getByText('private · 1 pistas', { exact: true });
  await expect(playlistSummary).toBeVisible();
  // Reload proves persistence beyond component/query state and restores the
  // real cookie session; no injected localStorage/session token.
  const restoredSession = page.waitForResponse((res) => res.url() === `${api}/session`);
  const restoredLibrary = page.waitForResponse((res) => res.url() === `${api}/music/playlists` && res.request().method() === 'GET');
  await page.reload();
  expect((await restoredSession).status()).toBe(200);
  const restoredResponse = await restoredLibrary;
  expect(restoredResponse.status()).toBe(200);
  expect((await restoredResponse.json()).find((playlist) => playlist.name === name)?.items).toHaveLength(1);
  await expect(playlistSummary).toBeVisible();
  await expect(page.getByRole('button', { name: 'Reproducir favoritos' })).toBeEnabled();
  await page.getByRole('button', { name: /^Quitar .* de favoritos$/ }).click();
  page.once('dialog', (dialog) => dialog.accept());
  await page.getByRole('button', { name: `Eliminar playlist ${name}` }).click();
  await expect(page.getByText(name, { exact: true })).toHaveCount(0);
});

test('@critical PW-MUSIC-REAL-03 artist creates a private draft and recovers a rejected autosave without submitting stale data', async ({ page }, testInfo) => {
  await loginFromRelease(page);
  await page.goto('/label/releases/nuevo');
  const title = `Borrador navegador ${testInfo.project.name}`;
  await page.getByRole('textbox', { name: /^Título/ }).fill(title);
  const createdResponse = page.waitForResponse((res) => res.url() === `${api}/music/releases` && res.request().method() === 'POST');
  await page.getByRole('button', { name: 'Crear borrador', exact: true }).click();
  const response = await createdResponse;
  expect(response.status()).toBe(201);
  const created = await response.json();
  expect(created.kind).toBe('single');
  expect(created.state).toBe('draft');
  expect(created.slug).toBe(`borrador-navegador-${testInfo.project.name}`);
  const versionUrl = `${api}/music/releases/${created.id}/versions/${created.versionId}`;
  await expect(page).toHaveURL(new RegExp(`/versions/${created.versionId}`));
  const attempts = [];
  page.on('request', (request) => {
    if (request.url().startsWith(versionUrl)) attempts.push(new URL(request.url()).pathname.split('/').at(-1));
  });
  const trackTitle = page.getByRole('textbox', { name: 'Título', exact: true }).nth(1);
  await trackTitle.fill('Pista sintética pendiente');
  // No authority declarations yet: the actual API rejects content after
  // metadata committed. Submission must stop here, not transition stale data.
  const rejectedSave = page.waitForResponse((res) => res.url() === `${versionUrl}/content` && res.status() === 400);
  await page.getByRole('button', { name: 'Enviar a revisión', exact: true }).click();
  await rejectedSave;
  await expect(page.getByRole('alert').filter({ hasText: 'authorityBasis' })).toBeVisible();
  expect(attempts).not.toContain('transition');
  expect(attempts).not.toContain('terms');
  await expect(trackTitle).toHaveValue('Pista sintética pendiente');
  await page.getByRole('textbox', { name: 'Base de autoridad', exact: true }).nth(0).fill('Autoría del audio sintético de prueba');
  const saved = page.waitForResponse((res) => res.url() === `${versionUrl}/content` && res.status() === 200);
  await page.getByRole('textbox', { name: 'Base de autoridad', exact: true }).nth(1).fill('Composición sintética propia de prueba');
  await saved; // Automatic save, no alternate API writes or DB intervention.
  await expect(page.getByRole('status').filter({ hasText: 'Borrador guardado automáticamente.' })).toBeVisible();
  const persisted = await page.evaluate(async (url) => {
    const res = await fetch(url, { credentials: 'include' });
    return { status: res.status, body: await res.json() };
  }, versionUrl);
  expect(persisted.status).toBe(200);
  expect(persisted.body.tracks).toHaveLength(1);
  expect(persisted.body.tracks[0].title).toBe('Pista sintética pendiente');
  expect(persisted.body.rights.map((item) => item.rights_scope).sort()).toEqual(['composition', 'master']);
  // A collaborator needs neither an account nor a credit/split to remain in
  // an unfinished draft. Save through Studio and reload the real API graph.
  await page.getByRole('button', { name: 'Añadir colaborador', exact: true }).click();
  await page.getByRole('textbox', { name: 'Nombre visible', exact: true }).nth(1).fill('Colaborador sin cuenta ni crédito');
  const collaboratorSaved = page.waitForResponse((res) =>
    res.url() === `${versionUrl}/content` && res.request().method() === 'PUT' && res.status() === 200);
  await page.getByRole('button', { name: 'Guardar ahora', exact: true }).click();
  const collaboratorGraph = await (await collaboratorSaved).json();
  expect(collaboratorGraph.parties).toHaveLength(2);
  const collaborator = collaboratorGraph.parties.find((party) => party.displayName === 'Colaborador sin cuenta ni crédito');
  expect(collaborator?.tdfPartyId).toBeNull();
  expect(collaborator?.id).toMatch(/^[0-9a-f-]{36}$/);
  expect(collaboratorGraph.credits.some((credit) => credit.music_party_id === collaborator.id)).toBe(false);
  expect(collaboratorGraph.rights.some((rights) => rights.splits.some((split) => split.rights_holder_id === collaborator.id))).toBe(false);
  const collaboratorIndex = collaboratorGraph.parties.findIndex((party) => party.id === collaborator.id);
  await page.getByRole('textbox', { name: 'Nombre visible', exact: true }).nth(collaboratorIndex).fill('Colaborador corregido en borrador');
  await page.getByRole('textbox', { name: 'Nombre legal', exact: true }).nth(collaboratorIndex).fill('Nombre legal sintético');
  const renamed = page.waitForResponse((res) => res.url() === `${versionUrl}/content` && res.status() === 200);
  await page.getByRole('button', { name: 'Guardar ahora', exact: true }).click();
  const renamedParty = (await (await renamed).json()).parties.find((party) => party.id === collaborator.id);
  expect(renamedParty.displayName).toBe('Colaborador corregido en borrador');
  expect(renamedParty.legalName).toBe('Nombre legal sintético');
  const restoredVersion = page.waitForResponse((res) => res.url() === versionUrl && res.request().method() === 'GET');
  await page.reload();
  const restoredVersionResponse = await restoredVersion;
  expect(restoredVersionResponse.status()).toBe(200);
  expect((await restoredVersionResponse.json()).parties.find((party) => party.id === collaborator.id)?.displayName)
    .toBe('Colaborador corregido en borrador');
  await expect(page.getByRole('textbox', { name: 'Título', exact: true }).nth(1)).toHaveValue('Pista sintética pendiente');
  await expect(page.getByRole('textbox', { name: 'Base de autoridad', exact: true }).nth(0)).not.toHaveValue('');
  await expect(page.getByRole('textbox', { name: 'Nombre visible', exact: true })).toHaveCount(2);
  expect(await page.getByRole('textbox', { name: 'Nombre visible', exact: true }).evaluateAll((inputs) => inputs.map((input) => input.value)))
    .toContain('Colaborador corregido en borrador');
  expect(await page.getByRole('textbox', { name: 'Nombre legal', exact: true }).evaluateAll((inputs) => inputs.map((input) => input.value)))
    .toContain('Nombre legal sintético');
  const publicStatus = await page.evaluate(async ({ api, slug }) =>
    (await fetch(`${api}/music/releases/${slug}`)).status, { api, slug: created.slug });
  expect(publicStatus).toBe(404);
  // Draft is intentionally incomplete (no assets); disposable DB teardown
  // removes it. This case does not claim upload/review/publication by UI.
});
