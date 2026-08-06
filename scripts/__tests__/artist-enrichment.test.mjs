import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import { mkdtemp, readFile, rm } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { promisify } from 'node:util';

import {
  apiRequestTimeoutMs,
  automaticMatchAllowed,
  artistNameAliasCandidate,
  detectImageMime,
  meaningfulSignals,
  normalizeName,
  parseArgs,
  probeImage,
  reportableLinkUrl,
  retryDelayMs,
  retryFetch,
  runPipeline,
  transcodeWithinBudget,
  uploadTdfDriveFile,
} from '../artist-enrichment.mjs';

const execFileAsync = promisify(execFile);

test('normaliza capitalización, acentos y espacios para comparar sin alterar el nombre guardado', () => {
  assert.equal(normalizeName('  Diego   SAÁ '), 'diego saa');
  assert.equal(normalizeName('Jóssëfina'), 'jossefina');
});

test('exige dos señales independientes y rechaza homónimos', () => {
  assert.equal(automaticMatchAllowed(['country'], 1), false);
  assert.equal(automaticMatchAllowed(['country', 'discography_cross_provider'], 1), true);
  assert.equal(automaticMatchAllowed(['country', 'discography_cross_provider'], 2), false);
  assert.deepEqual(meaningfulSignals(['country', 'country', '', null]), ['country']);
});

test('bloquea variantes probables para revisión antes de crear otro perfil', () => {
  assert.equal(artistNameAliasCandidate('Skankafe', 'Skanka Fe'), true);
  assert.equal(artistNameAliasCandidate('E Quimika Soul', 'Quimika Soul'), true);
  assert.equal(artistNameAliasCandidate('Juano Ledesma (duplicado)', 'Juano Ledesma'), true);
  assert.equal(artistNameAliasCandidate('Lord Ethnic', 'Lord Invader'), false);
});

test('calcula backoff exponencial acotado', () => {
  assert.equal(retryDelayMs(0), 500);
  assert.equal(retryDelayMs(3), 4000);
  assert.equal(retryDelayMs(20), 30000);
});

test('permite ampliar de forma segura el timeout de la API de TDF', () => {
  assert.equal(apiRequestTimeoutMs(), 180_000);
  assert.equal(apiRequestTimeoutMs('300000'), 300_000);
  assert.throws(() => apiRequestTimeoutMs('999'), /between 1000 and 900000/);
  assert.throws(() => apiRequestTimeoutMs('not-a-number'), /between 1000 and 900000/);
});

test('redacta imágenes inline de los reportes sin alterar enlaces externos', () => {
  assert.equal(reportableLinkUrl('data:image/png;base64,secret-bytes'), '[inline-data-url-redacted]');
  assert.equal(reportableLinkUrl('https://official.example/artist.webp'), 'https://official.example/artist.webp');
});

test('bloquea ingestión de imágenes sin derechos y atribución explícitos', () => {
  assert.throws(
    () => parseArgs(['--artist', '7', '--image-source-url', 'https://official.example/artist.jpg']),
    /image-rights/,
  );
  const options = parseArgs([
    '--mode', 'production', '--scope', 'media', '--artist', '7',
    '--image-source-url', 'https://official.example/artist.jpg',
    '--image-rights', 'licensed', '--image-attribution', 'Official press kit',
  ]);
  assert.equal(options.imageRights, 'licensed');
  assert.equal(options.imageAttribution, 'Official press kit');
});

test('reintenta límites de proveedor y se recupera sin duplicar la solicitud exitosa', async () => {
  const previousFetch = globalThis.fetch;
  let calls = 0;
  globalThis.fetch = async () => {
    calls += 1;
    return calls === 1
      ? new Response('{}', { status: 429 })
      : new Response('{"ok":true}', { status: 200 });
  };
  try {
    const response = await retryFetch('https://provider.test/resource', {}, { attempts: 2, timeoutMs: 1000 });
    assert.equal(response.status, 200);
    assert.equal(calls, 2);
  } finally {
    globalThis.fetch = previousFetch;
  }
});

test('propaga fallos del proxy de Google Drive sin incluir el token en el error', async () => {
  const previousFetch = globalThis.fetch;
  globalThis.fetch = async () => new Response('{"error":"drive unavailable"}', { status: 502 });
  try {
    await assert.rejects(
      uploadTdfDriveFile(
        'https://tdf.test',
        'secret-admin-token',
        'artist.webp',
        'image/webp',
        Buffer.from('image'),
        'a'.repeat(64),
      ),
      (error) => error instanceof Error
        && error.message.includes('502')
        && !error.message.includes('secret-admin-token'),
    );
  } finally {
    globalThis.fetch = previousFetch;
  }
});

test('valida la firma real de imágenes y no confunde cualquier contenedor ISO con AVIF', () => {
  assert.equal(detectImageMime(Buffer.from('ffd8ff00', 'hex')), 'image/jpeg');
  assert.equal(detectImageMime(Buffer.from('89504e470d0a1a0a', 'hex')), 'image/png');
  assert.equal(detectImageMime(Buffer.from('524946460000000057454250', 'hex')), 'image/webp');
  assert.equal(detectImageMime(Buffer.from('000000186674797061766966', 'hex')), 'image/avif');
  assert.equal(detectImageMime(Buffer.from('000000186674797069736f6d', 'hex')), null);
  assert.equal(detectImageMime(Buffer.from('<svg></svg>')), null);
});

test('rechaza una ejecución solapada con heartbeat reciente', async () => {
  const previousFetch = globalThis.fetch;
  const previousToken = process.env.ADMIN_TOKEN;
  const previousBase = process.env.TDF_API_BASE;
  globalThis.fetch = async () => new Response(JSON.stringify({
    aerId: 91,
    aerRunKey: 'operator:production:full:full:2026-08-05',
    aerStatus: 'running',
    aerPhase: 'external_research',
    aerHeartbeatAt: new Date().toISOString(),
  }), { status: 200 });
  try {
    process.env.ADMIN_TOKEN = 'overlap-test-token';
    process.env.TDF_API_BASE = 'https://tdf-api.test.invalid';
    await assert.rejects(
      runPipeline(parseArgs(['--mode', 'production', '--scope', 'full'])),
      /already active/,
    );
  } finally {
    if (previousToken == null) delete process.env.ADMIN_TOKEN;
    else process.env.ADMIN_TOKEN = previousToken;
    if (previousBase == null) delete process.env.TDF_API_BASE;
    else process.env.TDF_API_BASE = previousBase;
    globalThis.fetch = previousFetch;
  }
});

test('audita, persiste checkpoints y finaliza el run sin consultar proveedores externos', async () => {
  const requests = [];
  const previousFetch = globalThis.fetch;
  globalThis.fetch = async (rawUrl, options = {}) => {
    const url = new URL(rawUrl);
    const request = { method: options.method ?? 'GET', url: url.pathname + url.search, body: String(options.body ?? '') };
    requests.push(request);
    let payload;
    let status = 200;
    if (request.method === 'POST' && request.url === '/admin/artists/enrichment/runs') {
      payload = { aerId: 1, aerRunKey: 'test', aerStatus: 'completed' };
    } else if (request.method === 'PATCH' && request.url === '/admin/artists/enrichment/runs/1') {
      payload = { aerId: 1, aerRunKey: 'test', aerStatus: JSON.parse(request.body).aeruStatus };
    } else if (request.method === 'GET' && request.url === '/admin/artists/profiles') {
      payload = [{ apArtistId: 7, apDisplayName: 'Audit Artist', apHeroImageUrl: null }];
    } else if (request.method === 'GET' && request.url === '/admin/artists/enrichment/overview') {
      payload = {
        aeoProfiles: [{ apeArtistId: 7, apeArtistName: 'Audit Artist', apeReviewStatus: 'unverified', apeMissingFields: ['heroImageUrl'], apeBrokenFields: [] }],
        aeoInventory: [{ airId: 1 }],
        aeoSources: [],
        aeoSuggestions: [],
        aeoChanges: [],
        aeoRuns: [],
        aeoIdentityCandidates: [],
        aeoMedia: [],
      };
    } else {
      status = 404;
      payload = { error: 'not found' };
    }
    return new Response(JSON.stringify(payload), {
      status,
      headers: { 'content-type': 'application/json' },
    });
  };
  const tempDir = await mkdtemp(path.join(os.tmpdir(), 'tdf-enrichment-test-'));
  const previousToken = process.env.ADMIN_TOKEN;
  const previousBase = process.env.TDF_API_BASE;
  try {
    process.env.ADMIN_TOKEN = 'test-token-never-logged';
    process.env.TDF_API_BASE = 'https://tdf-api.test.invalid';
    const reportPath = path.join(tempDir, 'report.json');
    const report = await runPipeline({
      mode: 'dry_run',
      scope: 'audit',
      artistId: null,
      batchSize: 25,
      concurrency: 3,
      autoPublish: false,
      resume: false,
      checkpoint: path.join(tempDir, 'checkpoint.json'),
      report: reportPath,
    });
    assert.equal(report.artists.length, 1);
    assert.equal(report.artists[0].missingImage, true);
    assert.equal(report.errors.length, 0);
    assert.equal(JSON.parse(await readFile(reportPath, 'utf8')).runId, 1);
    assert.equal(requests.filter((item) => item.method === 'PATCH').length, 3);
    assert.equal(JSON.parse(requests.find((item) => item.method === 'PATCH').body).aeruPhase, 'external_research_claim');
    assert.equal(JSON.parse(requests.at(-1).body).aeruStatus, 'completed');
    assert.ok(requests.every((item) => !item.body.includes('test-token-never-logged')));
  } finally {
    if (previousToken == null) delete process.env.ADMIN_TOKEN;
    else process.env.ADMIN_TOKEN = previousToken;
    if (previousBase == null) delete process.env.TDF_API_BASE;
    else process.env.TDF_API_BASE = previousBase;
    globalThis.fetch = previousFetch;
    await rm(tempDir, { recursive: true, force: true });
  }
});

test('genera WebP y AVIF decodificables dentro de dimensiones y presupuestos', async () => {
  const tempDir = await mkdtemp(path.join(os.tmpdir(), 'tdf-media-test-'));
  try {
    const input = path.join(tempDir, 'input.png');
    const square = path.join(tempDir, 'square.webp');
    const landscape = path.join(tempDir, 'landscape.avif');
    await execFileAsync('ffmpeg', [
      '-hide_banner', '-loglevel', 'error', '-f', 'lavfi',
      '-i', 'color=c=0x336699:s=1200x800', '-frames:v', '1', input,
    ]);
    const squareBytes = await transcodeWithinBudget(
      input,
      square,
      'webp',
      'scale=1024:1024:force_original_aspect_ratio=decrease,pad=1024:1024:(ow-iw)/2:(oh-ih)/2',
      400 * 1024,
    );
    const landscapeBytes = await transcodeWithinBudget(
      input,
      landscape,
      'avif',
      'scale=1600:900:force_original_aspect_ratio=decrease,pad=1600:900:(ow-iw)/2:(oh-ih)/2',
      500 * 1024,
    );
    assert.deepEqual(await probeImage(square), { width: 1024, height: 1024 });
    assert.deepEqual(await probeImage(landscape), { width: 1600, height: 900 });
    assert.ok(squareBytes <= 400 * 1024);
    assert.ok(landscapeBytes <= 500 * 1024);
  } finally {
    await rm(tempDir, { recursive: true, force: true });
  }
});
