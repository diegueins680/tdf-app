// API integration helper: real local HTTPS storage + production media worker.
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { execFileSync, spawn } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import https from 'node:https';
import { tmpdir, userInfo } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { readS3TestXmlScalar as xml } from './music-s3-test-xml.mjs';
import { linuxWorkerConfiguration } from './music-linux-integration.mjs';

const root = dirname(dirname(dirname(fileURLToPath(import.meta.url))));
const sha = (bytes) => createHash('sha256').update(bytes).digest('hex');
const hostDatabaseUrl = () => `postgresql://127.0.0.1:${process.env.PGPORT ?? '5432'}/${process.env.TDF_MUSIC_API_E2E_DATABASE}?user=${encodeURIComponent(process.env.PGUSER ?? userInfo().username)}`;
export function localStorageRequest(url, method = 'GET', body, headers = {}) {
  const endpoint = process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT;
  assert.match(endpoint ?? '', /^https:\/\/127\.0\.0\.1:\d+$/);
  assert.equal(new URL(url).origin, endpoint, 'No remote object access in local E2E');
  const ca = readFileSync(process.env.TDF_MUSIC_API_E2E_S3_CA);
  return new Promise((resolve, reject) => {
    const req = https.request(url, { ca, agent: false, method, timeout: 30000,
      headers: { ...headers, ...(body === undefined ? {} : { 'Content-Length': body.length }) },
    }, (res) => {
      const chunks = [];
      res.on('data', (chunk) => chunks.push(chunk)); res.on('error', reject);
      res.on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(chunks) }));
    });
    req.on('error', reject); req.on('timeout', () => req.destroy(new Error('Local S3 timeout')));
    req.end(body);
  });
}
async function command(program, args, env = process.env) {
  return await new Promise((resolve, reject) => {
    const child = spawn(program, args, { env, stdio: ['ignore', 'pipe', 'pipe'] });
    let output = '';
    const append = (data) => { output = (output + data).slice(-5000); };
    child.stdout.on('data', append); child.stderr.on('data', append);
    const timer = setTimeout(() => child.kill('SIGTERM'), 600000);
    child.on('error', (error) => { clearTimeout(timer); reject(error); });
    child.on('close', (status) => {
      clearTimeout(timer);
      for (const key of ['PGPASSWORD', 'MUSIC_S3_SECRET_ACCESS_KEY', 'MUSIC_S3_ACCESS_KEY_ID']) {
        if (env[key]) output = output.replaceAll(env[key], '[test credential]');
      }
      status === 0 ? resolve(output) : reject(new Error(`${program} failed (${status}): ${output}`));
    });
  });
}

async function runAssetWorker(env) {
  if (!process.env.TDF_MUSIC_LINUX_IMAGE) return command('bash', [join(root, 'scripts/run-music-release-worker-once.sh')], env);
  const config = linuxWorkerConfiguration(env);
  const owner = async () => (await command('docker', ['inspect', '--format', '{{index .Config.Labels "tdf.music-linux-run"}}', config.name])).trim();
  try {
    assert.equal((await command('docker', ['network', 'inspect', '--format', '{{index .Labels "tdf.music-linux-run"}}', config.network])).trim(), config.network);
    return await command('docker', config.args, config.env);
  } finally {
    const present = (await command('docker', ['ps', '-aq', '--filter', `name=^/${config.name}$`, '--filter', `label=tdf.music-linux-run=${config.network}`])).trim();
    if (present) {
      assert.equal(await owner(), config.network);
      await command('docker', ['rm', '-f', config.name]);
    }
  }
}

export async function processRealApiAssets({ request, sql, single, recordingId, member }) {
  const runtime = mkdtempSync(join(tmpdir(), 'tdf-music-api-assets-'));
  try {
    const masterPath = join(runtime, 'master.wav'), coverPath = join(runtime, 'cover.png');
    // >16 MiB requires two parts under the actual API session policy.
    await command('ffmpeg', ['-v', 'error', '-f', 'lavfi', '-i', 'sine=frequency=440:sample_rate=48000',
      '-t', '62', '-ac', '2', '-c:a', 'pcm_s24le', '-threads', '1', masterPath]);
    await command('ffmpeg', ['-v', 'error', '-f', 'lavfi', '-i', 'color=c=navy:s=3000x3000',
      '-frames:v', '1', '-threads', '1', coverPath]);
    const master = readFileSync(masterPath), cover = readFileSync(coverPath);
    const uploadAsset = async (bytes, role, mediaType) => {
      const path = `/music/releases/${single.id}/versions/${single.versionId}/uploads`;
      const creation = { token: member.token, method: 'POST', expected: 201,
        idempotencyKey: `real-s3-${role}`, json: { recordingId: role === 'master_audio' ? recordingId : null,
          assetRole: role, originalFilename: role === 'master_audio' ? 'master.wav' : 'cover.png',
          expectedMediaType: mediaType, expectedSize: bytes.length, expectedSha256: sha(bytes) } };
      let session = await request(path, creation);
      assert.equal((await request(path, creation)).id, session.id);
      const started = await localStorageRequest(session.createMultipartUrl, 'POST');
      assert.equal(started.status, 200);
      await request(`/music/uploads/${session.id}/provider`, { token: member.token, method: 'PUT',
        json: { providerUploadId: xml(started.body, 'UploadId') } });
      const count = Math.ceil(bytes.length / session.partSizeBytes);
      if (role === 'master_audio') assert.ok(count > 1, 'Master must exercise real multipart resumption');
      for (let part = 1; part <= count; part += 1) {
        const data = bytes.subarray((part - 1) * session.partSizeBytes, part * session.partSizeBytes);
        const partPath = `/music/uploads/${session.id}/parts/${part}`;
        const signed = await request(partPath, { token: member.token });
        const response = await localStorageRequest(signed.url, 'PUT', data);
        assert.equal(response.status, 200); assert.ok(response.headers.etag);
        const evidence = { token: member.token, method: 'PUT', json: {
          byteSize: data.length, etag: response.headers.etag, sha256: sha(data) } };
        await request(partPath, evidence); await request(partPath, evidence);
        // Resume from API-persisted evidence, not a fabricated provider upload ID.
        session = await request(path, creation);
        assert.equal(session.parts.length, part);
        assert.equal(session.providerUploadIdBound, true);
      }
      const completion = await request(`/music/uploads/${session.id}/completion`, { token: member.token });
      const result = await localStorageRequest(completion.url, 'POST', Buffer.from(completion.body),
        { 'Content-Type': completion.contentType });
      assert.equal(result.status, 200); assert.match(result.body.toString(), /<CompleteMultipartUploadResult/);
      const confirm = { token: member.token, method: 'POST', json: { etag: xml(result.body, 'ETag') } };
      const confirmed = await request(`/music/uploads/${session.id}/confirm`, confirm);
      assert.equal(confirmed.status, 'completed');
      assert.deepEqual(await request(`/music/uploads/${session.id}/confirm`, confirm), confirmed);
      console.log(`PASS API → real S3 ${role}: ${count} parts, persisted resumption and idempotent confirmation`);
    };
    // Queue both assets before starting the worker.
    await uploadAsset(master, 'master_audio', 'audio/wav');
    await uploadAsset(cover, 'cover_original', 'image/png');
    const workerEnv = { ...process.env,
      DATABASE_URL: hostDatabaseUrl(),
      MUSIC_S3_ENDPOINT: process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT,
      MUSIC_S3_REGION: 'us-east-1', MUSIC_S3_MASTER_BUCKET: 'music-e2e-master',
      MUSIC_S3_DERIVATIVE_BUCKET: 'music-e2e-derivative', MUSIC_S3_DDEX_BUCKET: 'music-e2e-ddex',
      CURL_CA_BUNDLE: process.env.TDF_MUSIC_API_E2E_S3_CA, MUSIC_WORKER_ID: 'real-api-s3-test',
      // Exercise actual worker multipart with this >16 MiB synthetic master.
      MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES: '5242880', MUSIC_WORKER_MULTIPART_PART_BYTES: '5242880',
    };
    const workerOutputs = [];
    for (let i = 0; i < 2; i += 1) workerOutputs.push(await runAssetWorker(workerEnv));
    const receipts = workerOutputs.flatMap((output) => output.split('\n').flatMap((line) => {
      try { const value = JSON.parse(line); return value?.mode ? [value] : []; } catch { return []; }
    }));
    const masterReceipt = receipts.find((receipt) => receipt.sha256 === sha(master) && receipt.bytes === master.length);
    assert.ok(masterReceipt, 'Worker must report the actual master transfer');
    assert.equal(masterReceipt.mode, 'multipart');
    assert.equal(masterReceipt.parts, Math.ceil(master.length / 5242880));
    assert.equal(sql(`SELECT count(*) FROM music_processing_job WHERE release_version_id='${single.versionId}' AND status='succeeded'`), '2');
    assert.equal(sql(`SELECT count(*) FROM music_processing_job WHERE release_version_id='${single.versionId}'`), '2');
    const assets = JSON.parse(sql(`SELECT json_agg(json_build_object('id',id,'role',asset_role,'sha256',sha256,'state',processing_state,'immutable',immutable,'bytes',byte_size,'bucket',bucket_name,'key',object_key)) FROM music_asset WHERE release_version_id='${single.versionId}'`));
    const find = (role) => {
      const asset = assets.find((item) => item.role === role);
      assert.ok(asset, `Missing worker output ${role}`);
      assert.ok(asset.immutable); assert.ok(['ready', 'valid'].includes(asset.state));
      return asset;
    };
    const original = find('master_audio');
    assert.equal(original.sha256, sha(master)); assert.equal(original.bytes, master.length);
    assert.equal(find('cover_original').sha256, sha(cover));
    assert.equal(assets.filter((asset) => asset.role === 'stream_audio').length, 4);
    assert.equal(sql(`SELECT round(duration_ms/1000.0) FROM music_recording WHERE id='${recordingId}'`), '62');
    for (const asset of assets) {
      assert.equal((await localStorageRequest(`${workerEnv.MUSIC_S3_ENDPOINT}/${asset.bucket}/${asset.key}`)).status, 403);
    }
    console.log('PASS real worker: multipart master promotion, original SHA-256 preserved, four qualities, preview and private artwork');
    return { masterAssetId: original.id, streamAssetId: find('stream_audio').id,
      coverOriginalAssetId: find('cover_original').id, coverDisplayAssetId: find('cover_display').id,
      masterSha256: sha(master), assets };
  } finally { rmSync(runtime, { recursive: true, force: true }); }
}

export async function assertStoredAsset(url, expectedSha256, durationMs) {
  const stored = await localStorageRequest(url);
  assert.equal(stored.status, 200);
  assert.equal(sha(stored.body), expectedSha256);
  if (durationMs !== undefined) {
    const probe = JSON.parse(execFileSync('ffprobe', ['-v', 'error', '-show_entries',
      'format=duration', '-of', 'json', '-i', 'pipe:0'], { input: stored.body, encoding: 'utf8', timeout: 30000 }));
    assert.ok(Math.abs(Number(probe.format.duration) * 1000 - durationMs) < 40,
      `Stored preview duration ${probe.format.duration}s must match ${durationMs}ms`);
  }
  const range = await localStorageRequest(url, 'GET', undefined, { Range: 'bytes=0-127' });
  assert.equal(range.status, 206);
  assert.deepEqual(range.body, stored.body.subarray(0, 128));
}

async function runRealQueuedWorker() {
  await runAssetWorker({
    ...process.env,
    DATABASE_URL: hostDatabaseUrl(),
    MUSIC_S3_ENDPOINT: process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT, MUSIC_S3_REGION: 'us-east-1',
    MUSIC_S3_MASTER_BUCKET: 'music-e2e-master', MUSIC_S3_DERIVATIVE_BUCKET: 'music-e2e-derivative',
    MUSIC_S3_DDEX_BUCKET: 'music-e2e-ddex', CURL_CA_BUNDLE: process.env.TDF_MUSIC_API_E2E_S3_CA,
    MUSIC_WORKER_ID: 'real-api-preview-test',
    ...(process.env.TDF_MUSIC_API_E2E_DDEX_SCHEMA
      ? { MUSIC_DDEX_SCHEMA_DIR: process.env.TDF_MUSIC_API_E2E_DDEX_SCHEMA } : {}),
  });
}
export { runRealQueuedWorker as runRealPreviewWorker };

export async function assertRealDdexPackage({ request, sql, exportId, admin, outsider, evidence, operation = 'new_release' }) {
  // Prioritize this fixture's job over unrelated retry fixtures without
  // cancelling or consuming them. The worker still claims via its real SQL.
  sql(`UPDATE music_processing_job SET run_after=NOW()-INTERVAL '2 days'
    WHERE job_kind='generate_ddex' AND job_key='${exportId}'`);
  await runRealQueuedWorker();
  const exported = JSON.parse(sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exportId}'`));
  assert.equal(exported.status, 'valid');
  assert.equal(exported.validation_report.adapterVersion, 'tdf-ern432-audio-v5');
  assert.equal(exported.operation, operation);
  assert.equal(exported.validation_report.deliveryPerformed, false);
  await request(`/music/ddex/exports/${exportId}/download`, { token: outsider.token, expected: 403 });
  const access = await request(`/music/ddex/exports/${exportId}/download`, { token: admin.token });
  const response = await localStorageRequest(access.url);
  assert.equal(response.status, 200);
  assert.equal(sha(response.body), exported.package_sha256);
  assert.equal(response.body.length, access.byteSize);
  const packagePath = join(evidence, operation === 'new_release' ? 'package.zip' : `${operation}-${exportId}.zip`);
  writeFileSync(packagePath, response.body);
  const extract = path => execFileSync('unzip', ['-p', packagePath, path], { maxBuffer: 32 * 1024 * 1024 });
  const manifest = JSON.parse(extract('manifest.json'));
  assert.equal(manifest.manifestVersion, 3);
  assert.match(manifest.messageFile, /^(?:[0-9]{12,13}|[A-Z0-9]{18})\.xml$/);
  assert.equal(manifest.canonicalSnapshotSha256, exported.canonical_snapshot_sha256);
  assert.equal(manifest.packageFormat, 'tdf-offline-review-bundle');
  assert.deepEqual(execFileSync('unzip', ['-Z1', packagePath], { encoding: 'utf8' }).trim().split('\n').sort(),
    ['manifest.json', ...manifest.files.map(file => file.path)].sort());
  for (const file of manifest.files) {
    const bytes = extract(file.path);
    assert.equal(sha(bytes), file.sha256); assert.equal(bytes.length, file.bytes);
  }
  const xml = extract(manifest.messageFile);
  const releaseId = manifest.messageFile.slice(0, -4);
  for (const file of manifest.files.filter(file => file.path.startsWith('resources/'))) {
    assert.match(file.path, new RegExp(`^resources/${releaseId}_T(?:[0-9]+_SoundRecording\\.m4a|Artwork_CoverArt\\.jpg)$`));
  }
  assert.ok(!xml.includes('UpdateIndicator'));
  if (operation === 'takedown') assert.ok(!xml.includes('<DealList>'));
  else {
    assert.ok(xml.includes('<CommercialModelType>FreeOfChargeModel</CommercialModelType>'));
    assert.ok(xml.includes('<UseType>OnDemandStream</UseType>'));
  }
  execFileSync('xmllint', ['--nonet', '--noout', '--schema',
    join(process.env.TDF_MUSIC_API_E2E_DDEX_SCHEMA, 'release-notification.xsd'), '-'], { input: xml });
  // Simulate crash after export commit, before job acknowledgement; recovery
  // must not change the export row, references or stored package bytes.
  const before = sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exportId}'`);
  sql(`UPDATE music_processing_job SET status='retry',run_after=NOW()-INTERVAL '1 day'
    WHERE job_kind='generate_ddex' AND job_key='${exportId}'`);
  await runRealQueuedWorker();
  assert.equal(sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${exportId}'`), before);
  assert.equal(sql(`SELECT output->>'recoveredExistingExport' FROM music_processing_job
    WHERE job_kind='generate_ddex' AND job_key='${exportId}'`), 'true');
  const again = await request(`/music/ddex/exports/${exportId}/download`, { token: admin.token });
  assert.deepEqual((await localStorageRequest(again.url)).body, response.body);
  const asset = JSON.parse(sql(`SELECT json_build_object('bucket_name',bucket_name,'object_key',object_key)
    FROM music_asset WHERE id='${exported.package_asset_id}'`));
  assert.equal((await localStorageRequest(`${process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT}/${asset.bucket_name}/${asset.object_key}`)).status, 403);
  console.log(`PASS complete offline DDEX bundle → real renderer/XSD/resources/ZIP/S3, authorized identical downloads and crash recovery; evidence ${packagePath}`);
  return { exported, xml: xml.toString('utf8'), bytes: response.body, packagePath };
}
