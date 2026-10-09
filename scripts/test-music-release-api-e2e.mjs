#!/usr/bin/env node

import assert from 'node:assert/strict';
import { randomUUID } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { tmpdir } from 'node:os';
import { fileURLToPath } from 'node:url';
import { processRealApiAssets, assertStoredAsset, runRealPreviewWorker, assertRealDdexPackage } from './lib/music-api-s3-probe.mjs';
import { runMusicBrowserProbe } from './lib/music-browser-probe.mjs';
import { probeDdexEnqueue } from './lib/music-ddex-enqueue-probe.mjs';
import { probePlaybackIdentity } from './lib/music-playback-identity-probe.mjs';

const apiBase = (process.env.TDF_MUSIC_API_E2E_BASE ?? '').replace(/\/$/, '');
const database = process.env.TDF_MUSIC_API_E2E_DATABASE ?? '';
const password = process.env.TDF_MUSIC_API_E2E_PASSWORD ?? '';
const realS3 = Boolean(process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT);
let realAssets;
let masterAssetId, streamAssetId, coverOriginalAssetId, coverDisplayAssetId;

assert.match(apiBase, /^http:\/\/(127\.0\.0\.1|localhost):\d+$/, 'E2E API must use loopback HTTP');
assert.match(database, /^[a-z0-9_]+$/, 'E2E database name is invalid');
assert.ok(password.length >= 16, 'A runtime-only synthetic password is required');

const uuidPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const assertUuid = (value, label) => assert.match(value, uuidPattern, `${label} must be a UUID`);
const sleep = (milliseconds) => new Promise((resolve) => setTimeout(resolve, milliseconds));

function sql(statement) {
  return execFileSync(
    'psql',
    ['-X', '-v', 'ON_ERROR_STOP=1', '-d', database, '-Atqc', statement],
    { encoding: 'utf8' },
  ).trim();
}

async function request(path, {
  token,
  method = 'GET',
  json,
  expected = 200,
  idempotencyKey,
  country,
} = {}) {
  const headers = {};
  if (token) headers.Authorization = `Bearer ${token}`;
  if (json !== undefined) headers['Content-Type'] = 'application/json';
  if (idempotencyKey) headers['Idempotency-Key'] = idempotencyKey;
  if (country) headers['CF-IPCountry'] = country;
  const response = await fetch(`${apiBase}${path}`, {
    method,
    headers,
    body: json === undefined ? undefined : JSON.stringify(json),
  });
  const raw = await response.text();
  if (!(Array.isArray(expected) ? expected.includes(response.status) : response.status === expected)) {
    throw new Error(`${method} ${path}: expected ${expected}, got ${response.status}: ${raw.slice(0, 1200)}`);
  }
  if (!raw) return undefined;
  return (response.headers.get('content-type') ?? '').includes('json') ? JSON.parse(raw) : raw;
}

async function login(email) {
  const session = await request('/login', {
    method: 'POST',
    json: { username: email, password },
  });
  assert.ok(session.token, `Login did not return a token for ${email}`);
  return session;
}

const artist = await login('music.artist@persona.test');
const member = await login('per-12.karla@persona.test');
const admin = await login('per-16.irene@persona.test');
const outsider = await login('per-10.lucia@persona.test');
const genreId = sql(`
  SELECT item.id
  FROM genre item
  JOIN catalog_definition catalog ON catalog.id=item.catalog_id
  JOIN workflow_state state ON state.id=item.workflow_state_id
  WHERE catalog.code='genres'
    AND catalog.active
    AND item.code='latin'
    AND item.active
    AND item.deprecated_at IS NULL
    AND state.code IN ('published','approved','active')
  LIMIT 1
`);
assertUuid(genreId, 'published canonical genre');

const createPayload = (kind, slug, title) => ({
  artistPartyId: artist.partyId,
  canonicalSlug: slug,
  releaseKind: kind,
  title,
  displayArtist: 'Artista Música E2E',
  titleLanguage: 'es',
});

const single = await request('/music/releases', {
  token: artist.token,
  method: 'POST',
  expected: 201,
  idempotencyKey: 'music-e2e-single-create',
  json: createPayload('single', 'single-musica-e2e', 'Single Música E2E'),
});
assertUuid(single.id, 'single release');
assertUuid(single.versionId, 'single version');

const repeatedSingle = await request('/music/releases', {
  token: artist.token,
  method: 'POST',
  expected: 201,
  idempotencyKey: 'music-e2e-single-create',
  json: createPayload('single', 'single-musica-e2e', 'Single Música E2E'),
});
assert.equal(repeatedSingle.id, single.id);
assert.equal(repeatedSingle.versionId, single.versionId);

const album = await request('/music/releases', {
  token: member.token,
  method: 'POST',
  expected: 201,
  idempotencyKey: 'music-e2e-member-album',
  json: createPayload('album', 'album-equipo-e2e', 'Álbum del Equipo E2E'),
});
assert.equal(album.kind, 'album');

await request('/music/releases', {
  token: outsider.token,
  method: 'POST',
  expected: 403,
  idempotencyKey: 'music-e2e-outsider-denied',
  json: createPayload('ep', 'ep-no-autorizado-e2e', 'EP no autorizado'),
});

const mineForMember = await request(`/music/studio/releases?artistPartyId=${artist.partyId}`, {
  token: member.token,
});
assert.equal(mineForMember.length, 2);

let version = await request(`/music/releases/${single.id}/versions/${single.versionId}`, {
  token: artist.token,
});
version = await request(`/music/releases/${single.id}/versions/${single.versionId}/draft`, {
  token: artist.token,
  method: 'PUT',
  json: {
    expectedUpdatedAt: version.updatedAt,
    title: 'Single Música E2E',
    subtitle: null,
    versionTitle: null,
    displayArtist: 'Artista Música E2E',
    titleLanguage: 'es',
    titleScript: null,
    primaryGenreId: genreId,
    secondaryGenreId: null,
    explicitContent: 'not_explicit',
    originalReleaseDate: '2026-09-12',
    releaseAtUtc: null,
    releaseTimezone: null,
    embargoUntilUtc: null,
    labelName: 'Sello Sintético E2E',
    catalogNumber: 'E2E-0001',
    recordingCopyrightText: '℗ 2026 Artista Música E2E',
    workCopyrightText: '© 2026 Artista Música E2E',
  },
});

const partyDraft = {
  clientRef: 'external-rightsholder',
  partyId: null,
  tdfPartyId: null,
  displayName: 'Colaborador Externo Sintético',
  legalName: 'Colaborador Externo Sintético',
  partyKind: 'person',
  identifiers: [],
};
const pendingPartyDraft = {
  ...partyDraft,
  clientRef: 'uncredited-collaborator',
  displayName: 'Colaborador Sin Crédito E2E',
  legalName: null,
};
const trackDraft = (recordingId = null) => ({
  clientRef: 'track-1',
  recordingId,
  title: 'Pista Sintética E2E',
  subtitle: null,
  versionTitle: null,
  titleLanguage: 'es',
  titleScript: null,
  explicitContent: 'not_explicit',
  discNumber: 1,
  trackNumber: 1,
  displayArtist: 'Artista Música E2E',
  isPrimaryResource: true,
  previewStartMs: realS3 ? 1500 : null,
  previewDurationMs: realS3 ? 1250 : null,
});
const rights = ['master', 'composition'].map((scope) => ({
  trackRef: null,
  scope,
  authorityBasis: scope === 'master' ? 'owned' : 'licensed',
  territories: ['EC'],
  startsOn: '2026-09-12',
  endsOn: null,
  splits: [{
    partyRef: 'external-rightsholder',
    basisPoints: 10000,
    territories: ['EC'],
    startsOn: '2026-09-12',
    endsOn: null,
  }],
}));
const credits = [
  { partyRef: 'external-rightsholder', trackRef: null, role: 'main_artist', displayOrder: 0, notes: null },
  { partyRef: 'external-rightsholder', trackRef: null, role: 'composer', displayOrder: 1, notes: null },
];
const identifiers = [
  { trackRef: null, type: 'upc', value: '036000291452' },
  { trackRef: 'track-1', type: 'isrc', value: 'USRC17607839' },
];
const availability = (masterAssetId = null) => [{
  trackRef: null,
  territoryMode: 'include',
  territories: ['EC'],
  startsAt: null,
  endsAt: null,
  listeningPolicy: 'full',
  downloadPolicy: masterAssetId ? 'purchase' : 'none',
  purchasable: Boolean(masterAssetId),
  priceMinor: masterAssetId ? 250 : null,
  currency: masterAssetId ? 'USD' : null,
  downloadableAssetId: masterAssetId,
}];

version = await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: artist.token,
  method: 'PUT',
  json: {
    expectedUpdatedAt: version.updatedAt,
    tracks: [trackDraft()],
    parties: [partyDraft, pendingPartyDraft],
    credits,
    identifiers,
    rightsDeclarations: rights,
    availability: availability(),
  },
});
assert.equal(version.tracks.length, 1);
assert.equal(version.parties.length, 2);
const recordingId = version.tracks[0].recordingId;
const rightsHolderId = version.parties.find((party) => party.displayName === partyDraft.displayName)?.id;
const pendingPartyId = version.parties.find((party) => party.displayName === pendingPartyDraft.displayName)?.id;
assertUuid(recordingId, 'recording');
assertUuid(rightsHolderId, 'external rights holder');
assertUuid(pendingPartyId, 'uncredited collaborator');
const retainedParties = [
  { ...partyDraft, partyId: rightsHolderId },
  { ...pendingPartyDraft, partyId: pendingPartyId },
];
const draftContent = () => ({
  expectedUpdatedAt: version.updatedAt, tracks: [trackDraft(recordingId)],
  parties: retainedParties, credits, identifiers, rightsDeclarations: rights,
  availability: availability(),
});
// Another authorized editor can save the creator's uncredited external party.
version = await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: member.token, method: 'PUT', json: draftContent(),
});
assert.deepEqual(new Set(version.parties.map((party) => party.id)), new Set([rightsHolderId, pendingPartyId]));
const reloadedParties = await request(`/music/releases/${single.id}/versions/${single.versionId}`, {
  token: artist.token,
});
assert.equal(reloadedParties.parties.find((party) => party.id === pendingPartyId)?.tdfPartyId, null);
assert.equal(sql(`SELECT count(*) FROM music_party WHERE display_name='Colaborador Sin Crédito E2E'`), '1');
await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: outsider.token, method: 'PUT', expected: 403, json: draftContent(),
});
const foreignPartyId = sql(`INSERT INTO music_party(display_name,created_by)
  VALUES ('Unrelated private synthetic party',${outsider.partyId}) RETURNING id;`);
assertUuid(foreignPartyId, 'unrelated private party');
await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: member.token, method: 'PUT', expected: 400,
  json: { ...draftContent(), parties: [...retainedParties, {
    ...pendingPartyDraft, clientRef: 'foreign-party', partyId: foreignPartyId,
  }] },
});
assert.equal(sql(`SELECT count(*) FROM music_release_version_party WHERE music_party_id='${foreignPartyId}'`), '0');
console.log('PASS API uncredited collaborator → retained IDs, team edit, reload and isolation');

// Edit the same identity in this version, including replacement/removal of
// provided identifiers. No identity-directory row is modified by this write.
retainedParties[1] = { ...retainedParties[1], displayName: 'Nombre Revisado E2E',
  legalName: 'Nombre Legal Sintético E2E',
  identifiers: [{ type: 'proprietary', value: 'synthetic:collaborator:v1' }] };
version = await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: member.token, method: 'PUT', json: draftContent(),
});
const revisedParty = version.parties.find((party) => party.id === pendingPartyId);
assert.equal(revisedParty.displayName, retainedParties[1].displayName);
assert.equal(revisedParty.legalName, retainedParties[1].legalName);
assert.equal(revisedParty.detailsSource, 'user_provided');
assert.equal(revisedParty.identifiers[0].identifier_value, 'synthetic:collaborator:v1');
assert.equal(revisedParty.identifiers[0].verification_status, 'unvalidated');
assert.equal(sql(`SELECT display_name FROM music_party WHERE id='${pendingPartyId}'`), pendingPartyDraft.displayName);
assert.equal(sql(`SELECT count(*) FROM music_party_identifier WHERE music_party_id='${pendingPartyId}'`), '0');
const partyAudit = JSON.parse(sql(`SELECT data->'parties_snapshot' FROM music_release_audit_event
  WHERE release_version_id='${single.versionId}' AND event_type='catalog_content_replaced'
  ORDER BY occurred_at DESC,id DESC LIMIT 1`));
assert.equal(partyAudit.find((party) => party.id === pendingPartyId).legalName, revisedParty.legalName);
console.log('PASS API party details → version-local name, legal name, identifier and audit snapshot');

const invalidBeforeProcessing = await request(
  `/music/releases/${single.id}/versions/${single.versionId}/validate`,
  { token: artist.token, method: 'POST' },
);
assert.equal(invalidBeforeProcessing.valid, false);
assert.ok(invalidBeforeProcessing.errors.some((error) => error.code === 'assets_invalid'));

if (realS3) {
  realAssets = await processRealApiAssets({ request, sql, single, recordingId, member });
  ({ masterAssetId, streamAssetId, coverOriginalAssetId, coverDisplayAssetId } = realAssets);
} else {
const upload = await request(`/music/releases/${single.id}/versions/${single.versionId}/uploads`, {
  token: member.token,
  method: 'POST',
  expected: 201,
  idempotencyKey: 'music-e2e-resumable-upload',
  json: {
    recordingId,
    assetRole: 'master_audio',
    originalFilename: 'master-sintetico.wav',
    expectedMediaType: 'audio/wav',
    expectedSize: 1024,
    expectedSha256: 'a'.repeat(64),
  },
});
assertUuid(upload.id, 'upload session');
assert.match(upload.createMultipartUrl, /^https:\/\/127\.0\.0\.1:19000\//);
const repeatedUpload = await request(`/music/releases/${single.id}/versions/${single.versionId}/uploads`, {
  token: member.token,
  method: 'POST',
  expected: 201,
  idempotencyKey: 'music-e2e-resumable-upload',
  json: {
    recordingId,
    assetRole: 'master_audio',
    originalFilename: 'master-sintetico.wav',
    expectedMediaType: 'audio/wav',
    expectedSize: 1024,
    expectedSha256: 'a'.repeat(64),
  },
});
assert.equal(repeatedUpload.id, upload.id);

await request(`/music/uploads/${upload.id}/provider`, {
  token: member.token,
  method: 'PUT',
  json: { providerUploadId: 'synthetic-upload-id-0001' },
});
const signedPart = await request(`/music/uploads/${upload.id}/parts/1`, { token: member.token });
assert.match(signedPart.url, /partNumber=1/);
await request(`/music/uploads/${upload.id}/parts/1`, {
  token: member.token,
  method: 'PUT',
  json: { byteSize: 1024, etag: '"d41d8cd98f00b204e9800998ecf8427e"', sha256: 'b'.repeat(64) },
});
await request(`/music/uploads/${upload.id}/parts/1`, {
  token: member.token,
  method: 'PUT',
  json: { byteSize: 1024, etag: '"d41d8cd98f00b204e9800998ecf8427e"', sha256: 'b'.repeat(64) },
});
const completion = await request(`/music/uploads/${upload.id}/completion`, { token: member.token });
assert.equal(completion.method, 'POST');
assert.match(completion.body, /<PartNumber>1<\/PartNumber>/);
const confirmedUpload = await request(`/music/uploads/${upload.id}/confirm`, {
  token: member.token,
  method: 'POST',
  json: { etag: '"0cc175b9c0f1b6a831c399e269772661"' },
});
assert.equal(confirmedUpload.status, 'completed');

masterAssetId = randomUUID();
streamAssetId = randomUUID();
coverOriginalAssetId = randomUUID();
coverDisplayAssetId = randomUUID();
for (const [value, label] of [
  [masterAssetId, 'master asset'],
  [streamAssetId, 'stream asset'],
  [coverOriginalAssetId, 'cover original'],
  [coverDisplayAssetId, 'cover display'],
]) assertUuid(value, label);

sql(`
  UPDATE music_recording
  SET duration_ms=62000
  WHERE id='${recordingId}'::uuid;
  INSERT INTO music_asset(
    id,release_version_id,recording_id,asset_role,storage_provider,storage_class,
    bucket_name,object_key,original_filename,media_type,byte_size,sha256,
    processing_state,immutable,technical_metadata,created_by,ready_at
  ) VALUES (
    '${masterAssetId}'::uuid,'${single.versionId}'::uuid,'${recordingId}'::uuid,
    'master_audio','s3_compatible','standard','music-e2e-master',
    'masters/e2e/${masterAssetId}/master.wav','master-sintetico.wav','audio/wav',
    2048,'${'c'.repeat(64)}','ready',TRUE,'{"fixture":"licensed-synthetic"}'::jsonb,
    ${artist.partyId},NOW()
  );
  INSERT INTO music_asset(
    id,release_version_id,recording_id,parent_asset_id,asset_role,storage_provider,
    storage_class,bucket_name,object_key,media_type,byte_size,sha256,processing_state,
    immutable,technical_metadata,created_by,ready_at
  ) VALUES (
    '${streamAssetId}'::uuid,'${single.versionId}'::uuid,'${recordingId}'::uuid,
    '${masterAssetId}'::uuid,'stream_audio','s3_compatible','standard',
    'music-e2e-derivative','streams/e2e/${streamAssetId}/high.m4a','audio/mp4',1024,
    '${'d'.repeat(64)}','ready',TRUE,
    '{"codec":"aac","bitrate_kbps":256,"loudness_lufs":-14.0}'::jsonb,
    ${artist.partyId},NOW()
  );
  INSERT INTO music_asset(
    id,release_version_id,asset_role,storage_provider,storage_class,bucket_name,
    object_key,original_filename,media_type,byte_size,sha256,processing_state,
    immutable,created_by,ready_at
  ) VALUES (
    '${coverOriginalAssetId}'::uuid,'${single.versionId}'::uuid,'cover_original',
    's3_compatible','standard','music-e2e-master',
    'art/e2e/${coverOriginalAssetId}/cover.png','cover-sintetica.png','image/png',2048,
    '${'e'.repeat(64)}','ready',TRUE,${artist.partyId},NOW()
  );
  INSERT INTO music_asset(
    id,release_version_id,parent_asset_id,asset_role,storage_provider,storage_class,
    bucket_name,object_key,media_type,byte_size,sha256,processing_state,immutable,
    created_by,ready_at
  ) VALUES (
    '${coverDisplayAssetId}'::uuid,'${single.versionId}'::uuid,
    '${coverOriginalAssetId}'::uuid,'cover_display','s3_compatible','standard',
    'music-e2e-derivative','art/e2e/${coverDisplayAssetId}/cover.jpg','image/jpeg',1024,
    '${'f'.repeat(64)}','ready',TRUE,${artist.partyId},NOW()
  );
  SELECT * FROM music_refresh_validation_flags('${single.versionId}'::uuid);
`);
}

version = await request(`/music/releases/${single.id}/versions/${single.versionId}`, {
  token: artist.token,
});
version = await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
  token: artist.token,
  method: 'PUT',
  json: {
    expectedUpdatedAt: version.updatedAt,
    tracks: [trackDraft(recordingId)],
    parties: retainedParties,
    credits,
    identifiers,
    rightsDeclarations: rights,
    availability: availability(masterAssetId),
  },
});
let availabilityRuleId = version.availability[0].id;
assertUuid(availabilityRuleId, 'availability rule');

if (realS3) {
  version = await request(`/music/releases/${single.id}/versions/${single.versionId}/content`, {
    token: artist.token, method: 'PUT', json: {
      expectedUpdatedAt: version.updatedAt,
      tracks: [{ ...trackDraft(recordingId), previewStartMs: 2500, previewDurationMs: 1750 }],
      parties: retainedParties, credits, identifiers,
      rightsDeclarations: rights, availability: availability(masterAssetId),
    },
  });
  availabilityRuleId = version.availability[0].id;
  const pending = await request(`/music/releases/${single.id}/versions/${single.versionId}/validate`, {
    token: artist.token, method: 'POST',
  });
  assert.ok(pending.errors.some((error) => error.code === 'preview_processing_required'));
  await runRealPreviewWorker();
  assert.equal(sql(`SELECT count(*) FROM music_processing_job WHERE release_version_id='${single.versionId}' AND job_kind='create_preview' AND status='succeeded'`), '1');
  assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${single.versionId}' AND asset_role='preview_audio'`), '2');
  console.log('PASS API preview correction → validation gate, real preview job and retained previous asset');
}

await request(`/music/releases/${single.id}/versions/${single.versionId}/terms`, {
  token: artist.token,
  method: 'POST',
  expected: 201,
  json: {
    kind: 'publication_authority',
    version: 'publication-authority-e2e-v1',
    accepted: true,
    evidence: { fixture: 'synthetic', authorityConfirmed: true },
  },
});
const validation = await request(`/music/releases/${single.id}/versions/${single.versionId}/validate`, {
  token: artist.token,
  method: 'POST',
});
assert.deepEqual(validation, { valid: true, errors: [] });

const transition = (token, key, targetState, extra = {}) => request(
  `/music/releases/${single.id}/versions/${single.versionId}/transition`,
  {
    token,
    method: 'POST',
    idempotencyKey: key,
    json: { targetState, ...extra },
  },
);

assert.equal((await transition(artist.token, 'music-e2e-ready-1', 'ready_for_review')).state, 'ready_for_review');
assert.equal((await transition(admin.token, 'music-e2e-review-1', 'in_review')).state, 'in_review');
const changeRequest = await request(`/music/releases/${single.id}/versions/${single.versionId}/comments`, {
  token: admin.token,
  method: 'POST',
  expected: 201,
  json: {
    parentId: null,
    fieldPath: 'metadata.catalogNumber',
    body: 'Confirma el número de catálogo sintético.',
    staffOnly: false,
    requestChanges: true,
  },
});
assertUuid(changeRequest.id, 'change request');
await request(
  `/music/releases/${single.id}/versions/${single.versionId}/comments/${changeRequest.id}/resolve`,
  { token: artist.token, method: 'POST' },
);
assert.equal((await transition(artist.token, 'music-e2e-ready-2', 'ready_for_review')).state, 'ready_for_review');
assert.equal((await transition(admin.token, 'music-e2e-review-2', 'in_review')).state, 'in_review');
assert.equal((await transition(admin.token, 'music-e2e-approved', 'approved')).state, 'approved');
const approval = JSON.parse(sql(`SELECT immutable_snapshot FROM music_release_version WHERE id='${single.versionId}'`));
const approvalHash = sql(`SELECT snapshot_sha256 FROM music_release_version WHERE id='${single.versionId}'`);
assert.equal(approval.schemaVersion, 2);
assert.equal(approval.parties.find((party) => party.id === pendingPartyId).displayName, 'Nombre Revisado E2E');
assert.equal(approval.parties.find((party) => party.id === pendingPartyId).identifiers[0].identifier_value, 'synthetic:collaborator:v1');
assert.equal((await transition(admin.token, 'music-e2e-approved', 'approved')).state, 'approved');
assert.equal(sql(`SELECT snapshot_sha256 FROM music_release_version WHERE id='${single.versionId}'`), approvalHash);

const releaseAt = new Date(Date.now() + 3000).toISOString();
assert.equal((await transition(admin.token, 'music-e2e-scheduled', 'scheduled', {
  releaseAtUtc: releaseAt,
  releaseTimezone: 'America/Guayaquil',
  embargoUntilUtc: releaseAt,
})).state, 'scheduled');

await request('/music/releases/single-musica-e2e', { country: 'EC', expected: 404 });
await sleep(3400);
assert.equal(sql('SELECT count(*) FROM music_publish_due(10);'), '1');
assert.equal(sql('SELECT count(*) FROM music_publish_due(10);'), '0');

assert.equal((await request('/music/releases', { country: 'US' })).length, 0);
assert.equal((await request('/music/releases')).length, 0);
const publicCatalog = await request('/music/releases', { country: 'EC' });
assert.equal(publicCatalog.length, 1);
const publicRelease = await request('/music/releases/single-musica-e2e', { country: 'EC' });
assert.equal(publicRelease.id, single.id);
assert.equal(publicRelease.tracks.length, 1);

const coverAccess = await request(`/music/assets/${coverDisplayAssetId}/access`, { country: 'EC' });
assert.match(coverAccess.url, /^https:\/\/127\.0\.0\.1:\d+\/music-e2e-derivative\//);
const streamAccess = await request(`/music/assets/${streamAssetId}/access`, { country: 'EC' });
if (realS3) {
  for (const [id, access] of [[coverDisplayAssetId, coverAccess], [streamAssetId, streamAccess]]) {
    await assertStoredAsset(access.url, realAssets.assets.find((asset) => asset.id === id).sha256);
  }
  const currentPreview = JSON.parse(sql(`SELECT json_build_object('id',id,'sha256',sha256) FROM music_asset WHERE release_version_id='${single.versionId}' AND music_preview_matches(id)`));
  const previewAccess = await request(`/music/assets/${currentPreview.id}/access`, { country: 'EC' });
  await assertStoredAsset(previewAccess.url, currentPreview.sha256, 1750);
  const oldPreview = realAssets.assets.find((asset) => asset.role === 'preview_audio');
  await request(`/music/assets/${oldPreview.id}/access`, { country: 'EC', expected: 404 });
  assert.deepEqual(publicRelease.tracks[0].sources.filter((source) => source.role === 'preview_audio').map((source) => source.assetId), [currentPreview.id]);
  console.log('PASS published preview → configured duration and old preview excluded/denied');
  console.log('PASS published API access → original worker bytes over HTTPS and exact ranges');
} else assert.match(streamAccess.url, /streams\/e2e/);
await request(`/music/assets/${coverDisplayAssetId}/access`, { country: 'US', expected: 404 });
await request(`/music/assets/${coverDisplayAssetId}/access`, { expected: 404 });
await request(`/music/assets/${masterAssetId}/access`, { country: 'EC', expected: 404 });

if (process.env.TDF_MUSIC_API_E2E_BROWSER === '1') {
  assert.ok(realS3, 'Browser integration requires actual processed objects');
  await runMusicBrowserProbe({ apiBase, storageEndpoint: process.env.TDF_MUSIC_API_E2E_S3_ENDPOINT,
    password, masterAssetId, recordingId });
}

const anonymousEvent = {
  eventId: randomUUID(),
  sessionId: randomUUID(),
  sequenceNumber: 1,
  anonymousId: 'anonymous-music-e2e-opaque-id',
  releaseVersionId: single.versionId,
  recordingId,
  eventType: 'progress',
  positionMs: 31000,
  listenedDeltaMs: 30000,
  quality: 'high',
  territoryCode: 'US',
  occurredAt: new Date().toISOString(),
  metadata: { source: 'real-api-e2e' },
};
await request('/music/playback-events', { method: 'POST', country: 'EC', json: anonymousEvent });
await request('/music/playback-events', { method: 'POST', country: 'EC', json: anonymousEvent });
await request('/music/playback-events', { method: 'POST', expected: 404, country: 'US', json: {
  ...anonymousEvent,
  eventId: randomUUID(),
  sequenceNumber: 2,
} });

await request('/music/favorites', {
  token: member.token,
  method: 'PUT',
  expected: 200,
  country: 'EC',
  json: { recordingId },
});
await request('/music/favorites', {
  token: outsider.token,
  method: 'PUT',
  expected: 404,
  country: 'US',
  json: { recordingId },
});
assert.equal((await request('/music/favorites', { token: member.token, country: 'EC' })).length, 1);
const playlist = await request('/music/playlists', {
  token: member.token,
  method: 'POST',
  expected: 201,
  json: { name: 'Playlist Música E2E', visibility: 'private' },
});
const playlistItem = await request(`/music/playlists/${playlist.id}/items`, {
  token: member.token,
  method: 'POST',
  expected: 201,
  country: 'EC',
  json: { recordingId, position: 0 },
});
assert.equal(playlistItem.position, 0);
assert.equal((await request('/music/playlists', { token: member.token, country: 'EC' }))[0].items.length, 1);
assert.equal((await request('/music/playlists', { token: member.token, country: 'US' }))[0].items[0].available, false);

const authenticatedEvent = {
  ...anonymousEvent,
  eventId: randomUUID(),
  sessionId: randomUUID(),
  anonymousId: null,
  eventType: 'play_start',
  positionMs: 0,
  listenedDeltaMs: 0,
};
await request('/music/me/playback-events', {
  token: member.token,
  method: 'POST',
  expected: 200,
  country: 'EC',
  json: authenticatedEvent,
});
assert.equal((await request('/music/history', { token: member.token, country: 'EC' })).length, 1);
assert.equal((await request('/music/history', { token: member.token, country: 'US' }))[0].available, false);
await probePlaybackIdentity({ request, sql, anonymousEvent, authenticatedEvent, member, outsider });

const purchase = await request('/music/purchases', {
  token: member.token,
  method: 'POST',
  expected: 201,
  country: 'EC',
  idempotencyKey: 'music-e2e-purchase-order',
  json: {
    availabilityRuleId,
    territoryCode: 'US',
    termsVersion: 'music-download-e2e-v1',
  },
});
const repeatedPurchase = await request('/music/purchases', {
  token: member.token,
  method: 'POST',
  expected: 201,
  country: 'EC',
  idempotencyKey: 'music-e2e-purchase-order',
  json: {
    availabilityRuleId,
    territoryCode: 'US',
    termsVersion: 'music-download-e2e-v1',
  },
});
assert.equal(repeatedPurchase.id, purchase.id);
assert.equal(purchase.grossMinor, 250);
assert.equal(purchase.currency, 'USD');

const paymentAttemptId = randomUUID();
sql(`
  INSERT INTO commerce_payment_attempt(
    id,checkout_id,provider,environment,operation,status,amount_minor,currency,
    merchant_account_ref,idempotency_key
  ) VALUES (
    '${paymentAttemptId}'::uuid,'${purchase.checkoutId}'::uuid,'datafast','sandbox',
    'capture','succeeded',250,'USD','synthetic-e2e-merchant','music-e2e-capture'
  );
  INSERT INTO commerce_provider_binding(
    payment_attempt_id,provider,environment,merchant_account_ref,resource_type,
    provider_resource_id,provider_resource_path,merchant_reference,amount_minor,currency
  ) VALUES (
    '${paymentAttemptId}'::uuid,'datafast','sandbox','synthetic-e2e-merchant','payment',
    'synthetic-e2e-payment','/v1/checkouts/synthetic-e2e/payment','${purchase.id}',250,'USD'
  );
  UPDATE commerce_checkout_session
  SET status='paid',paid_minor=250,paid_at=NOW()
  WHERE id='${purchase.checkoutId}'::uuid;
`);
let entitlements = await request('/music/entitlements', { token: member.token });
assert.equal(entitlements.length, 1);
assert.equal(entitlements[0].status, 'active');
const downloadRequestId = randomUUID();
await request(`/music/entitlements/${entitlements[0].id}/download`, {
  token: outsider.token, method: 'POST', expected: 404,
  json: { requestId: randomUUID() },
});
const download = await request(`/music/entitlements/${entitlements[0].id}/download`, {
  token: member.token,
  method: 'POST',
  json: { requestId: downloadRequestId },
});
assert.match(download.url, /music-e2e-master/);
if (realS3) {
  await assertStoredAsset(download.url, realAssets.masterSha256);
  console.log('PASS entitlement download → intact original master bytes over HTTPS');
}
await request(`/music/entitlements/${entitlements[0].id}/download`, {
  token: member.token,
  method: 'POST',
  json: { requestId: downloadRequestId },
});
entitlements = await request('/music/entitlements', { token: member.token });
assert.equal(entitlements[0].downloadCount, 1);

sql(`
  UPDATE commerce_checkout_session
  SET status='refunded',refunded_minor=250,updated_at=NOW()
  WHERE id='${purchase.checkoutId}'::uuid;
`);
entitlements = await request('/music/entitlements', { token: member.token });
assert.equal(entitlements[0].status, 'refunded');
await request(`/music/entitlements/${entitlements[0].id}/download`, {
  token: member.token,
  method: 'POST',
  expected: 404,
  json: { requestId: randomUUID() },
});

const correction = await request(
  `/music/releases/${single.id}/versions/${single.versionId}/corrections`,
  {
    token: artist.token,
    method: 'POST',
    expected: 201,
    idempotencyKey: 'music-e2e-correction',
  },
);
assert.equal(correction.state, 'draft');
assert.equal(correction.versionNumber, 2);
assert.equal(correction.tracks.length, 1);
assert.deepEqual(new Set(correction.parties.map((party) => party.id)), new Set([rightsHolderId, pendingPartyId]));
assert.equal(sql(`SELECT count(*) FROM music_release_version_party WHERE release_version_id='${single.versionId}'`), '2');
assert.deepEqual(correction.parties, approval.parties);
const correctedParties = retainedParties.map((party) => party.partyId === pendingPartyId
  ? { ...party, displayName: 'Nombre Solo Corrección', legalName: null, identifiers: [] }
  : party);
const editedCorrection = await request(`/music/releases/${single.id}/versions/${correction.id}/content`, {
  token: member.token, method: 'PUT', json: {
    expectedUpdatedAt: correction.updatedAt,
    tracks: [{ ...trackDraft(correction.tracks[0].recordingId),
      previewStartMs: correction.tracks[0].previewStartMs,
      previewDurationMs: correction.tracks[0].previewDurationMs }],
    parties: correctedParties, credits, identifiers, rightsDeclarations: rights,
    availability: availability(),
  },
});
const correctedParty = editedCorrection.parties.find((party) => party.id === pendingPartyId);
assert.equal(correctedParty.displayName, 'Nombre Solo Corrección');
assert.equal(correctedParty.legalName, null);
assert.deepEqual(correctedParty.identifiers, []);
const originalGraph = await request(`/music/releases/${single.id}/versions/${single.versionId}`, { token: artist.token });
assert.deepEqual(originalGraph.parties, approval.parties);
assert.equal(sql(`SELECT snapshot_sha256 FROM music_release_version WHERE id='${single.versionId}'`), approvalHash);
assert.deepEqual(JSON.parse(sql(`SELECT immutable_snapshot FROM music_release_version WHERE id='${single.versionId}'`)), approval);
console.log('PASS API correction → editable party details, identifier removal and unchanged approved snapshot/hash');

// The real compiled renderer reads the approved snapshot from this disposable
// database. No official identifiers or DPID verification are asserted by fixtures.
const ddexParties = correctedParties.map((party) => party.partyId === pendingPartyId
  ? { ...party, identifiers: [{ type: 'ipi', value: '00000000001' }] } : party);
const ddexCredits = [...credits,
  { partyRef: 'uncredited-collaborator', trackRef: 'track-1', role: 'lyricist', displayOrder: 2, notes: null }];
await request(`/music/releases/${single.id}/versions/${correction.id}/content`, {
  token: member.token, method: 'PUT', json: {
    expectedUpdatedAt: editedCorrection.updatedAt,
    tracks: [{ ...trackDraft(correction.tracks[0].recordingId),
      previewStartMs: correction.tracks[0].previewStartMs,
      previewDurationMs: correction.tracks[0].previewDurationMs }],
    parties: ddexParties, credits: ddexCredits, identifiers, rightsDeclarations: rights,
    availability: availability().map(rule => ({ ...rule, endsAt: '2030-01-01T00:00:00Z' })),
  },
});
await request(`/music/releases/${single.id}/versions/${correction.id}/terms`, {
  token: artist.token, method: 'POST', expected: 201,
  json: { kind: 'publication_authority', version: 'publication-authority-e2e-v1', accepted: true,
    evidence: { fixture: 'synthetic-ddex-only', authorityConfirmed: true } },
});
await request(`/music/releases/${single.id}/versions/${correction.id}/validate`, { token: artist.token, method: 'POST' });
for (const state of ['ready_for_review', 'in_review', 'approved']) {
  await request(`/music/releases/${single.id}/versions/${correction.id}/transition`, {
    token: state === 'ready_for_review' ? artist.token : admin.token, method: 'POST',
    idempotencyKey: `music-e2e-ddex-${state}`, json: { targetState: state },
  });
}
const counterparties = [];
for (const [role, dpid] of [['sender', 'PADPIDA000TDF001'], ['recipient', 'PADPIDA000DSP001']]) {
  counterparties.push(await request('/music/ddex/parties', {
    token: admin.token, method: 'POST', expected: 201,
    json: { partyName: `Synthetic ${role}`, partyDpid: dpid, partyRole: role,
      verificationAuthority: 'synthetic-test-only', verificationEvidence: { fixture: true } },
  }));
}
const exportPath = `/music/releases/${single.id}/versions/${correction.id}/ddex-exports`;
const exportRequest = { token: admin.token, method: 'POST', expected: 201,
  idempotencyKey: 'music-e2e-ddex-export', json: { operation: 'new_release',
    senderRegistryId: counterparties[0].id, recipientRegistryId: counterparties[1].id } };
const beforeInitial = await request(exportPath, { ...exportRequest, expected: 422,
  idempotencyKey: 'music-e2e-update-without-initial', json: { ...exportRequest.json, operation: 'update' } });
assert.ok(beforeInitial.errors.some(error => error.code === 'initial_export_missing'));
const ddexExport = await probeDdexEnqueue({ request, sql, database,
  path: exportPath, payload: exportRequest, versionId: correction.id });
assert.equal((await request(exportPath, exportRequest)).id, ddexExport.id);
await request(exportPath, { ...exportRequest, expected: 409,
  json: { ...exportRequest.json, operation: 'update' } });
await request(exportPath, { ...exportRequest, expected: 409,
  json: { ...exportRequest.json, recipientRegistryId: counterparties[0].id } });
assert.equal(sql(`SELECT count(*) FROM music_processing_job WHERE job_kind='generate_ddex' AND job_key='${ddexExport.id}'`), '1');
sql(`UPDATE music_party SET display_name='LIVE DIRECTORY MUST NOT LEAK' WHERE id='${pendingPartyId}'`);
const ddexEvidence = mkdtempSync(join(tmpdir(), 'tdf-ddex-api-evidence-'));
const ddexXml = join(ddexEvidence, 'release.xml');
const renderer = join(dirname(process.env.TDF_MUSIC_API_E2E_BACKEND_EXE), 'tdf-ddex-render');
execFileSync(renderer, [ddexExport.id, ddexXml, join(ddexEvidence, 'resources.tsv')], {
  env: { ...process.env, DATABASE_URL: `dbname=${database}` }, encoding: 'utf8', timeout: 30000,
});
const xmlText = readFileSync(ddexXml, 'utf8');
assert.ok(xmlText.includes('Nombre Solo Corrección'));
assert.ok(!xmlText.includes('LIVE DIRECTORY MUST NOT LEAK'));
assert.ok(xmlText.includes('<IpiNameNumber>00000000001</IpiNameNumber>'));
assert.ok(xmlText.includes('<Role><Value>Lyricist</Value></Role>'));
assert.ok(xmlText.includes('<Role><Value>Artist</Value></Role><Role><Value>Composer</Value></Role>'));
assert.ok(xmlText.includes('<EndDateTime>2030-01-01T00:00:00Z</EndDateTime>'));
assert.equal(ddexExport.status, 'queued', 'Rendering XML alone is not a completed/validated package');
console.log(`PASS API DDEX → approved snapshot, idempotent export, real renderer and frozen party data; XML evidence ${ddexEvidence}`);
let initialDdexBundle;
if (realS3 && process.env.TDF_MUSIC_API_E2E_DDEX_SCHEMA) {
  initialDdexBundle = await assertRealDdexPackage({ request, sql, exportId: ddexExport.id, admin, outsider, evidence: ddexEvidence });
}

// Clone the approved correction, including preview -> stream -> master.
const unsupported = await request(`/music/releases/${single.id}/versions/${correction.id}/corrections`, {
  token: artist.token, method: 'POST', expected: 201,
  idempotencyKey: 'music-e2e-ddex-unsupported-correction',
});
assert.equal(unsupported.versionNumber, 3);
const correctionReplay = await request(`/music/releases/${single.id}/versions/${correction.id}/corrections`, {
  token: artist.token, method: 'POST', expected: 201,
  idempotencyKey: 'music-e2e-ddex-unsupported-correction',
});
assert.equal(correctionReplay.id, unsupported.id);
assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${unsupported.id}'`),
  sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${correction.id}' AND asset_role NOT IN ('ddex_xml','ddex_manifest','ddex_package')`));
assert.equal(sql(`SELECT bool_and(copied.party_details=source.party_details AND copied.details_source=source.details_source) FROM music_release_version_party copied JOIN music_release_version_party source ON source.music_party_id=copied.music_party_id AND source.release_version_id='${correction.id}' WHERE copied.release_version_id='${unsupported.id}'`), 't');
assert.equal(sql(`SELECT count(*) FROM music_asset child JOIN music_asset parent ON parent.id=child.parent_asset_id WHERE child.release_version_id='${unsupported.id}' AND parent.release_version_id<>'${unsupported.id}'`), '0');
assert.equal(sql(`SELECT count(*) FROM music_asset cloned JOIN music_asset source ON source.id=(cloned.provenance->>'correctionSourceAssetId')::uuid WHERE cloned.release_version_id='${unsupported.id}' AND source.release_version_id='${correction.id}' AND (cloned.sha256<>source.sha256 OR cloned.object_key<>source.object_key OR cloned.bucket_name<>source.bucket_name)`), '0');
console.log('PASS API chained correction → third version, local parent references and unchanged immutable bytes');
await request(`/music/releases/${single.id}/versions/${unsupported.id}/content`, {
  token: artist.token, method: 'PUT', json: {
    expectedUpdatedAt: unsupported.updatedAt,
    tracks: [{ ...trackDraft(unsupported.tracks[0].recordingId),
      previewStartMs: unsupported.tracks[0].previewStartMs,
      previewDurationMs: unsupported.tracks[0].previewDurationMs }],
    parties: ddexParties, credits: [...ddexCredits,
      { partyRef: 'uncredited-collaborator', trackRef: 'track-1', role: 'other', displayOrder: 3, notes: 'Unmapped synthetic credit' }],
    identifiers, rightsDeclarations: rights, availability: availability(),
  },
});
await request(`/music/releases/${single.id}/versions/${unsupported.id}/terms`, {
  token: artist.token, method: 'POST', expected: 201,
  json: { kind: 'publication_authority', version: 'publication-authority-e2e-v1', accepted: true,
    evidence: { fixture: 'synthetic-ddex-negative', authorityConfirmed: true } },
});
await request(`/music/releases/${single.id}/versions/${unsupported.id}/validate`, { token: artist.token, method: 'POST' });
for (const state of ['ready_for_review', 'in_review', 'approved']) {
  await request(`/music/releases/${single.id}/versions/${unsupported.id}/transition`, {
    token: state === 'ready_for_review' ? artist.token : admin.token, method: 'POST',
    idempotencyKey: `music-e2e-ddex-negative-${state}`, json: { targetState: state },
  });
}
const rejectedExport = await request(`/music/releases/${single.id}/versions/${unsupported.id}/ddex-exports`, {
  ...exportRequest, expected: 422, idempotencyKey: 'music-e2e-ddex-unsupported-export',
});
await request(`/music/releases/${single.id}/versions/${unsupported.id}/ddex-exports`, {
  ...exportRequest, expected: 409,
});
assert.ok(rejectedExport.errors.some((error) => error.code === 'unsupported_credit_role' && error.fieldPath.startsWith('credits[')));
assert.equal(sql(`SELECT count(*) FROM music_ddex_export WHERE release_version_id='${unsupported.id}'`), '0');
assert.equal(sql(`SELECT count(*) FROM music_processing_job WHERE release_version_id='${unsupported.id}' AND job_kind='generate_ddex'`), '0');
console.log('PASS API DDEX → field-level 422 for unmapped approved credit; no export or job created');

// Distinct keys/sources must serialize only allocation, while identical keys
// must create exactly one graph and one audit event.
const concurrentBase = Number(sql(`SELECT max(version_number) FROM music_release_version WHERE release_id='${single.id}'`));
const simultaneous = await Promise.all(Array.from({ length: 4 }, (_, index) =>
  request(`/music/releases/${single.id}/versions/${index % 2 ? correction.id : single.versionId}/corrections`, {
    token: index % 2 ? member.token : artist.token, method: 'POST', expected: 201,
    idempotencyKey: `music-e2e-concurrent-${index}`,
  })));
assert.equal(new Set(simultaneous.map((version) => version.id)).size, 4);
assert.deepEqual(simultaneous.map((version) => version.versionNumber).sort((a, b) => a - b),
  [1, 2, 3, 4].map((offset) => concurrentBase + offset));
const replayPath = `/music/releases/${single.id}/versions/${correction.id}/corrections`;
const parallelReplays = await Promise.all(Array.from({ length: 4 }, () =>
  request(replayPath, { token: artist.token, method: 'POST', expected: 201,
    idempotencyKey: 'music-e2e-parallel-replay' })));
assert.equal(new Set(parallelReplays.map((version) => version.id)).size, 1);
assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_id='${single.id}'
  AND idempotency_key='music-e2e-parallel-replay' AND event_type='correction_created'`), '1');
assert.equal(Number(sql(`SELECT max(version_number) FROM music_release_version WHERE release_id='${single.id}'`)),
  concurrentBase + 5);
const beforeConflict = sql(`SELECT count(*) FROM music_release_version WHERE release_id='${single.id}'`);
await request(`/music/releases/${single.id}/versions/${single.versionId}/corrections`, {
  token: artist.token, method: 'POST', expected: 409, idempotencyKey: 'music-e2e-parallel-replay',
});
assert.equal(sql(`SELECT count(*) FROM music_release_version WHERE release_id='${single.id}'`), beforeConflict);
console.log('PASS API concurrent corrections → consecutive versions, team permissions, one replay graph/audit and source-bound keys');

// Explicit legacy-corruption fixture, never published or served: insert while
// draft, approve through the real API, then ensure the failed clone rolls back.
const malformed = await request(replayPath, {
  token: artist.token, method: 'POST', expected: 201, idempotencyKey: 'music-e2e-malformed-source',
});
const malformedAsset = randomUUID();
sql(`INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
  storage_provider,bucket_name,object_key,media_type,byte_size,sha256,processing_state,
  created_by,ready_at,provenance)
  SELECT '${malformedAsset}',release_version_id,recording_id,id,'preview_audio',
    storage_provider,bucket_name,'synthetic/legacy-cycle-do-not-serve','audio/mp4',1,
    repeat('e',64),'ready',created_by,NOW(),'{"fixture":"legacy-corruption"}'::jsonb
  FROM music_asset WHERE release_version_id='${malformed.id}' AND asset_role='master_audio';
  UPDATE music_asset SET parent_asset_id=id WHERE id='${malformedAsset}';`);
await request(`/music/releases/${single.id}/versions/${malformed.id}/terms`, {
  token: artist.token, method: 'POST', expected: 201,
  json: { kind: 'publication_authority', version: 'publication-authority-e2e-v1', accepted: true,
    evidence: { fixture: 'synthetic-legacy-corruption', authorityConfirmed: true } },
});
await request(`/music/releases/${single.id}/versions/${malformed.id}/validate`, {
  token: artist.token, method: 'POST',
});
for (const state of ['ready_for_review', 'in_review', 'approved']) {
  await request(`/music/releases/${single.id}/versions/${malformed.id}/transition`, {
    token: state === 'ready_for_review' ? artist.token : admin.token, method: 'POST',
    idempotencyKey: `music-e2e-malformed-${state}`, json: { targetState: state },
  });
}
// Upgrade around an already-approved malformed legacy graph. This intentional
// late migration preserves the pre-upgrade corruption regression above.
const legacySnapshot = sql(`SELECT snapshot_sha256 FROM music_release_version WHERE id='${malformed.id}'`);
// Queue through the real API before upgrading, then prove the worker uses the
// upgraded gate. No invalid fixture resource is ever read or published.
const legacyQueuedExport = await request(`/music/releases/${single.id}/versions/${malformed.id}/ddex-exports`, {
  ...exportRequest, idempotencyKey: 'music-e2e-legacy-queued-export',
});
execFileSync('psql', ['-X', '-v', 'ON_ERROR_STOP=1', '-d', database, '-f',
  fileURLToPath(new URL('../tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql', import.meta.url))],
{ encoding: 'utf8' });
assert.equal(sql(`SELECT snapshot_sha256 FROM music_release_version WHERE id='${malformed.id}'`), legacySnapshot);
assert.equal(sql(`SELECT count(*) FROM music_resource_graph_sanitation_queue
  WHERE release_version_id='${malformed.id}' AND error_code='resource_graph_unrooted'`), '1');
const legacyValidation = await request(`/music/releases/${single.id}/versions/${malformed.id}/validate`,
  { token: artist.token, method: 'POST' });
assert.equal(legacyValidation.valid, false);
assert.ok(legacyValidation.errors.some((error) => error.code === 'resource_graph_unrooted'));
const deniedSchedule = await request(`/music/releases/${single.id}/versions/${malformed.id}/transition`, {
  token: admin.token, method: 'POST', expected: 422, idempotencyKey: 'music-e2e-bad-graph-schedule',
  json: { targetState: 'scheduled', releaseAtUtc: new Date(Date.now() + 60000).toISOString(),
    releaseTimezone: 'America/Guayaquil' },
});
assert.ok(deniedSchedule.errors.some((error) => error.code === 'resource_graph_unrooted'));
assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${malformed.id}'`), 'approved');
const graphExport = await request(`/music/releases/${single.id}/versions/${malformed.id}/ddex-exports`, {
  ...exportRequest, expected: 422, idempotencyKey: 'music-e2e-bad-graph-export',
});
assert.ok(graphExport.errors.some((error) => error.code === 'resource_graph_unrooted'));
assert.equal(sql(`SELECT count(*) FROM music_ddex_export WHERE release_version_id='${malformed.id}'`), '1',
  'Denied request must not create an export beyond the pre-upgrade queued one');
if (realS3) {
  // Prioritize only this synthetic job; leave unrelated queued exports intact.
  const previousExport = sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${ddexExport.id}'`);
  sql(`UPDATE music_processing_job SET run_after=NOW()-INTERVAL '1 day'
    WHERE job_kind='generate_ddex' AND job_key='${legacyQueuedExport.id}'`);
  await assert.rejects(runRealPreviewWorker(), /DDEX preconditions failed/);
  const rejected = JSON.parse(sql(`SELECT validation_report FROM music_ddex_export WHERE id='${legacyQueuedExport.id}'`));
  assert.equal(rejected.code, 'ddex_preconditions_failed');
  assert.ok(rejected.errors.some(issue => issue.code === 'resource_graph_unrooted'));
  assert.equal(sql(`SELECT status||':'||attempt_count FROM music_processing_job
    WHERE job_kind='generate_ddex' AND job_key='${legacyQueuedExport.id}'`), 'retry:1');
  assert.equal(sql(`SELECT count(*) FROM music_asset WHERE release_version_id='${malformed.id}' AND asset_role LIKE 'ddex_%'`), '0');
  assert.equal(sql(`SELECT row_to_json(e) FROM music_ddex_export e WHERE id='${ddexExport.id}'`), previousExport);
  console.log('PASS real worker queued DDEX → upgraded graph rejected before rendering/storage, safe field report and unrelated export unchanged');
}

const graphDraft = await request(replayPath, {
  token: artist.token, method: 'POST', expected: 201, idempotencyKey: 'music-e2e-graph-draft',
});
const draftFaultId = randomUUID();
sql(`INSERT INTO music_asset(id,release_version_id,recording_id,parent_asset_id,asset_role,
  storage_provider,bucket_name,object_key,media_type,byte_size,sha256,processing_state,created_by,ready_at)
  SELECT '${draftFaultId}',release_version_id,recording_id,'${draftFaultId}','preview_audio',
    storage_provider,bucket_name,'synthetic/draft-cycle-do-not-serve','audio/mp4',1,
    repeat('e',64),'ready',created_by,NOW() FROM music_asset
  WHERE release_version_id='${graphDraft.id}' AND asset_role='master_audio';`);
await request(`/music/releases/${single.id}/versions/${graphDraft.id}/terms`, {
  token: artist.token, method: 'POST', expected: 201,
  json: { kind: 'publication_authority', version: 'publication-authority-e2e-v1', accepted: true,
    evidence: { fixture: 'synthetic-editorial-graph', authorityConfirmed: true } },
});
for (let attempt = 0; attempt < 2; attempt += 1) {
  const invalidDraft = await request(`/music/releases/${single.id}/versions/${graphDraft.id}/validate`,
    { token: member.token, method: 'POST' });
  assert.equal(invalidDraft.valid, false);
  assert.ok(invalidDraft.errors.some((error) => error.fieldPath === `assets.${draftFaultId}.parentAssetId`));
  const rejected = await request(`/music/releases/${single.id}/versions/${graphDraft.id}/transition`, {
    token: artist.token, method: 'POST', expected: 422, idempotencyKey: 'music-e2e-graph-submit',
    json: { targetState: 'ready_for_review' },
  });
  assert.ok(rejected.errors.some((error) => error.code === 'resource_graph_unrooted'));
  assert.doesNotMatch(JSON.stringify(rejected), /draft-cycle-do-not-serve|SELECT|INSERT/);
}
assert.equal(sql(`SELECT state FROM music_release_version WHERE id='${graphDraft.id}'`), 'draft');
assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_version_id='${graphDraft.id}'
  AND idempotency_key='music-e2e-graph-submit'`), '0');
// Repair only synthetic provenance, not master bytes. Real sanitation requires
// operator evidence; this is not an upload/reprocessing UI implementation.
sql(`UPDATE music_asset SET parent_asset_id=(SELECT id FROM music_asset
  WHERE release_version_id='${graphDraft.id}' AND asset_role='master_audio')
  WHERE id='${draftFaultId}';`);
const fixedDraft = await request(`/music/releases/${single.id}/versions/${graphDraft.id}/validate`,
  { token: artist.token, method: 'POST' });
assert.equal(fixedDraft.valid, true);
for (const state of ['ready_for_review', 'in_review', 'approved']) {
  await request(`/music/releases/${single.id}/versions/${graphDraft.id}/transition`, {
    token: state === 'ready_for_review' ? artist.token : admin.token, method: 'POST',
    idempotencyKey: state === 'ready_for_review' ? 'music-e2e-graph-submit' : `music-e2e-graph-${state}`,
    json: { targetState: state },
  });
}
assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_version_id='${graphDraft.id}'
  AND idempotency_key='music-e2e-graph-submit'`), '1');
console.log('PASS API editorial graph → field errors, no transition on retry, repair/review/approval and unchanged legacy snapshot with scheduling/export blocked');
const correctionCounts = () => sql(`SELECT json_build_array(
  (SELECT count(*) FROM music_release_version),(SELECT count(*) FROM music_asset),
  (SELECT count(*) FROM music_recording),(SELECT count(*) FROM music_credit),
  (SELECT count(*) FROM music_release_version_party),(SELECT count(*) FROM music_release_audit_event))`);
const beforeMalformed = correctionCounts();
for (let attempt = 0; attempt < 2; attempt += 1) {
  const failure = await request(`/music/releases/${single.id}/versions/${malformed.id}/corrections`, {
    token: artist.token, method: 'POST', expected: 422, idempotencyKey: 'music-e2e-invalid-graph-retry',
  });
  assert.equal(failure.errors[0].code, 'correction_resource_graph_invalid');
  assert.equal(failure.errors[0].fieldPath, 'assets');
  assert.match(failure.errors[0].message, /soporte/);
  assert.doesNotMatch(JSON.stringify(failure), /legacy-cycle|SELECT|INSERT|music_asset_check/);
  assert.equal(correctionCounts(), beforeMalformed);
}
await request(`/music/releases/${single.id}/versions/${malformed.id}/corrections`, {
  token: outsider.token, method: 'POST', expected: 403, idempotencyKey: 'music-e2e-invalid-graph-outsider',
});
console.log('PASS API invalid legacy graph → actionable sanitized 422, safe retry, atomic rollback and outsider denial');

if (initialDdexBundle) {
  const lifecyclePath = `/music/releases/${single.id}/versions/${graphDraft.id}/ddex-exports`;
  for (const [role, dpid, field] of [['sender','PADPIDA000TDF002','senderRegistryId'],
    ['recipient','PADPIDA000DSP002','recipientRegistryId']]) {
    const party = await request('/music/ddex/parties', { token: admin.token, method: 'POST', expected: 201,
      json: { partyName: `Synthetic alternative ${role}`, partyDpid: dpid, partyRole: role,
        verificationAuthority: 'synthetic-test-only', verificationEvidence: { fixture: true } } });
    const rejected = await request(lifecyclePath, { ...exportRequest, expected: 422,
      idempotencyKey: `music-e2e-wrong-${role}`, json: { ...exportRequest.json, operation: 'update', [field]: party.id } });
    assert.ok(rejected.errors.some(error => error.code === 'initial_export_missing'));
  }
  const updateRequest = { ...exportRequest, idempotencyKey: 'music-e2e-real-update',
    json: { ...exportRequest.json, operation: 'update' } };
  const update = await request(lifecyclePath, updateRequest);
  assert.equal((await request(lifecyclePath, updateRequest)).id, update.id);
  const updated = await assertRealDdexPackage({ request, sql, exportId: update.id, admin, outsider,
    evidence: ddexEvidence, operation: 'update' });
  const xmlValue = (xml, path) => execFileSync('xmllint', ['--nonet', '--xpath', `string(${path})`, '-'],
    { input: xml, encoding: 'utf8' }).trim();
  const trackIdentity = '//TrackRelease/ReleaseId/ProprietaryId';
  assert.match(xmlValue(initialDdexBundle.xml, trackIdentity), /^TDF-TRACK-.+-[A-Z]{2}[A-Z0-9]{3}\d{7}$/);
  assert.ok(xmlValue(initialDdexBundle.xml, '//MessageThreadId'), 'Initial message thread must exist');
  assert.equal(xmlValue(updated.xml, trackIdentity), xmlValue(initialDdexBundle.xml, trackIdentity));
  assert.equal(xmlValue(updated.xml, '//MessageThreadId'), xmlValue(initialDdexBundle.xml, '//MessageThreadId'));
  assert.notEqual(xmlValue(updated.xml, '//MessageId'), xmlValue(initialDdexBundle.xml, '//MessageId'));
  assert.notEqual(updated.exported.canonical_snapshot_sha256, initialDdexBundle.exported.canonical_snapshot_sha256);
  await request(`/music/releases/${single.id}/versions/${graphDraft.id}/transition`, {
    token: admin.token, method: 'POST', idempotencyKey: 'music-e2e-ddex-suspend',
    json: { targetState: 'suspended', reason: 'Synthetic takedown verification' },
  });
  const takedownRequest = { ...exportRequest, idempotencyKey: 'music-e2e-real-ddex-takedown',
    json: { ...exportRequest.json, operation: 'takedown' } };
  const takedown = await request(lifecyclePath, takedownRequest);
  assert.equal((await request(lifecyclePath, takedownRequest)).id, takedown.id);
  const withdrawn = await assertRealDdexPackage({ request, sql, exportId: takedown.id, admin, outsider,
    evidence: ddexEvidence, operation: 'takedown' });
  assert.equal(xmlValue(withdrawn.xml, trackIdentity), xmlValue(initialDdexBundle.xml, trackIdentity));
  assert.equal(withdrawn.exported.canonical_snapshot_sha256, updated.exported.canonical_snapshot_sha256);
  assert.notEqual(withdrawn.exported.package_sha256, updated.exported.package_sha256);
  assert.equal(sql(`SELECT package_sha256 FROM music_ddex_export WHERE id='${ddexExport.id}'`),
    initialDdexBundle.exported.package_sha256);
  assert.equal(sql(`SELECT package_sha256 FROM music_ddex_export WHERE id='${update.id}'`), updated.exported.package_sha256);
  console.log('PASS DDEX lifecycle → initial/update/suspended takedown real ZIPs, stable track identities, counterparties, snapshots and immutable prior packages');
}

const takedownAt = new Date(Date.now() + 1200).toISOString();
assert.equal((await transition(admin.token, 'music-e2e-takedown', 'takedown_scheduled', {
  takedownAtUtc: takedownAt,
  takedownTimezone: 'America/Guayaquil',
})).state, 'takedown_scheduled');
await sleep(1500);
assert.equal(sql('SELECT count(*) FROM music_withdraw_due(10);'), '1');
assert.equal(sql('SELECT count(*) FROM music_withdraw_due(10);'), '0');
await request('/music/releases/single-musica-e2e', { country: 'EC', expected: 404 });
await request(`/music/assets/${streamAssetId}/access`, { country: 'EC', expected: 404 });

assert.equal(sql(`SELECT count(*) FROM music_playback_event WHERE event_id='${anonymousEvent.eventId}'::uuid;`), '1');
assert.equal(sql(`SELECT state FROM music_purchase_order WHERE id='${purchase.id}'::uuid;`), 'refunded');
assert.equal(sql(`SELECT count(*) FROM music_release_audit_event WHERE release_id='${single.id}'::uuid;`) > 10, true);

console.log('Music release real API E2E assertions passed.');
