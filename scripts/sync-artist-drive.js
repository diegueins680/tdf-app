#!/usr/bin/env node
/**
 * Syncs artist folders and featured video previews from Google Drive.
 *
 * Requirements:
 * - Service account with Drive access to the parent folder.
 * - Env:
 *   GDRIVE_CLIENT_EMAIL
 *   GDRIVE_PRIVATE_KEY (use literal newlines or escaped \n)
 *   GDRIVE_PARENT_ID (defaults to the TDF parent folder id)
 *   API_BASE (e.g. https://hq.tudominio.com/api)
 *   API_TOKEN (admin bearer token)
 *
 * Run manually or via cron:
 *   GDRIVE_CLIENT_EMAIL=... GDRIVE_PRIVATE_KEY="..." API_BASE=... API_TOKEN=... node scripts/sync-artist-drive.js
 */
const crypto = require('crypto');

const DRIVE_PARENT_ID = process.env.GDRIVE_PARENT_ID || '1_ScZjmwmOuBX_325JgFocZ-QlfFEIMax';
const CLIENT_EMAIL = process.env.GDRIVE_CLIENT_EMAIL;
const PRIVATE_KEY = process.env.GDRIVE_PRIVATE_KEY?.replace(/\\n/g, '\n');
const API_BASE = process.env.API_BASE;
const API_TOKEN = process.env.API_TOKEN;

if (!CLIENT_EMAIL || !PRIVATE_KEY) {
  console.error('Missing Google Drive service account envs (GDRIVE_CLIENT_EMAIL / GDRIVE_PRIVATE_KEY).');
  process.exit(1);
}
if (!API_BASE || !API_TOKEN) {
  console.error('Missing API_BASE or API_TOKEN.');
  process.exit(1);
}

const driveScope = 'https://www.googleapis.com/auth/drive';
const tokenUrl = 'https://oauth2.googleapis.com/token';
const driveBase = 'https://www.googleapis.com/drive/v3';

const base64url = (input) => Buffer.from(input).toString('base64url');

async function getAccessToken() {
  const now = Math.floor(Date.now() / 1000);
  const header = { alg: 'RS256', typ: 'JWT' };
  const payload = {
    iss: CLIENT_EMAIL,
    scope: driveScope,
    aud: tokenUrl,
    exp: now + 3600,
    iat: now,
  };
  const headerSegment = base64url(JSON.stringify(header));
  const payloadSegment = base64url(JSON.stringify(payload));
  const unsigned = `${headerSegment}.${payloadSegment}`;
  const signer = crypto.createSign('RSA-SHA256');
  signer.update(unsigned);
  signer.end();
  const signature = signer.sign(PRIVATE_KEY, 'base64url');
  const assertion = `${unsigned}.${signature}`;

  const res = await fetch(tokenUrl, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      grant_type: 'urn:ietf:params:oauth:grant-type:jwt-bearer',
      assertion,
    }),
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(`Failed to fetch Drive access token: ${text}`);
  }
  const json = await res.json();
  return json.access_token;
}

async function fetchJson(url, options = {}) {
  const res = await fetch(url, options);
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`Request failed ${res.status}: ${text}`);
  }
  return res.json();
}

async function listArtists() {
  const url = `${API_BASE}/admin/artists/profiles`;
  return fetchJson(url, {
    headers: { Authorization: `Bearer ${API_TOKEN}` },
  });
}

async function findOrCreateFolder(driveToken, parentId, folderName) {
  const safeName = folderName.replace(/'/g, "\\'");
  const q = `name='${safeName}' and mimeType='application/vnd.google-apps.folder' and '${parentId}' in parents and trashed=false`;
  const searchUrl = `${driveBase}/files?q=${encodeURIComponent(q)}&fields=files(id,name)`;
  const search = await fetchJson(searchUrl, { headers: { Authorization: `Bearer ${driveToken}` } });
  if (search.files && search.files.length > 0) return search.files[0].id;

  const createRes = await fetch(`${driveBase}/files?fields=id`, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${driveToken}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      name: folderName,
      mimeType: 'application/vnd.google-apps.folder',
      parents: [parentId],
    }),
  });
  if (!createRes.ok) {
    const text = await createRes.text();
    throw new Error(`Failed to create folder for ${folderName}: ${text}`);
  }
  const created = await createRes.json();
  return created.id;
}

async function findLatestVideo(driveToken, folderId) {
  const q = `'${folderId}' in parents and trashed=false and mimeType contains 'video/'`;
  const url = `${driveBase}/files?q=${encodeURIComponent(q)}&orderBy=modifiedTime desc&fields=files(id,name,mimeType,modifiedTime)&pageSize=1`;
  const data = await fetchJson(url, { headers: { Authorization: `Bearer ${driveToken}` } });
  return data.files?.[0] ?? null;
}

async function makePublicIfNeeded(driveToken, fileId) {
  // Best-effort: ignore errors (e.g., if already public)
  await fetch(`${driveBase}/files/${fileId}/permissions`, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${driveToken}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      role: 'reader',
      type: 'anyone',
      allowFileDiscovery: false,
    }),
  }).catch(() => {});
}

async function upsertArtistVideo(artist, previewUrl) {
  const payload = {
    apuArtistId: artist.apArtistId,
    apuSlug: artist.apSlug ?? null,
    apuBio: artist.apBio ?? null,
    apuCity: artist.apCity ?? null,
    apuHeroImageUrl: artist.apHeroImageUrl ?? null,
    apuSpotifyArtistId: artist.apSpotifyArtistId ?? null,
    apuSpotifyUrl: artist.apSpotifyUrl ?? null,
    apuYoutubeChannelId: artist.apYoutubeChannelId ?? null,
    apuYoutubeUrl: artist.apYoutubeUrl ?? null,
    apuWebsiteUrl: artist.apWebsiteUrl ?? null,
    apuFeaturedVideoUrl: previewUrl,
    apuGenres: artist.apGenres ?? null,
    apuHighlights: artist.apHighlights ?? null,
  };
  const url = `${API_BASE}/admin/artists/profiles`;
  await fetchJson(url, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${API_TOKEN}`,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(payload),
  });
}

async function run() {
  const driveToken = await getAccessToken();
  const artists = await listArtists();
  console.log(`Syncing ${artists.length} artists to Drive parent ${DRIVE_PARENT_ID}...`);

  for (const artist of artists) {
    const folderId = await findOrCreateFolder(driveToken, DRIVE_PARENT_ID, artist.apDisplayName);
    const video = await findLatestVideo(driveToken, folderId);
    if (!video) {
      console.log(`No video found for ${artist.apDisplayName} (folder ${folderId}).`);
      continue;
    }
    await makePublicIfNeeded(driveToken, video.id);
    const previewUrl = `https://drive.google.com/file/d/${video.id}/preview`;
    if (artist.apFeaturedVideoUrl === previewUrl) {
      console.log(`No change for ${artist.apDisplayName}, preview already set.`);
      continue;
    }
    await upsertArtistVideo(artist, previewUrl);
    console.log(`Updated ${artist.apDisplayName} with preview ${previewUrl}`);
  }
}

run().catch((err) => {
  console.error(err);
  process.exit(1);
});
