#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { createReadStream, existsSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, '..', '..');
const outputDir = resolve(here, 'output');
const manifestPath = resolve(here, 'evidence-manifest.json');
const permissionNotes = {
  instagram_basic: resolve(here, 'permission-notes', 'instagram_basic.txt'),
  instagram_manage_messages: resolve(here, 'permission-notes', 'instagram_manage_messages.txt'),
  instagram_business_basic: resolve(here, 'permission-notes', 'instagram_business_basic.txt'),
  instagram_business_manage_messages: resolve(here, 'permission-notes', 'instagram_business_manage_messages.txt'),
};

const videos = [
  {
    flow: 'facebook-login',
    file: resolve(outputDir, 'final-facebook-login-2026-03-28.mp4'),
    permissions: ['instagram_basic', 'instagram_manage_messages'],
    dependencyScopes: ['pages_show_list', 'pages_read_engagement'],
    provider: 'facebook',
  },
  {
    flow: 'instagram-login',
    file: resolve(outputDir, 'final-instagram-business-login-2026-03-10.mp4'),
    permissions: ['instagram_business_basic', 'instagram_business_manage_messages'],
    dependencyScopes: [],
    provider: 'instagram',
  },
];

function sha256(filePath) {
  return new Promise((resolvePromise, rejectPromise) => {
    const hash = createHash('sha256');
    const stream = createReadStream(filePath);
    stream.on('data', (chunk) => hash.update(chunk));
    stream.on('error', rejectPromise);
    stream.on('end', () => resolvePromise(hash.digest('hex')));
  });
}

function probe(filePath) {
  const result = spawnSync(
    'ffprobe',
    [
      '-v', 'error',
      '-print_format', 'json',
      '-show_entries', 'format=duration,size:stream=codec_name,width,height',
      filePath,
    ],
    { encoding: 'utf8' },
  );

  if (result.status !== 0) {
    throw new Error(`ffprobe failed for ${filePath}: ${result.stderr || result.stdout}`);
  }

  return JSON.parse(result.stdout || '{}');
}

async function main() {
  const coveredPermissions = new Set();
  const manifestVideos = [];

  for (const video of videos) {
    if (!existsSync(video.file)) {
      throw new Error(`Missing evidence file: ${video.file}`);
    }

    const stats = statSync(video.file);
    const sha = await sha256(video.file);
    const probeData = probe(video.file);
    const videoStream = Array.isArray(probeData.streams)
      ? probeData.streams.find((stream) => Number(stream.width) > 0 && Number(stream.height) > 0)
      : null;

    for (const permission of video.permissions) {
      coveredPermissions.add(permission);
    }

    manifestVideos.push({
      flow: video.flow,
      provider: video.provider,
      file: relative(repoRoot, video.file),
      sha256: sha,
      sizeBytes: stats.size,
      durationSeconds: Number(probeData?.format?.duration ?? 0),
      width: Number(videoStream?.width ?? 0),
      height: Number(videoStream?.height ?? 0),
      codec: videoStream?.codec_name ?? null,
      permissions: video.permissions,
      dependencyScopes: video.dependencyScopes,
    });
  }

  const manifest = {
    generatedAt: new Date().toISOString(),
    reviewAppSurface: {
      setupRoute: '/social/instagram?review=1',
      inboxRoute: '/social/inbox?review=1',
      providerMapping: {
        facebook: ['instagram_basic', 'instagram_manage_messages'],
        instagram: ['instagram_business_basic', 'instagram_business_manage_messages'],
      },
      dependencyScopes: ['pages_show_list', 'pages_read_engagement'],
    },
    coverage: {
      requestedPermissions: [
        'instagram_basic',
        'instagram_manage_messages',
        'instagram_business_basic',
        'instagram_business_manage_messages',
      ],
      coveredPermissions: Array.from(coveredPermissions),
      missingPermissions: [
        'instagram_basic',
        'instagram_manage_messages',
        'instagram_business_basic',
        'instagram_business_manage_messages',
      ].filter((permission) => !coveredPermissions.has(permission)),
    },
    videos: manifestVideos,
    notes: {
      combinedReviewerNotesFile: relative(repoRoot, resolve(here, 'submission-notes.txt')),
      permissionNotes: Object.fromEntries(
        Object.entries(permissionNotes).map(([permission, filePath]) => [permission, relative(repoRoot, filePath)]),
      ),
      screencastGuide: relative(repoRoot, resolve(repoRoot, 'docs', 'META_APP_REVIEW_SCREENCAST.md')),
    },
  };

  writeFileSync(manifestPath, JSON.stringify(manifest, null, 2) + '\n');
  console.log(`Wrote ${manifestPath}`);
}

main().catch((error) => {
  console.error(error instanceof Error ? error.stack : String(error));
  process.exit(1);
});
