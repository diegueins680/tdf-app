import fs from 'node:fs/promises';
import path from 'node:path';

const PREFERRED_LINUX_TARBALL_PATTERNS = [
  /linux.*amd64.*\.tar\.gz$/i,
  /linux.*x86_64.*\.tar\.gz$/i,
  /Linux_x86_64\.tar\.gz$/i,
  /linux-amd64\.tar\.gz$/i,
  /linux-x86_64\.tar\.gz$/i,
];
const FALLBACK_LINUX_TARBALL_PATTERN = /linux.*\.tar\.gz$/i;

function normalizeCandidate(candidate) {
  return String(candidate ?? '').trim();
}

function compareByName(left, right) {
  return left.name.localeCompare(right.name);
}

export function isExecutableMode(mode) {
  return (mode & 0o111) !== 0;
}

export function selectFlyctlDownloadUrl(candidates) {
  const urls = [...new Set(candidates.map(normalizeCandidate).filter((url) => url !== '' && url !== 'null'))];

  for (const pattern of PREFERRED_LINUX_TARBALL_PATTERNS) {
    const match = urls.find((url) => pattern.test(url));
    if (match) {
      return match;
    }
  }

  return urls.find((url) => FALLBACK_LINUX_TARBALL_PATTERN.test(url)) ?? null;
}

export async function findExtractedFlyctlBinary(rootDir) {
  const pendingDirs = [rootDir];

  while (pendingDirs.length > 0) {
    const currentDir = pendingDirs.shift();
    const entries = await fs.readdir(currentDir, { withFileTypes: true });
    entries.sort(compareByName);

    for (const entry of entries) {
      const entryPath = path.join(currentDir, entry.name);

      if (entry.isDirectory()) {
        pendingDirs.push(entryPath);
        continue;
      }

      if (!entry.isFile() || entry.name !== 'flyctl') {
        continue;
      }

      const stats = await fs.stat(entryPath);
      if (isExecutableMode(stats.mode)) {
        return entryPath;
      }
    }
  }

  return null;
}
