import fs from 'node:fs/promises';
import path from 'node:path';

function scoreFlyctlDownloadUrl(value) {
  const url = String(value ?? '').trim();
  if (!url || url === 'null') return -Infinity;
  if (!/^https?:\/\//i.test(url)) return -Infinity;
  if (!/flyctl/i.test(url)) return -Infinity;
  if (/checksum|sha256|\.txt$/i.test(url)) return -Infinity;

  let score = 0;
  if (/\.tar\.gz$/i.test(url)) score += 20;
  if (/linux/i.test(url)) score += 20;
  if (/(x86_64|amd64)/i.test(url)) score += 20;
  if (/arm64|aarch64/i.test(url)) score += 5;
  return score;
}

export function selectFlyctlDownloadUrl(urls) {
  const candidates = [...urls]
    .map((url) => ({ url: String(url ?? '').trim(), score: scoreFlyctlDownloadUrl(url) }))
    .filter((candidate) => candidate.score > -Infinity)
    .sort((left, right) => right.score - left.score || left.url.localeCompare(right.url));

  return candidates[0]?.url ?? '';
}

async function isExecutableFile(filePath) {
  try {
    const stats = await fs.stat(filePath);
    return stats.isFile() && (stats.mode & 0o111) !== 0;
  } catch (error) {
    if (error?.code === 'ENOENT') return false;
    throw error;
  }
}

export async function findExtractedFlyctlBinary(rootDir) {
  const entries = await fs.readdir(rootDir, { withFileTypes: true });

  for (const entry of entries) {
    const entryPath = path.join(rootDir, entry.name);
    if (entry.isDirectory()) {
      const nested = await findExtractedFlyctlBinary(entryPath);
      if (nested) return nested;
      continue;
    }

    if ((entry.name === 'flyctl' || entry.name === 'flyctl.exe') && (await isExecutableFile(entryPath))) {
      return entryPath;
    }
  }

  return '';
}
