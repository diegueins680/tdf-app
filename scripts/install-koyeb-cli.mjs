import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

import { findExtractedKoyebBinary, selectKoyebDownloadUrl } from './lib/koyeb-cli.mjs';

const execFileAsync = promisify(execFile);
const DEFAULT_RELEASE_API = 'https://api.github.com/repos/koyeb/koyeb-cli/releases/latest';
const REQUEST_HEADERS = {
  Accept: 'application/vnd.github+json',
  'User-Agent': 'tdf-app-koyeb-installer',
};

function createLogger(log) {
  if (typeof log === 'function') {
    return log;
  }

  if (log && typeof log.log === 'function') {
    return log.log.bind(log);
  }

  return () => {};
}

function buildRequestHeaders(githubToken) {
  if (!githubToken) {
    return REQUEST_HEADERS;
  }

  return {
    ...REQUEST_HEADERS,
    Authorization: `Bearer ${githubToken}`,
  };
}

async function fetchJson(url, githubToken) {
  const response = await fetch(url, { headers: buildRequestHeaders(githubToken) });
  if (!response.ok) {
    throw new Error(`Failed to fetch Koyeb release metadata (${response.status} ${response.statusText})`);
  }
  return response.json();
}

async function downloadFile(url, filePath) {
  const response = await fetch(url, { headers: REQUEST_HEADERS });
  if (!response.ok) {
    throw new Error(`Failed to download Koyeb CLI (${response.status} ${response.statusText})`);
  }

  const archiveBytes = Buffer.from(await response.arrayBuffer());
  await fs.writeFile(filePath, archiveBytes);
}

export async function installKoyebCli({
  releaseApiUrl = DEFAULT_RELEASE_API,
  installDir = '/usr/local/bin',
  workDir,
  skipVersionCheck = false,
  githubToken,
  log = console,
} = {}) {
  const logger = createLogger(log);
  const workspace = workDir ?? (await fs.mkdtemp(path.join(os.tmpdir(), 'koyeb-cli-')));
  const shouldCleanupWorkspace = workDir == null;
  const archivePath = path.join(workspace, 'koyeb.tar.gz');
  const extractDir = path.join(workspace, 'extract');
  const installPath = path.join(installDir, 'koyeb');

  await fs.mkdir(workspace, { recursive: true });
  await fs.mkdir(extractDir, { recursive: true });
  await fs.mkdir(installDir, { recursive: true });

  try {
    const release = await fetchJson(releaseApiUrl, githubToken);
    const assets = Array.isArray(release?.assets) ? release.assets : [];
    const candidateUrls = assets.map((asset) => asset?.browser_download_url);
    const downloadUrl = selectKoyebDownloadUrl(candidateUrls);

    if (!downloadUrl) {
      throw new Error('Failed to resolve Koyeb CLI download URL');
    }

    logger(`Downloading Koyeb CLI from ${downloadUrl}`);
    await downloadFile(downloadUrl, archivePath);
    await execFileAsync('tar', ['-xzf', archivePath, '-C', extractDir]);

    const binPath = await findExtractedKoyebBinary(extractDir);
    if (!binPath) {
      throw new Error('Koyeb binary not found after extraction');
    }

    await execFileAsync('install', ['-m', '755', binPath, installPath]);

    let versionOutput = '';
    if (!skipVersionCheck) {
      const { stdout } = await execFileAsync(installPath, ['version']);
      versionOutput = stdout.trim();
      if (versionOutput !== '') {
        logger(versionOutput);
      }
    }

    return { binPath, downloadUrl, installPath, versionOutput };
  } finally {
    if (shouldCleanupWorkspace) {
      await fs.rm(workspace, { recursive: true, force: true });
    }
  }
}

async function main() {
  try {
    await installKoyebCli({
      releaseApiUrl: process.env.KOYEB_RELEASE_API || DEFAULT_RELEASE_API,
      installDir: process.env.KOYEB_INSTALL_DIR || '/usr/local/bin',
      workDir: process.env.KOYEB_WORK_DIR || undefined,
      skipVersionCheck: process.env.KOYEB_SKIP_VERSION_CHECK === '1',
      githubToken: process.env.KOYEB_RELEASE_GITHUB_TOKEN || process.env.GITHUB_TOKEN,
    });
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    console.error(message);
    process.exitCode = 1;
  }
}

if (process.argv[1] && fileURLToPath(import.meta.url) === path.resolve(process.argv[1])) {
  await main();
}
