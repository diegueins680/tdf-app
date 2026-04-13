import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

import { findExtractedFlyctlBinary, selectFlyctlDownloadUrl } from './lib/flyctl-cli.mjs';

const execFileAsync = promisify(execFile);
const DEFAULT_RELEASE_API = 'https://api.github.com/repos/superfly/flyctl/releases/latest';
const REQUEST_HEADERS = {
  Accept: 'application/vnd.github+json',
  'User-Agent': 'tdf-app-flyctl-installer',
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

function createHttpError(message, response) {
  const error = new Error(`${message} (${response.status} ${response.statusText})`);
  error.status = response.status;
  return error;
}

function isRetriableError(error) {
  if (!error || typeof error !== 'object') {
    return true;
  }

  if (typeof error.status !== 'number') {
    return true;
  }

  return error.status === 429 || error.status >= 500;
}

async function withRetries(operationName, operation, { attempts = 5, initialDelayMs = 2_000, log } = {}) {
  let waitMs = initialDelayMs;
  let lastError;

  for (let attempt = 1; attempt <= attempts; attempt += 1) {
    try {
      return await operation();
    } catch (error) {
      lastError = error;
      if (attempt >= attempts || !isRetriableError(error)) {
        throw error;
      }

      const message = error instanceof Error ? error.message : String(error);
      log?.(`${operationName} attempt ${attempt} failed: ${message}. Retrying in ${waitMs}ms.`);
      await delay(waitMs);
      waitMs *= 2;
    }
  }

  throw lastError;
}

async function fetchJson(url, githubToken, log, retryOptions) {
  return withRetries(
    'Fetch Fly release metadata',
    async () => {
      const response = await fetch(url, { headers: buildRequestHeaders(githubToken) });
      if (!response.ok) {
        throw createHttpError('Failed to fetch Fly release metadata', response);
      }
      return response.json();
    },
    { log, ...retryOptions },
  );
}

async function downloadFile(url, filePath, log, retryOptions) {
  await withRetries(
    'Download flyctl archive',
    async () => {
      const response = await fetch(url, { headers: REQUEST_HEADERS });
      if (!response.ok) {
        throw createHttpError('Failed to download flyctl', response);
      }

      const archiveBytes = Buffer.from(await response.arrayBuffer());
      await fs.writeFile(filePath, archiveBytes);
    },
    { log, ...retryOptions },
  );
}

export async function installFlyctl({
  releaseApiUrl = DEFAULT_RELEASE_API,
  installDir = path.join(os.homedir(), '.fly', 'bin'),
  workDir,
  skipVersionCheck = false,
  githubToken,
  retryAttempts = 5,
  retryInitialDelayMs = 2_000,
  log = console,
} = {}) {
  const logger = createLogger(log);
  const retryOptions = {
    attempts: retryAttempts,
    initialDelayMs: retryInitialDelayMs,
  };
  const workspace = workDir ?? (await fs.mkdtemp(path.join(os.tmpdir(), 'flyctl-')));
  const shouldCleanupWorkspace = workDir == null;
  const archivePath = path.join(workspace, 'flyctl.tar.gz');
  const extractDir = path.join(workspace, 'extract');
  const installPath = path.join(installDir, 'flyctl');

  await fs.mkdir(workspace, { recursive: true });
  await fs.mkdir(extractDir, { recursive: true });
  await fs.mkdir(installDir, { recursive: true });

  try {
    const release = await fetchJson(releaseApiUrl, githubToken, logger, retryOptions);
    const assets = Array.isArray(release?.assets) ? release.assets : [];
    const candidateUrls = assets.map((asset) => asset?.browser_download_url);
    const downloadUrl = selectFlyctlDownloadUrl(candidateUrls);

    if (!downloadUrl) {
      throw new Error('Failed to resolve flyctl download URL');
    }

    logger(`Downloading flyctl from ${downloadUrl}`);
    await downloadFile(downloadUrl, archivePath, logger, retryOptions);
    await execFileAsync('tar', ['-xzf', archivePath, '-C', extractDir]);

    const binPath = await findExtractedFlyctlBinary(extractDir);
    if (!binPath) {
      throw new Error('flyctl binary not found after extraction');
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
    await installFlyctl({
      releaseApiUrl: process.env.FLYCTL_RELEASE_API || DEFAULT_RELEASE_API,
      installDir: process.env.FLYCTL_INSTALL_DIR || path.join(os.homedir(), '.fly', 'bin'),
      workDir: process.env.FLYCTL_WORK_DIR || undefined,
      skipVersionCheck: process.env.FLYCTL_SKIP_VERSION_CHECK === '1',
      githubToken: process.env.FLYCTL_RELEASE_GITHUB_TOKEN || process.env.GITHUB_TOKEN,
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
