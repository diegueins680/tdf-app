#!/usr/bin/env node
import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';
import { promisify } from 'node:util';

import { findExtractedFlyctlBinary, selectFlyctlDownloadUrl } from './lib/flyctl-cli.mjs';

const execFileAsync = promisify(execFile);
const DEFAULT_RELEASE_API_URL = 'https://api.github.com/repos/superfly/flyctl/releases/latest';

async function fetchJson(url, githubToken) {
  const headers = githubToken ? { Authorization: `Bearer ${githubToken}` } : {};
  const response = await fetch(url, { headers });
  if (!response.ok) {
    throw new Error(`Failed to fetch flyctl release metadata: HTTP ${response.status}`);
  }
  return response.json();
}

async function fetchBytes(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to download flyctl archive: HTTP ${response.status}`);
  }
  return Buffer.from(await response.arrayBuffer());
}

export async function installFlyctl(options = {}) {
  const {
    githubToken = process.env.GITHUB_TOKEN ?? process.env.GH_TOKEN ?? '',
    installDir = path.join(process.cwd(), 'tmp', 'bin'),
    log = console.log,
    releaseApiUrl = DEFAULT_RELEASE_API_URL,
  } = options;

  const release = await fetchJson(releaseApiUrl, githubToken);
  const downloadUrl = selectFlyctlDownloadUrl(
    (release.assets ?? []).map((asset) => asset.browser_download_url),
  );
  if (!downloadUrl) {
    throw new Error('Could not find a suitable flyctl release archive.');
  }

  const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'flyctl-install-'));
  try {
    const archivePath = path.join(tempDir, path.basename(new URL(downloadUrl).pathname));
    const extractDir = path.join(tempDir, 'extract');
    await fs.mkdir(extractDir, { recursive: true });
    await fs.writeFile(archivePath, await fetchBytes(downloadUrl));
    await execFileAsync('tar', ['-xzf', archivePath, '-C', extractDir]);

    const extractedBinary = await findExtractedFlyctlBinary(extractDir);
    if (!extractedBinary) {
      throw new Error('Downloaded flyctl archive did not contain an executable flyctl binary.');
    }

    await fs.mkdir(installDir, { recursive: true });
    const binPath = path.join(installDir, path.basename(extractedBinary));
    await fs.copyFile(extractedBinary, binPath);
    await fs.chmod(binPath, 0o755);
    log(`Installed flyctl to ${binPath}`);
    return { downloadUrl, binPath };
  } finally {
    await fs.rm(tempDir, { recursive: true, force: true });
  }
}

async function main() {
  const result = await installFlyctl();
  console.log(result.binPath);
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error(error.message);
    process.exit(1);
  });
}
