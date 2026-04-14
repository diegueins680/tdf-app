#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { createReadStream, existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { dirname, isAbsolute, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(here, '..');
const packetConfigPath = resolve(repoRoot, 'screencast/meta-app-review/submission-packet.current.json');
const outputPath = resolve(repoRoot, 'docs/meta-app-review-current-packet.md');

const packet = JSON.parse(readFileSync(packetConfigPath, 'utf8'));

function resolveFromRepo(filePath) {
  return isAbsolute(filePath) ? filePath : resolve(repoRoot, filePath);
}

function ensureFile(label, filePath) {
  if (!existsSync(filePath)) {
    throw new Error(`Missing ${label}: ${filePath}`);
  }
  const stats = statSync(filePath);
  if (!stats.isFile()) {
    throw new Error(`Expected ${label} to be a file: ${filePath}`);
  }
  if (stats.size <= 0) {
    throw new Error(`Expected ${label} to be non-empty: ${filePath}`);
  }
  return stats;
}

function sha256(filePath) {
  return new Promise((resolvePromise, rejectPromise) => {
    const hash = createHash('sha256');
    const stream = createReadStream(filePath);
    stream.on('data', (chunk) => hash.update(chunk));
    stream.on('error', rejectPromise);
    stream.on('end', () => resolvePromise(hash.digest('hex')));
  });
}

function formatPath(filePath) {
  return isAbsolute(filePath) ? filePath : relative(repoRoot, resolveFromRepo(filePath));
}

async function buildArtifactSection(artifacts) {
  const lines = [];
  for (const [artifactKey, artifact] of Object.entries(artifacts)) {
    const absolutePath = resolveFromRepo(artifact.path);
    const stats = ensureFile(`artifact ${artifactKey}`, absolutePath);
    const digest = await sha256(absolutePath);
    lines.push(`### ${artifactKey}`);
    lines.push(`- Path: \`${formatPath(artifact.path)}\``);
    lines.push(`- Size: ${stats.size} bytes`);
    lines.push(`- SHA256: \`${digest}\``);
    if (artifact.purpose) {
      lines.push(`- Purpose: ${artifact.purpose}`);
    }
    if (Array.isArray(artifact.proofAnchors) && artifact.proofAnchors.length > 0) {
      lines.push('- Proof anchors:');
      for (const anchor of artifact.proofAnchors) {
        lines.push(`  - ${anchor}`);
      }
    }
    lines.push('');
  }
  return lines;
}

function buildPermissionSection(permissions, artifacts) {
  const lines = [];
  for (const permission of packet.requestedPermissions) {
    const entry = permissions[permission];
    if (!entry) {
      throw new Error(`Missing permission entry for ${permission}`);
    }
    const notePath = resolveFromRepo(entry.noteFile);
    ensureFile(`note file for ${permission}`, notePath);
    lines.push(`### ${permission}`);
    lines.push(`- Intent: ${entry.intent}`);
    lines.push(`- Note file: \`${formatPath(entry.noteFile)}\``);
    lines.push(`- Evidence artifacts: ${entry.evidenceArtifacts.map((key) => `\`${key}\``).join(', ')}`);
    if (Array.isArray(entry.proofAnchors) && entry.proofAnchors.length > 0) {
      lines.push('- Proof anchors:');
      for (const anchor of entry.proofAnchors) {
        lines.push(`  - ${anchor}`);
      }
    }
    for (const artifactKey of entry.evidenceArtifacts) {
      if (!artifacts[artifactKey]) {
        throw new Error(`Permission ${permission} references missing artifact key ${artifactKey}`);
      }
    }
    lines.push('');
  }
  return lines;
}

async function main() {
  ensureFile('submission packet config', packetConfigPath);
  const combinedNotesPath = resolveFromRepo(packet.notes.combinedReviewerNotesFile);
  ensureFile('combined reviewer notes', combinedNotesPath);
  ensureFile('screencast guide', resolveFromRepo(packet.notes.screencastGuide));
  for (const historyPath of packet.notes.historicalContext ?? []) {
    ensureFile('historical context file', resolveFromRepo(historyPath));
  }

  const lines = [];
  lines.push('# Meta App Review Current Packet');
  lines.push('');
  lines.push(`Updated: ${packet.updatedAt}`);
  lines.push('');
  lines.push('## Owner Direction');
  lines.push(packet.ownerDirection);
  lines.push('');
  lines.push('## Live Submission');
  lines.push(`- App ID: \`${packet.app.appId}\``);
  lines.push(`- Business ID: \`${packet.app.businessId}\``);
  lines.push(`- Requests URL: ${packet.app.requestsUrl}`);
  lines.push(`- Active detail URL: ${packet.app.activeDetailUrl}`);
  lines.push(`- Active submission id: \`${packet.app.activeSubmissionId}\``);
  lines.push(`- Historical submission ids: ${(packet.app.historicalSubmissionIds ?? []).map((id) => `\`${id}\``).join(', ')}`);
  lines.push('');
  lines.push('## Live Meta State');
  lines.push(`- Surface heading: \`${packet.reviewState.surfaceHeading}\``);
  lines.push(`- Status: \`${packet.reviewState.status}\``);
  lines.push(`- Permissions not approved: ${packet.reviewState.permissionsNotApproved.map((permission) => `\`${permission}\``).join(', ')}`);
  lines.push('');
  lines.push('## Requested Permissions');
  for (const permission of packet.requestedPermissions) {
    lines.push(`- \`${permission}\``);
  }
  lines.push('');
  lines.push('## Packet Text');
  for (const paragraph of packet.packetText) {
    lines.push(paragraph);
    lines.push('');
  }
  lines.push('## Evidence Artifacts');
  lines.push(...(await buildArtifactSection(packet.artifacts)));
  lines.push('## Per-Permission Mapping');
  lines.push(...buildPermissionSection(packet.permissions, packet.artifacts));
  lines.push('## Operator Files');
  lines.push(`- Combined reviewer notes: \`${formatPath(packet.notes.combinedReviewerNotesFile)}\``);
  lines.push(`- Screencast guide: \`${formatPath(packet.notes.screencastGuide)}\``);
  lines.push('- Historical context:');
  for (const historyPath of packet.notes.historicalContext ?? []) {
    lines.push(`  - \`${formatPath(historyPath)}\``);
  }
  lines.push('');
  lines.push('## Submission Action');
  lines.push(`Open ${packet.app.activeDetailUrl} in an authenticated Meta App Review session, use the files above, paste the matching permission notes, and resubmit only after the active evidence bundle is attached.`);
  lines.push('');

  mkdirSync(dirname(outputPath), { recursive: true });
  writeFileSync(outputPath, `${lines.join('\n')}\n`);
  console.log(`Wrote ${relative(repoRoot, outputPath)}`);
}

main().catch((error) => {
  console.error(error instanceof Error ? error.stack : String(error));
  process.exit(1);
});
