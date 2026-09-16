import { appendFile } from 'node:fs/promises';
import { execFileSync } from 'node:child_process';
import { pathToFileURL } from 'node:url';
import { LifecycleError } from './lib/instagram-token-lifecycle.mjs';

const ARTIFACT = 'instagram-lifecycle-v1';
const WORKFLOW = '.github/workflows/refresh-instagram-token.yml';
const id = value => Number.isSafeInteger(value) && value > 0;

export async function findLifecycleRun({ repository, sha, currentRunId }, api) {
  if (!/^[\w.-]+\/[\w.-]+$/.test(repository || '') || !/^[a-f0-9]{40}$/.test(sha || '')) throw new LifecycleError('Invalid lifecycle repository or commit');
  // Inspect producing runs first, not just surviving artifacts: a failed write
  // or deleted newest checkpoint must not silently resurrect older credentials.
  for (let page = 1; page <= 20; page++) {
    const result = await api(`repos/${repository}/actions/workflows/refresh-instagram-token.yml/runs?event=workflow_dispatch&per_page=100&page=${page}`);
    if (!Array.isArray(result.workflow_runs) || !Number.isSafeInteger(result.total_count)) throw new LifecycleError('Invalid lifecycle run inventory');
    for (const run of result.workflow_runs) {
      if (String(run.id) === String(currentRunId) || !['Instagram lifecycle: setup', 'Instagram lifecycle: refresh'].includes(run.display_title)) continue;
      if (!id(run.id) || run.path !== WORKFLOW || run.head_repository?.full_name !== repository || !/^[a-f0-9]{40}$/.test(run.head_sha || '')) throw new LifecycleError('Invalid lifecycle producer identity');
      const comparison = await api(`repos/${repository}/compare/${run.head_sha}...${sha}`);
      if (!['ahead', 'identical'].includes(comparison.status)) continue;
      if (run.status !== 'completed' || run.conclusion !== 'success') throw new LifecycleError('Newest eligible lifecycle producing run did not succeed');
      const inventory = await api(`repos/${repository}/actions/runs/${run.id}/artifacts?per_page=100`);
      if (!Array.isArray(inventory.artifacts) || inventory.total_count > 100) throw new LifecycleError('Invalid lifecycle artifact inventory');
      const matches = inventory.artifacts.filter(a => a.name === ARTIFACT);
      if (matches.length !== 1 || !id(matches[0].id) || matches[0].expired) throw new LifecycleError('Newest lifecycle checkpoint is missing, ambiguous or expired');
      return String(run.id);
    }
    if (page * 100 >= result.total_count) break;
    if (result.workflow_runs.length === 0 || page === 20) throw new LifecycleError('Lifecycle history exceeds the safe lookup bound or is incomplete');
  }
  throw new LifecycleError('No eligible encrypted lifecycle state; run an approved OAuth setup on this commit or an ancestor');
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    const runId = await findLifecycleRun({ repository: process.env.GITHUB_REPOSITORY, sha: process.env.GITHUB_SHA, currentRunId: process.env.GITHUB_RUN_ID }, async endpoint => {
      try { return JSON.parse(execFileSync('gh', ['api', endpoint], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'], timeout: 30000 })); }
      catch { throw new LifecycleError('GitHub lifecycle metadata request failed'); }
    });
    if (!process.env.GITHUB_OUTPUT) throw new LifecycleError('GITHUB_OUTPUT is required');
    await appendFile(process.env.GITHUB_OUTPUT, `run-id=${runId}\n`);
  } catch (error) {
    console.error(error instanceof LifecycleError ? error.message : 'Lifecycle artifact lookup failed');
    process.exitCode = 1;
  }
}
