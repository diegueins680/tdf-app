import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { readFileSync, lstatSync } from 'node:fs';

export const sha256 = bytes => createHash('sha256').update(bytes).digest('hex');

export function sourceManifest(root) {
  // Include untracked implementation work locally; CI uses a clean checkout.
  const paths = execFileSync('git', ['ls-files', '-z', '--cached', '--others', '--exclude-standard'],
    { cwd: root, encoding: 'utf8', maxBuffer: 16 * 1024 * 1024 }).split('\0').filter(Boolean);
  const selected = paths.filter(p => /^(formal\/|scripts\/|tdf-hq\/|tdf-hq-ui\/|\.github\/)/.test(p)
    || /^docs\/.*\.(md|yaml|json)$/.test(p)
    || /^(package(-lock)?\.json|specs\.yaml|fly[^/]*\.toml|\.gitmodules|FORMAL_VERIFICATION\.md)$/.test(p));
  const files = {};
  for (const p of [...new Set(selected)].sort()) {
    // Receipts are outputs, not proof inputs; never recursively hash the run's receipt.
    if (p.startsWith('formal/system/evidence/')) continue;
    if (p.split('/').some(part => part === 'node_modules' || part === '__pycache__')) continue;
    try {
      const full = `${root}/${p}`;
      const stat = lstatSync(full);
      if (stat.isSymbolicLink()) throw new Error(`Unexpected source symlink: ${p}`);
      if (stat.isFile()) files[p] = sha256(readFileSync(full));
    } catch (error) {
      if (error.code === 'ENOENT') files[p] = 'DELETED';
      else throw error;
    }
  }
  const mobile = execFileSync('git', ['ls-files', '--stage', 'tdf-mobile'], { cwd: root, encoding: 'utf8' }).trim();
  return { files, mobileGitlink: mobile, digest: sha256(JSON.stringify({ files, mobile })) };
}

export function classifyExecution({ status, signal, error, log, before, after }) {
  if (error || signal || status !== 0) return 'execution-failed';
  if (before !== after) return 'stale-during-execution';
  if (!log.includes('Event operations formal verification passed within the documented finite bounds.')) return 'incomplete';
  return 'bounded-model-checks-passed';
}
