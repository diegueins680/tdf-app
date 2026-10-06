import { execFileSync } from 'node:child_process';
import { readFileSync, realpathSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

// Compare to committed bytes, including staged edits. A root pathspec cannot
// inspect files inside a gitlink, and missing mobile must never count as green.
export function checkGeneratedApi(root) {
  const git = (cwd, ...args) => execFileSync('git', args, { cwd, stdio: ['ignore', 'pipe', 'pipe'] });
  const mobile = path.join(root, 'tdf-mobile');
  const link = git(root, 'ls-tree', 'HEAD', '--', 'tdf-mobile').toString().trim();
  const match = /^160000 commit ([0-9a-f]{40})\ttdf-mobile$/.exec(link);
  if (!match) throw new Error('Missing committed Mobile gitlink');
  const stagedLink = git(root, 'ls-files', '--stage', '--', 'tdf-mobile').toString().trim();
  if (stagedLink !== `160000 ${match[1]} 0\ttdf-mobile`) {
    throw new Error('Staged Mobile gitlink differs from committed pin');
  }
  // rev-parse alone climbs to the parent repo when a submodule is uninitialized.
  if (realpathSync(git(mobile, 'rev-parse', '--show-toplevel').toString().trim()) !== realpathSync(mobile)) {
    throw new Error('Mobile repository is not initialized');
  }
  const actual = git(mobile, 'rev-parse', 'HEAD').toString().trim();
  if (actual !== match[1]) throw new Error(`Mobile checkout ${actual} differs from pinned ${match[1]}`);
  const clients = [
    [root, 'tdf-hq-ui/src/api/generated/types.ts'],
    [mobile, 'src/api/generated/types.ts'],
  ];
  for (const [repo, file] of clients) {
    // Compare exact Git blob identities without buffering the generated file in
    // child-process stdout. The consolidated contract exceeds Node's 1 MiB limit.
    const expected = git(repo, 'rev-parse', `HEAD:${file}`).toString().trim();
    const staged = git(repo, 'rev-parse', `:${file}`).toString().trim();
    const actualBlob = execFileSync('git', ['hash-object', '--no-filters', '--stdin'], {
      cwd: repo, input: readFileSync(path.join(repo, file)), encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe'],
    }).trim();
    if (expected !== staged || expected !== actualBlob) {
      throw new Error(`Generated API drift: ${path.relative(root, path.join(repo, file))}`);
    }
  }
  return { mobileRevision: actual, clients: clients.length };
}

if (process.argv[1] && import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href) {
  const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
  console.log(JSON.stringify(checkGeneratedApi(root)));
}
