import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { createRequire } from 'node:module';
import { execFile } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';
import {
  buildCommitContext,
  expandTemplate,
  htmlToText,
  parseIdeaMarkdown,
  parseGitHubRemote,
  summarizeWorkflowRuns,
  verifyImprovementLoopModel,
} from '../lib/continuous-improvement-loop.mjs';
import { checkpointDirtyWorktree } from '../lib/continuous-improvement-loop-dirty-worktree.mjs';
import { syncAndPollLatestRemoteCi, syncSubmodulesRecursively, waitForGreenCi } from '../continuous-improvement-loop.mjs';
import {
  buildDefaultIdea,
  chooseDiscoveryLane,
  discoverImprovementIdea,
  verifyDiscoveryPolicyModel,
} from '../lib/discovery.mjs';
import { findExtractedKoyebBinary, selectKoyebDownloadUrl } from '../lib/koyeb-cli.mjs';
import { auditUiSource } from '../lib/ui-static-audit.mjs';
import { installKoyebCli } from '../install-koyeb-cli.mjs';

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const loopScriptPath = path.resolve(testDir, '..', 'continuous-improvement-loop.mjs');

test('expandTemplate substitutes known placeholders and leaves unknown ones intact', () => {
  const rendered = expandTemplate('idea={idea_file} missing={unknown}', {
    idea_file: '/tmp/idea.md',
  });

  assert.equal(rendered, 'idea=/tmp/idea.md missing={unknown}');
});

test('parseGitHubRemote supports ssh and https remotes', () => {
  assert.deepEqual(parseGitHubRemote('git@github.com:diegueins680/tdf-app.git'), {
    owner: 'diegueins680',
    repo: 'tdf-app',
  });

  assert.deepEqual(parseGitHubRemote('https://github.com/diegueins680/tdf-app.git'), {
    owner: 'diegueins680',
    repo: 'tdf-app',
  });
});

test('parseIdeaMarkdown extracts title and metadata fields', () => {
  const markdown = `
    # Improvement Idea

    Source: builtin static UI audit
    Target: tdf-hq-ui/src/pages/LabelArtistsPage.tsx:337
    Reason: Icon-only button is missing an aria-label.
  `;

  assert.deepEqual(parseIdeaMarkdown(markdown), {
    title: 'Improvement Idea',
    source: 'builtin static UI audit',
    target: 'tdf-hq-ui/src/pages/LabelArtistsPage.tsx:337',
    reason: 'Icon-only button is missing an aria-label.',
  });
});

test('buildCommitContext prefers the idea title when staged files match the target', () => {
  const context = buildCommitContext(
    {
      idea_title: 'Address icon-button-label in tdf-hq-ui/src/pages/LabelArtistsPage.tsx',
      idea_target: 'tdf-hq-ui/src/pages/LabelArtistsPage.tsx:337',
      idea_reason: 'Icon-only button is missing an aria-label.',
    },
    ['tdf-hq-ui/src/pages/LabelArtistsPage.tsx'],
  );

  assert.equal(context.commit_type, 'fix');
  assert.equal(context.commit_summary, 'address icon button label in label artists page');
  assert.equal(context.commit_message, 'fix: address icon button label in label artists page');
});

test('buildCommitContext falls back to the staged area when the target does not match', () => {
  const context = buildCommitContext(
    {
      idea_title: 'Address icon-button-label in tdf-hq-ui/src/pages/LabelArtistsPage.tsx',
      idea_target: 'tdf-hq-ui/src/pages/LabelArtistsPage.tsx:337',
      idea_reason: 'Icon-only button is missing an aria-label.',
    },
    [
      'scripts/continuous-improvement-loop.mjs',
      'scripts/continuous-improvement-loop.example.json',
      'scripts/__tests__/continuous-improvement-loop.test.mjs',
    ],
  );

  assert.equal(context.commit_type, 'chore');
  assert.equal(context.commit_message, 'chore: update continuous improvement loop');
});

test('buildCommitContext ignores generic idea titles and uses target plus reason instead', () => {
  const context = buildCommitContext(
    {
      idea_title: 'Improvement Idea',
      idea_target: 'tdf-hq-ui/src/pages/LabelArtistsPage.tsx:337',
      idea_reason: 'IconButton is missing an explicit accessible label.',
    },
    ['tdf-hq-ui/src/pages/LabelArtistsPage.tsx'],
  );

  assert.equal(context.commit_type, 'fix');
  assert.equal(context.commit_summary, 'label icon buttons in label artists page');
  assert.equal(context.commit_message, 'fix: label icon buttons in label artists page');
});

test('summarizeWorkflowRuns separates pending, successful, and failed workflow runs', () => {
  const summary = summarizeWorkflowRuns([
    { id: 1, name: 'Build Image', status: 'completed', conclusion: 'success', html_url: 'https://example.com/build' },
    { id: 2, name: 'Deploy', status: 'in_progress', conclusion: null, html_url: 'https://example.com/deploy' },
    { id: 3, name: 'Smoke', status: 'completed', conclusion: 'failure', html_url: 'https://example.com/smoke' },
  ]);

  assert.deepEqual(summary.successful, [
    {
      id: 1,
      name: 'Build Image',
      status: 'completed',
      conclusion: 'success',
      detailsUrl: 'https://example.com/build',
    },
  ]);
  assert.deepEqual(summary.pending, [
    {
      id: 2,
      name: 'Deploy',
      status: 'in_progress',
      conclusion: 'pending',
      detailsUrl: 'https://example.com/deploy',
    },
  ]);
  assert.deepEqual(summary.failed, [
    {
      id: 3,
      name: 'Smoke',
      status: 'completed',
      conclusion: 'failure',
      detailsUrl: 'https://example.com/smoke',
    },
  ]);
});

test('selectKoyebDownloadUrl prefers amd64 Linux tarballs over weaker fallbacks', () => {
  const selected = selectKoyebDownloadUrl([
    '',
    'null',
    'https://example.com/koyeb-cli_5.10.1_linux_arm64.tar.gz',
    'https://example.com/koyeb-cli_5.10.1_linux_amd64.tar.gz',
    'https://example.com/checksums.txt',
  ]);

  assert.equal(selected, 'https://example.com/koyeb-cli_5.10.1_linux_amd64.tar.gz');
});

test('findExtractedKoyebBinary ignores similarly named archives and only returns an executable binary', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'koyeb-binary-search-test-'));
  const nestedDir = path.join(tempRoot, 'nested');
  const binaryPath = path.join(nestedDir, 'koyeb');

  try {
    await fs.mkdir(nestedDir, { recursive: true });
    await fs.writeFile(path.join(tempRoot, 'koyeb.tar.gz'), 'not a binary\n', 'utf8');
    await fs.writeFile(binaryPath, '#!/usr/bin/env bash\nprintf "koyeb 5.10.1\\n"\n', { encoding: 'utf8', mode: 0o755 });

    const selected = await findExtractedKoyebBinary(tempRoot);
    assert.equal(selected, binaryPath);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('installKoyebCli installs the extracted executable instead of the downloaded archive', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'koyeb-install-test-'));
  const payloadDir = path.join(tempRoot, 'payload');
  const installDir = path.join(tempRoot, 'bin');
  const archivePath = path.join(tempRoot, 'koyeb-cli_5.10.1_linux_amd64.tar.gz');
  const originalFetch = global.fetch;

  try {
    await fs.mkdir(payloadDir, { recursive: true });
    await fs.writeFile(
      path.join(payloadDir, 'koyeb'),
      '#!/usr/bin/env bash\nprintf "koyeb 5.10.1\\n"\n',
      { encoding: 'utf8', mode: 0o755 },
    );
    await execFileAsync('tar', ['-czf', archivePath, '-C', payloadDir, '.']);

    const archiveBytes = await fs.readFile(archivePath);
    const releaseApiUrl = 'https://example.com/releases/latest';
    const tarballUrl = 'https://example.com/koyeb-cli_5.10.1_linux_amd64.tar.gz';
    const githubToken = 'test-github-token';

    global.fetch = async (input, init = {}) => {
      const href = String(input);
      if (href === releaseApiUrl) {
        assert.equal(init.headers.Authorization, `Bearer ${githubToken}`);
        return new Response(
          JSON.stringify({
            assets: [
              {
                browser_download_url: tarballUrl,
              },
            ],
          }),
          {
            headers: { 'content-type': 'application/json' },
            status: 200,
          },
        );
      }

      if (href === tarballUrl) {
        return new Response(archiveBytes, {
          headers: { 'content-type': 'application/gzip' },
          status: 200,
        });
      }

      throw new Error(`unexpected fetch url: ${href}`);
    };

    const result = await installKoyebCli({
      githubToken,
      installDir,
      log: () => {},
      releaseApiUrl,
    });

    assert.equal(result.downloadUrl, tarballUrl);
    assert.equal(path.basename(result.binPath), 'koyeb');

    const { stdout } = await execFileAsync(path.join(installDir, 'koyeb'), ['version']);
    assert.equal(stdout.trim(), 'koyeb 5.10.1');
  } finally {
    global.fetch = originalFetch;
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('syncSubmodulesRecursively initializes nested submodules', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'loop-submodules-test-'));
  const leafRepo = path.join(tempRoot, 'leaf-repo');
  const mobileRepo = path.join(tempRoot, 'mobile-repo');
  const appRepo = path.join(tempRoot, 'app-repo');
  const cloneRepo = path.join(tempRoot, 'clone-repo');
  const previousAllowProtocol = process.env.GIT_ALLOW_PROTOCOL;

  async function git(args, cwd) {
    await execFileAsync('git', args, { cwd });
  }

  async function initCommittedRepo(repoPath, files) {
    await fs.mkdir(repoPath, { recursive: true });
    await git(['init', '--initial-branch=main'], repoPath);
    await git(['config', 'user.name', 'Loop Test'], repoPath);
    await git(['config', 'user.email', 'loop-test@example.com'], repoPath);
    for (const [filePath, content] of files) {
      const fullPath = path.join(repoPath, filePath);
      await fs.mkdir(path.dirname(fullPath), { recursive: true });
      await fs.writeFile(fullPath, content, 'utf8');
    }
    await git(['add', '.'], repoPath);
    await git(['commit', '-m', 'init'], repoPath);
  }

  process.env.GIT_ALLOW_PROTOCOL = 'file';

  try {
    await initCommittedRepo(leafRepo, [['leaf.txt', 'leaf\n']]);
    await initCommittedRepo(mobileRepo, [['mobile.txt', 'mobile\n']]);
    await git(['-c', 'protocol.file.allow=always', 'submodule', 'add', leafRepo, 'vendor/leaf'], mobileRepo);
    await git(['commit', '-am', 'add nested submodule'], mobileRepo);

    await initCommittedRepo(appRepo, [['README.md', 'app\n']]);
    await git(['-c', 'protocol.file.allow=always', 'submodule', 'add', mobileRepo, 'tdf-mobile'], appRepo);
    await git(['commit', '-am', 'add mobile submodule'], appRepo);

    await execFileAsync('git', ['clone', '--no-recurse-submodules', appRepo, cloneRepo]);
    await syncSubmodulesRecursively(cloneRepo);

    const nestedLeaf = await fs.readFile(path.join(cloneRepo, 'tdf-mobile', 'vendor', 'leaf', 'leaf.txt'), 'utf8');
    assert.equal(nestedLeaf, 'leaf\n');
  } finally {
    if (previousAllowProtocol === undefined) {
      delete process.env.GIT_ALLOW_PROTOCOL;
    } else {
      process.env.GIT_ALLOW_PROTOCOL = previousAllowProtocol;
    }
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('htmlToText strips tags and decodes common entities', () => {
  assert.equal(
    htmlToText('<table><tr><td>Status:</td><td>🚫&nbsp;Build failed &amp; stopped.</td></tr></table>'),
    'Status: 🚫 Build failed & stopped.',
  );
});

test('verifyImprovementLoopModel passes its own safety checks', () => {
  const report = verifyImprovementLoopModel();

  assert.equal(report.ok, true);
  assert.equal(report.findings.length, 0);
  assert.ok(report.reachableStates.includes('pollCi'));
  assert.equal(report.discoveryPolicy?.ok, true);
});

test('verifyDiscoveryPolicyModel passes its own fairness checks', () => {
  const report = verifyDiscoveryPolicyModel();

  assert.equal(report.ok, true);
  assert.equal(report.findings.length, 0);
  assert.ok(report.casesChecked > 0);
});

test('chooseDiscoveryLane alternates equal-priority lanes and prefers the first tie for backend', () => {
  const candidates = {
    ui: { lane: 'ui', priority: 1, idea: { source: 'ui-fallback' } },
    backend: { lane: 'backend', priority: 1, idea: { source: 'backend-fallback' } },
  };

  assert.equal(chooseDiscoveryLane(candidates, { lastLane: '' }), 'backend');
  assert.equal(chooseDiscoveryLane(candidates, { lastLane: 'backend' }), 'ui');
  assert.equal(chooseDiscoveryLane(candidates, { lastLane: 'ui' }), 'backend');
});

test('buildDefaultIdea prefers a real UI finding over a backend fallback', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'discovery-priority-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(path.join(repoDir, 'tdf-hq-ui', 'src'), { recursive: true });
  await fs.mkdir(path.join(repoDir, 'tdf-hq', 'app'), { recursive: true });

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init']);
    await fs.writeFile(
      path.join(repoDir, 'tdf-hq-ui', 'src', 'Example.tsx'),
      [
        'export function Example() {',
        '  return (',
        '    <IconButton onClick={toggleSidebar}>',
        '      <MenuIcon />',
        '    </IconButton>',
        '  );',
        '}',
        '',
      ].join('\n'),
      'utf8',
    );
    await git(['add', 'tdf-hq-ui/src/Example.tsx']);

    const idea = await buildDefaultIdea(repoDir, { lastLane: 'ui' });
    assert.equal(idea.source, 'builtin-ui-audit');
    assert.equal(idea.lane, 'ui');
    assert.match(idea.markdown, /Lane: ui/);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('discoverImprovementIdea persists backend and ui lane rotation between fallback iterations', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'discovery-rotation-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const statePath = path.join(tempRoot, 'discovery-state.json');
  await fs.mkdir(path.join(repoDir, 'tdf-hq-ui', 'src'), { recursive: true });
  await fs.mkdir(path.join(repoDir, 'tdf-hq', 'app'), { recursive: true });

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init']);
    await fs.writeFile(path.join(repoDir, 'tdf-hq-ui', 'src', 'Clean.tsx'), 'export const ok = true;\n', 'utf8');
    await fs.writeFile(path.join(repoDir, 'tdf-hq', 'app', 'Main.hs'), 'main = putStrLn "ok"\n', 'utf8');
    await git(['add', 'tdf-hq-ui/src/Clean.tsx', 'tdf-hq/app/Main.hs']);

    const firstIdea = await discoverImprovementIdea(repoDir, { statePath });
    assert.equal(firstIdea.lane, 'backend');
    assert.equal(firstIdea.source, 'builtin-backend-fallback');
    assert.match(firstIdea.markdown, /Lane: backend/);

    const secondIdea = await discoverImprovementIdea(repoDir, { statePath });
    assert.equal(secondIdea.lane, 'ui');
    assert.equal(secondIdea.source, 'builtin-ui-fallback');
    assert.match(secondIdea.markdown, /Lane: ui/);

    const state = JSON.parse(await fs.readFile(statePath, 'utf8'));
    assert.equal(state.lastLane, 'ui');
    assert.equal(state.counts.backend, 1);
    assert.equal(state.counts.ui, 1);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('auditUiSource flags unlabeled icon buttons and unlabeled images', () => {
  const source = `
    <IconButton onClick={toggleSidebar}>
      <MenuIcon />
    </IconButton>
    <TextField value={value} onChange={setValue} />
    <img src="/hero.png" />
    <IconButton aria-label="Open inbox">
      <MailIcon />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 3);
  assert.deepEqual(
    findings.map((finding) => finding.rule),
    ['icon-button-label', 'text-field-label', 'image-alt'],
  );
});

test('auditUiSource does not treat arrow functions as the end of a labeled tag', () => {
  const source = `
    <IconButton
      size="small"
      onClick={() => deleteItem(item.id)}
      aria-label={\`Eliminar nota: \${item.text}\`}
    >
      <DeleteIcon fontSize="small" />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 0);
});

test('auditUiSource ignores commented-out pseudo-tags', () => {
  const source = `
    {/* <IconButton onClick={toggleSidebar}><MenuIcon /></IconButton> */}
    <IconButton aria-label="Open inbox">
      <MailIcon />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 0);
});

test('auditUiSource ignores quoted pseudo-tags', () => {
  const source = `
    const template = '<IconButton onClick={toggleSidebar}><MenuIcon /></IconButton>';
    <IconButton aria-label="Open inbox">
      <MailIcon />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 0);
});

test('buildDefaultIdea ignores TODO-like code literals and keeps real comment TODOs', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'discovery-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(path.join(repoDir, 'src'), { recursive: true });

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init']);
    await fs.writeFile(
      path.join(repoDir, 'src', 'example.mjs'),
      [
        'const TODO_PATTERN = /\\b(TODO|FIXME|HACK|XXX)\\b[^\\n]*/g;',
        '// TODO: replace the placeholder scan with comment-aware matching',
        '',
      ].join('\n'),
      'utf8',
    );
    await git(['add', 'src/example.mjs']);

    const idea = await buildDefaultIdea(repoDir);

    assert.equal(idea.source, 'builtin-todo-scan');
    assert.match(idea.markdown, /Target: src\/example\.mjs:2/);
    assert.match(idea.markdown, /Reason: TODO: replace the placeholder scan with comment-aware matching/);
    assert.doesNotMatch(idea.markdown, /Target: src\/example\.mjs:1/);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('buildDefaultIdea surfaces UI audit errors instead of hiding them behind TODO fallback', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'discovery-ui-error-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(path.join(repoDir, 'tdf-hq-ui'), { recursive: true });
  await fs.mkdir(path.join(repoDir, 'src'), { recursive: true });

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init']);
    await fs.writeFile(path.join(repoDir, 'tdf-hq-ui', 'src'), 'not a directory\n', 'utf8');
    await fs.writeFile(
      path.join(repoDir, 'src', 'example.mjs'),
      '// TODO: this should not hide a broken UI workspace\n',
      'utf8',
    );
    await git(['add', 'tdf-hq-ui/src', 'src/example.mjs']);

    await assert.rejects(() => buildDefaultIdea(repoDir), {
      code: 'ENOTDIR',
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('continuous-improvement-loop honors --allow-dirty for tracked baseline changes without committing them', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'existing.txt'), 'baseline\n', 'utf8');
    await git(['add', 'existing.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    await fs.writeFile(path.join(repoDir, 'existing.txt'), 'baseline\nlocal dirty change\n', 'utf8');
    const configPath = path.join(repoDir, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          pollGitHub: false,
          iterationDelaySeconds: 0,
          ideaCommand: "printf '# Dirty baseline test\\nSource: test\\nTarget: new-file.txt:1\\nReason: verify allow-dirty tracked baselines stay out of loop commits.\\n'",
          implementationCommand: "printf 'created by loop\\n' > new-file.txt",
          uiAuditCommand: "printf '[]\\n'",
        },
        null,
        2,
      ),
      'utf8',
    );

    await execFileAsync(
      'node',
      [loopScriptPath, '--config', 'loop-config.json', '--allow-dirty', '--max-iterations', '1'],
      { cwd: repoDir },
    );

    const commitDiff = await git(['diff', '--name-only', 'HEAD~1', 'HEAD']);
    assert.deepEqual(commitDiff.stdout.trim().split('\n').filter(Boolean), ['new-file.txt']);

    const status = await git(['status', '--short']);
    assert.match(status.stdout, /^ M existing\.txt$/m);
    assert.doesNotMatch(status.stdout, /^M  existing\.txt$/m);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('dirty-worktree checkpoint helper commits directly to main and keeps the loop branch clean', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-dirty-checkpoint-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  const otherDir = path.join(tempRoot, 'other');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
    await git(['add', 'tracked.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    await execFileAsync('git', ['clone', '--branch', 'main', remoteDir, otherDir], { cwd: tempRoot });
    await execFileAsync('git', ['config', 'user.name', 'Remote Writer'], { cwd: otherDir });
    await execFileAsync('git', ['config', 'user.email', 'remote@example.com'], { cwd: otherDir });
    await fs.writeFile(path.join(otherDir, 'remote-only.txt'), 'remote\n', 'utf8');
    await execFileAsync('git', ['add', 'remote-only.txt'], { cwd: otherDir });
    await execFileAsync('git', ['commit', '-m', 'remote advance'], { cwd: otherDir });
    await execFileAsync('git', ['push', 'origin', 'main'], { cwd: otherDir });

    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\ndirty tracked change\n', 'utf8');
    await fs.writeFile(path.join(repoDir, 'untracked.txt'), 'dirty untracked change\n', 'utf8');

    const result = await checkpointDirtyWorktree({ repoRoot: repoDir, currentBranch: 'main' });

    assert.equal(result.recovered, true);
    assert.equal(result.branch, 'main');
    assert.equal((await git(['branch', '--show-current'])).stdout.trim(), 'main');
    assert.equal((await git(['status', '--short'])).stdout.trim(), '');
    assert.equal((await git(['rev-parse', 'HEAD'])).stdout.trim(), result.commit);
    assert.equal(await fs.readFile(path.join(repoDir, 'remote-only.txt'), 'utf8'), 'remote\n');

    const remoteMain = await execFileAsync('git', ['--git-dir', remoteDir, 'rev-parse', 'refs/heads/main'], {
      cwd: tempRoot,
    });
    assert.equal(remoteMain.stdout.trim(), result.commit);

    const checkpointLog = await git(['log', '-1', '--pretty=%s%n%b', result.commit]);
    assert.match(checkpointLog.stdout, /^chore\(loop\): checkpoint dirty worktree/m);
    assert.match(checkpointLog.stdout, /Dirty worktree analysis:/);
    assert.match(checkpointLog.stdout, /tracked\.txt/);
    assert.match(checkpointLog.stdout, /untracked\.txt/);
    assert.match(result.summary, /committed dirty worktree directly to main/);
    const logSubjects = (await git(['log', '-2', '--pretty=%s'])).stdout.trim().split('\n');
    assert.deepEqual(logSubjects, ['chore(loop): checkpoint dirty worktree', 'remote advance']);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('dirty-worktree helper rebases onto main before falling back to merge recovery', async () => {
  const script = await fs.readFile(new URL('../lib/continuous-improvement-loop-dirty-worktree.mjs', import.meta.url), 'utf8');
  assert.match(script, /function rebaseHeadOntoRemoteBranch\(repoRoot, remoteName, branchName\)/);
  assert.match(script, /runGit\(repoRoot, \['rebase', remoteRef\], \{ capture: false \}\)/);
  assert.match(script, /runGit\(repoRoot, \['rebase', '--abort'\], \{ capture: false \}\)/);
  assert.match(script, /return mergeRefOntoHead\(repoRoot, remoteRef, branchName\);/);
});

test('continuous-improvement-loop stages tracked files modified during an iteration', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-tracked-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
    await git(['add', 'tracked.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    const configPath = path.join(repoDir, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          pollGitHub: false,
          iterationDelaySeconds: 0,
          ideaCommand:
            "printf '# Tracked file test\\nSource: test\\nTarget: tracked.txt:1\\nReason: verify tracked file edits are staged correctly.\\n'",
          implementationCommand: "printf 'after\\n' >> tracked.txt",
          uiAuditCommand: "printf '[]\\n'",
        },
        null,
        2,
      ),
      'utf8',
    );

    await execFileAsync(
      'node',
      [loopScriptPath, '--config', 'loop-config.json', '--allow-dirty', '--max-iterations', '1'],
      { cwd: repoDir },
    );

    const commitDiff = await git(['diff', '--name-only', 'HEAD~1', 'HEAD']);
    assert.deepEqual(commitDiff.stdout.trim().split('\n').filter(Boolean), ['tracked.txt']);

    const fileContents = await fs.readFile(path.join(repoDir, 'tracked.txt'), 'utf8');
    assert.equal(fileContents, 'before\nafter\n');
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('continuous-improvement-loop rebases onto the push branch before pushing a stale local branch', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-rebase-push-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  const otherDir = path.join(tempRoot, 'other');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
    await git(['add', 'tracked.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    await execFileAsync('git', ['clone', '--branch', 'main', remoteDir, otherDir], { cwd: tempRoot });
    await execFileAsync('git', ['config', 'user.name', 'Remote Writer'], { cwd: otherDir });
    await execFileAsync('git', ['config', 'user.email', 'remote@example.com'], { cwd: otherDir });
    await fs.writeFile(path.join(otherDir, 'remote-only.txt'), 'remote\n', 'utf8');
    await execFileAsync('git', ['add', 'remote-only.txt'], { cwd: otherDir });
    await execFileAsync('git', ['commit', '-m', 'remote advance'], { cwd: otherDir });
    await execFileAsync('git', ['push', 'origin', 'main'], { cwd: otherDir });

    const configPath = path.join(repoDir, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          pollGitHub: false,
          iterationDelaySeconds: 0,
          ideaCommand:
            "printf '# Rebase before push test\\nSource: test\\nTarget: tracked.txt:1\\nReason: verify the loop rebases stale local commits onto origin/main before push.\\n'",
          implementationCommand: "printf 'local\\n' >> tracked.txt",
          uiAuditCommand: "printf '[]\\n'",
        },
        null,
        2,
      ),
      'utf8',
    );
    await git(['add', 'loop-config.json']);
    await git(['commit', '-m', 'config']);

    await execFileAsync(
      'node',
      [loopScriptPath, '--config', 'loop-config.json', '--max-iterations', '1'],
      { cwd: repoDir },
    );

    const localHead = (await git(['rev-parse', 'HEAD'])).stdout.trim();
    const remoteHead = (await execFileAsync('git', ['--git-dir', remoteDir, 'rev-parse', 'refs/heads/main'], { cwd: tempRoot })).stdout.trim();
    assert.equal(localHead, remoteHead);
    assert.equal(await fs.readFile(path.join(repoDir, 'remote-only.txt'), 'utf8'), 'remote\n');
    assert.equal(await fs.readFile(path.join(repoDir, 'tracked.txt'), 'utf8'), 'before\nlocal\n');
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('continuous-improvement-loop blocks custom UI audit failures even when findings are empty', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-ui-report-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
    await git(['add', 'tracked.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    const configPath = path.join(repoDir, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          pollGitHub: false,
          iterationDelaySeconds: 0,
          ideaCommand:
            "printf '# UI report test\\nSource: test\\nTarget: tracked.txt:1\\nReason: ui audit failure must stop the loop even when findings are empty.\\n'",
          implementationCommand: "printf 'after\\n' >> tracked.txt",
          uiAuditCommand: "printf '{\"ok\":false,\"findings\":[],\"message\":\"blocked\"}\\n'",
        },
        null,
        2,
      ),
      'utf8',
    );
    await git(['add', 'loop-config.json']);
    await git(['commit', '-m', 'config']);

    await assert.rejects(
      () => execFileAsync('node', [loopScriptPath, '--config', 'loop-config.json', '--max-iterations', '1'], { cwd: repoDir }),
      /UI audit did not pass/,
    );

    const log = await git(['log', '-1', '--pretty=%s']);
    assert.equal(log.stdout.trim(), 'config');
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('continuous-improvement-loop blocks custom formal failures even when findings are empty', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-formal-report-test-'));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });

  try {
    await git(['init', '--bare', remoteDir], tempRoot);
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
    await git(['add', 'tracked.txt']);
    await git(['commit', '-m', 'initial']);
    await git(['remote', 'add', 'origin', remoteDir]);
    await git(['push', '-u', 'origin', 'main']);

    const configPath = path.join(repoDir, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          pollGitHub: false,
          iterationDelaySeconds: 0,
          ideaCommand:
            "printf '# Formal report test\\nSource: test\\nTarget: tracked.txt:1\\nReason: formal verification failure must stop the loop even when findings are empty.\\n'",
          implementationCommand: "printf 'after\\n' >> tracked.txt",
          uiAuditCommand: "printf '[]\\n'",
          formalVerifyCommand: "printf '{\"ok\":false,\"findings\":[],\"message\":\"blocked\"}\\n'",
        },
        null,
        2,
      ),
      'utf8',
    );
    await git(['add', 'loop-config.json']);
    await git(['commit', '-m', 'config']);

    await assert.rejects(
      () => execFileAsync('node', [loopScriptPath, '--config', 'loop-config.json', '--max-iterations', '1'], { cwd: repoDir }),
      /Formal verification did not pass/,
    );

    const log = await git(['log', '-1', '--pretty=%s']);
    assert.equal(log.stdout.trim(), 'config');
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('waitForGreenCi retries transient GitHub polling fetch failures', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-ci-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });
  const originalFetch = global.fetch;
  let requestCount = 0;

  try {
    await git(['init', '-b', 'main']);
    await git(['config', 'user.name', 'Codex Test']);
    await git(['config', 'user.email', 'codex@example.com']);
    await git(['remote', 'add', 'origin', 'https://github.com/example/project.git']);

    global.fetch = async (url) => {
      requestCount += 1;

      if (requestCount === 1) {
        throw new TypeError('fetch failed');
      }

      const href = String(url);
      const payload = href.includes('/check-runs')
        ? {
            check_runs: [
              {
                id: 1,
                name: 'Cloudflare Pages',
                status: 'completed',
                conclusion: 'success',
                details_url: 'https://example.com/check',
                html_url: 'https://example.com/check',
              },
            ],
          }
        : {
            workflow_runs: [],
          };

      return {
        ok: true,
        status: 200,
        statusText: 'OK',
        async json() {
          return payload;
        },
        async text() {
          return JSON.stringify(payload);
        },
      };
    };

    const result = await waitForGreenCi(
      repoDir,
      {
        ciTimeoutMinutes: 1,
        pollIntervalSeconds: 0,
        pushRemote: 'origin',
      },
      'deadbeef',
    );

    assert.equal(result.ok, true);
    assert.equal(result.summary.checks.successful.length, 1);
    assert.equal(result.summary.workflows.successful.length, 0);
    assert.equal(result.pollErrors?.length, 1);
    assert.match(result.pollErrors?.[0]?.message ?? '', /fetch failed/i);
    assert.equal(requestCount, 3);
  } finally {
    global.fetch = originalFetch;
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('continuous-improvement-loop repairs the latest remote CI failure before idea discovery', async () => {
  const script = await fs.readFile(loopScriptPath, 'utf8');
  assert.equal(typeof syncAndPollLatestRemoteCi, 'function');
  assert.match(script, /export async function syncAndPollLatestRemoteCi\(repoRoot, config, context = null\)/);
  assert.match(script, /const latestRemote = await syncAndPollLatestRemoteCi\(repoRoot, config, context\);/);
  assert.match(script, /if \(latestRemote\.ciResult && !latestRemote\.ciResult\.ok\)/);
  assert.match(script, /phase: 'latest-commit-ci-repair'/);
  assert.match(script, /await runShellCommand\(config\.ciRepairCommand, repoRoot, context,/);
  assert.match(
    script,
    /if \(!\(await hasCommitCandidateChanges\(repoRoot, config\.initialUntracked, config\.initialTrackedPaths\)\)\) \{/,
  );
  assert.match(script, /if \(!repairLatestCommitOnly\) \{/);
});

test('continuous-improvement-loop prefers the stored gh login over a stale GH_TOKEN environment value', async () => {
  const script = await fs.readFile(loopScriptPath, 'utf8');
  assert.match(script, /function buildSanitizedGhAuthEnv\(\)/);
  assert.match(script, /env: buildSanitizedGhAuthEnv\(\),/);
  assert.match(script, /execFileSync\('gh', \['auth', 'token'\], \{/);
  assert.match(script, /cachedGitHubToken = process\.env\.GITHUB_TOKEN \?\? process\.env\.GH_TOKEN \?\? process\.env\.GITHUB_PAT \?\? '';/);
});

test('tdf-hq-ui Jest config keeps preset as a preset root specifier', () => {
  const repoDir = path.resolve(testDir, '..', '..');
  const uiDir = path.join(repoDir, 'tdf-hq-ui');
  const requireFromUi = createRequire(path.join(uiDir, 'package.json'));
  const config = requireFromUi('./jest.config.cjs');

  assert.equal(typeof config.preset, 'string');
  assert.notEqual(path.basename(config.preset), 'jest-preset.js');
  assert.ok(requireFromUi.resolve(`${config.preset}/jest-preset.js`).endsWith(path.join('ts-jest', 'presets', 'default-esm', 'jest-preset.js')));
});
