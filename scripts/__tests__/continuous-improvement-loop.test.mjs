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
import {
  VERIFICATION_GATES,
  parsePorcelainPaths,
  selectVerificationGates,
} from '../lib/continuous-improvement-loop-verify.mjs';
import {
  buildIterationBranchName,
  buildPullRequestBody,
  countOpenLoopPullRequests,
  isLinkedWorktree,
  openPullRequest,
  prepareIterationBranch,
  syncSubmodulesRecursively,
  validateLoopConfig,
  waitForGreenCi,
} from '../continuous-improvement-loop.mjs';
import {
  buildDefaultIdea,
  chooseDiscoveryLane,
  discoverImprovementIdea,
  verifyDiscoveryPolicyModel,
} from '../lib/discovery.mjs';
import { findExtractedFlyctlBinary, selectFlyctlDownloadUrl } from '../lib/flyctl-cli.mjs';
import { auditUiSource } from '../lib/ui-static-audit.mjs';
import { installFlyctl } from '../install-flyctl.mjs';

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const loopScriptPath = path.resolve(testDir, '..', 'continuous-improvement-loop.mjs');
const loopPollingScriptPath = path.resolve(testDir, '..', 'set-loop-github-polling.mjs');

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

const LOOP_BRANCH_PREFIX = 'continuous-improvement-loop/';

async function createLoopFixture(prefix) {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), prefix));
  const remoteDir = path.join(tempRoot, 'remote.git');
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);

  const git = async (args, cwd = repoDir) => execFileAsync('git', args, { cwd });
  const remoteGit = async (args) => execFileAsync('git', ['--git-dir', remoteDir, ...args], { cwd: tempRoot });

  await git(['init', '--bare', remoteDir], tempRoot);
  await git(['init', '-b', 'main']);
  await git(['config', 'user.name', 'Codex Test']);
  await git(['config', 'user.email', 'codex@example.com']);
  await fs.writeFile(path.join(repoDir, 'tracked.txt'), 'before\n', 'utf8');
  await git(['add', 'tracked.txt']);
  await git(['commit', '-m', 'initial']);
  await git(['remote', 'add', 'origin', remoteDir]);
  await git(['push', '-u', 'origin', 'main']);

  // The config lives outside the repository so the loop starts from a clean worktree.
  const writeConfig = async (overrides = {}) => {
    const configPath = path.join(tempRoot, 'loop-config.json');
    await fs.writeFile(
      configPath,
      JSON.stringify(
        {
          requireLinkedWorktree: false,
          openPullRequest: false,
          pollGitHub: false,
          iterationDelaySeconds: 0,
          verifyCommand: 'true',
          ideaCommand:
            "printf '# Fixture idea\\nSource: test\\nTarget: tracked.txt:1\\nReason: exercise the loop end to end.\\n'",
          implementationCommand: "printf 'after\\n' >> tracked.txt",
          uiAuditCommand: "printf '[]\\n'",
          ...overrides,
        },
        null,
        2,
      ),
      'utf8',
    );
    return configPath;
  };

  const runLoop = async (configPath, extraArgs = []) =>
    execFileAsync('node', [loopScriptPath, '--config', configPath, '--max-iterations', '1', ...extraArgs], {
      cwd: repoDir,
    });

  const remoteLoopBranches = async () =>
    (await remoteGit(['for-each-ref', '--format=%(refname:short)', `refs/heads/${LOOP_BRANCH_PREFIX}`])).stdout
      .trim()
      .split('\n')
      .filter(Boolean);

  const remoteMainSha = async () => (await remoteGit(['rev-parse', 'refs/heads/main'])).stdout.trim();

  const cleanup = async () => fs.rm(tempRoot, { recursive: true, force: true });

  return { tempRoot, remoteDir, repoDir, git, remoteGit, writeConfig, runLoop, remoteLoopBranches, remoteMainSha, cleanup };
}

test('buildIterationBranchName puts a sortable UTC timestamp and the iteration under the prefix', () => {
  const now = new Date('2026-11-13T15:04:05.678Z');
  assert.equal(buildIterationBranchName({}, 3, now), 'continuous-improvement-loop/20261113-150405-3');
  assert.equal(buildIterationBranchName({ branchPrefix: 'bot/loop-' }, 1, now), 'bot/loop-20261113-150405-1');
});

test('validateLoopConfig refuses every way of targeting the base branch or skipping verification', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-config-guard-test-');
  const valid = { verifyCommand: 'true', requireLinkedWorktree: false };

  try {
    await assert.rejects(
      () => validateLoopConfig(fixture.repoDir, { ...valid, allowPushToMain: true }),
      /Pushing to main is no longer supported/,
    );

    process.env.CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN = '1';
    await assert.rejects(() => validateLoopConfig(fixture.repoDir, valid), /Pushing to main is no longer supported/);
    delete process.env.CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN;

    await assert.rejects(
      () => validateLoopConfig(fixture.repoDir, { ...valid, pushBranch: 'main' }),
      /"pushBranch" is no longer supported/,
    );
    for (const branchPrefix of ['', 'main', 'loop/../x/', '/loop/']) {
      await assert.rejects(
        () => validateLoopConfig(fixture.repoDir, { ...valid, branchPrefix }),
        /Invalid branchPrefix/,
      );
    }
    await assert.rejects(
      () => validateLoopConfig(fixture.repoDir, { requireLinkedWorktree: false }),
      /verifyCommand is required/,
    );

    const accepted = await validateLoopConfig(fixture.repoDir, valid);
    assert.deepEqual(accepted, {
      baseBranch: 'main',
      branchPrefix: LOOP_BRANCH_PREFIX,
      pushingEnabled: true,
      requireLinkedWorktree: false,
    });

    // A dry run commits nothing, so it needs neither a verification gate nor isolation.
    const dryRun = await validateLoopConfig(fixture.repoDir, { dryRun: true });
    assert.equal(dryRun.pushingEnabled, false);
  } finally {
    delete process.env.CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN;
    await fixture.cleanup();
  }
});

test('validateLoopConfig only runs in a dedicated linked worktree by default', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-worktree-guard-test-');
  const linkedDir = path.join(fixture.tempRoot, 'loop-worktree');

  try {
    assert.equal(await isLinkedWorktree(fixture.repoDir), false);
    await assert.rejects(
      () => validateLoopConfig(fixture.repoDir, { verifyCommand: 'true' }),
      /must run in its own linked git worktree, not the primary checkout/,
    );

    await fixture.git(['worktree', 'add', '--detach', linkedDir, 'origin/main']);
    assert.equal(await isLinkedWorktree(linkedDir), true);
    const accepted = await validateLoopConfig(linkedDir, { verifyCommand: 'true' });
    assert.equal(accepted.requireLinkedWorktree, true);
  } finally {
    await fixture.cleanup();
  }
});

test('set-loop-github-polling toggles pollGitHub in a target config file', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-polling-toggle-test-'));
  const configPath = path.join(tempRoot, 'loop-config.json');

  try {
    await fs.writeFile(
      configPath,
      `${JSON.stringify({ pushRemote: 'origin', pollGitHub: false, iterationDelaySeconds: 0 }, null, 2)}\n`,
      'utf8',
    );

    await execFileAsync('node', [loopPollingScriptPath, 'on', '--config', configPath], { cwd: tempRoot });
    let config = JSON.parse(await fs.readFile(configPath, 'utf8'));
    assert.equal(config.pollGitHub, true);

    const status = await execFileAsync('node', [loopPollingScriptPath, 'status', '--config', configPath], {
      cwd: tempRoot,
    });
    assert.match(status.stdout, /pollGitHub=true/);

    await execFileAsync('node', [loopPollingScriptPath, 'off', '--config', configPath], { cwd: tempRoot });
    config = JSON.parse(await fs.readFile(configPath, 'utf8'));
    assert.equal(config.pollGitHub, false);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
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

test('selectFlyctlDownloadUrl prefers amd64 Linux tarballs over weaker fallbacks', () => {
  const selected = selectFlyctlDownloadUrl([
    '',
    'null',
    'https://example.com/flyctl_0.4.33_Linux_arm64.tar.gz',
    'https://example.com/flyctl_0.4.33_Linux_x86_64.tar.gz',
    'https://example.com/checksums.txt',
  ]);

  assert.equal(selected, 'https://example.com/flyctl_0.4.33_Linux_x86_64.tar.gz');
});

test('findExtractedFlyctlBinary ignores similarly named archives and only returns an executable binary', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'flyctl-binary-search-test-'));
  const nestedDir = path.join(tempRoot, 'nested');
  const binaryPath = path.join(nestedDir, 'flyctl');

  try {
    await fs.mkdir(nestedDir, { recursive: true });
    await fs.writeFile(path.join(tempRoot, 'flyctl.tar.gz'), 'not a binary\n', 'utf8');
    await fs.writeFile(binaryPath, '#!/usr/bin/env bash\nprintf "flyctl v0.4.33\\n"\n', { encoding: 'utf8', mode: 0o755 });

    const selected = await findExtractedFlyctlBinary(tempRoot);
    assert.equal(selected, binaryPath);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('installFlyctl installs the extracted executable instead of the downloaded archive', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'flyctl-install-test-'));
  const payloadDir = path.join(tempRoot, 'payload');
  const installDir = path.join(tempRoot, 'bin');
  const archivePath = path.join(tempRoot, 'flyctl_0.4.33_Linux_x86_64.tar.gz');
  const originalFetch = global.fetch;

  try {
    await fs.mkdir(payloadDir, { recursive: true });
    await fs.writeFile(
      path.join(payloadDir, 'flyctl'),
      '#!/usr/bin/env bash\nprintf "flyctl v0.4.33\\n"\n',
      { encoding: 'utf8', mode: 0o755 },
    );
    await execFileAsync('tar', ['-czf', archivePath, '-C', payloadDir, '.']);

    const archiveBytes = await fs.readFile(archivePath);
    const releaseApiUrl = 'https://example.com/flyctl/releases/latest';
    const tarballUrl = 'https://example.com/flyctl_0.4.33_Linux_x86_64.tar.gz';
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

    const result = await installFlyctl({
      githubToken,
      installDir,
      log: () => {},
      releaseApiUrl,
    });

    assert.equal(result.downloadUrl, tarballUrl);
    assert.equal(path.basename(result.binPath), 'flyctl');

    const { stdout } = await execFileAsync(path.join(installDir, 'flyctl'), ['version']);
    assert.equal(stdout.trim(), 'flyctl v0.4.33');
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

test('chooseDiscoveryLane keeps critical audit precedence ahead of importance and rotation', () => {
  assert.equal(
    chooseDiscoveryLane(
      {
        logical: { lane: 'logical', priority: 0, importance: 1, idea: { source: 'logical-critical' } },
        formal: { lane: 'formal', priority: 0, importance: 999, idea: { source: 'formal-critical' } },
        ux: { lane: 'ux', priority: 0, importance: 999, idea: { source: 'ux-critical' } },
        ui: { lane: 'ui', priority: 0, importance: 999, idea: { source: 'ui-critical' } },
      },
      { lastLane: 'logical' },
    ),
    'logical',
  );

  assert.equal(
    chooseDiscoveryLane(
      {
        formal: { lane: 'formal', priority: 0, importance: 1, idea: { source: 'formal-critical' } },
        ux: { lane: 'ux', priority: 0, importance: 999, idea: { source: 'ux-critical' } },
        ui: { lane: 'ui', priority: 0, importance: 999, idea: { source: 'ui-critical' } },
      },
      { lastLane: 'formal' },
    ),
    'formal',
  );

  assert.equal(
    chooseDiscoveryLane(
      {
        ux: { lane: 'ux', priority: 0, importance: 1, idea: { source: 'ux-critical' } },
        ui: { lane: 'ui', priority: 0, importance: 999, idea: { source: 'ui-critical' } },
        backend: { lane: 'backend', priority: 0, importance: 999, idea: { source: 'backend-critical' } },
      },
      { lastLane: 'ux' },
    ),
    'ux',
  );
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
    await fs.writeFile(
      path.join(repoDir, 'tdf-hq', 'app', 'Main.hs'),
      'module Main (main) where\n\nmain :: IO ()\nmain = putStrLn "ok"\n',
      'utf8',
    );
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

test('auditUiSource recognizes accessible names passed through TextField inputProps', () => {
  const source = `
    <TextField inputProps={{ 'aria-label': 'Buscar contactos' }} />
    <TextField
      inputProps={{
        'aria-labelledby': \`video-\${asset.id}-label video-status-column\`,
      }}
    />
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

test('continuous-improvement-loop publishes each iteration on its own branch and never moves main', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-branch-test-');

  try {
    const mainBefore = await fixture.remoteMainSha();
    const localMainBefore = (await fixture.git(['rev-parse', 'refs/heads/main'])).stdout.trim();
    const configPath = await fixture.writeConfig();

    await fixture.runLoop(configPath);

    assert.equal(await fixture.remoteMainSha(), mainBefore);
    assert.equal((await fixture.git(['rev-parse', 'refs/heads/main'])).stdout.trim(), localMainBefore);

    const branches = await fixture.remoteLoopBranches();
    assert.equal(branches.length, 1);
    assert.match(branches[0], /^continuous-improvement-loop\/\d{8}-\d{6}-1$/);

    const diff = await fixture.remoteGit(['diff', '--name-only', 'refs/heads/main', `refs/heads/${branches[0]}`]);
    assert.deepEqual(diff.stdout.trim().split('\n').filter(Boolean), ['tracked.txt']);
    const parent = (await fixture.remoteGit(['rev-parse', `refs/heads/${branches[0]}~1`])).stdout.trim();
    assert.equal(parent, mainBefore);

    // The worktree is left clean, detached at the base, with no leftover local loop branch.
    assert.equal((await fixture.git(['status', '--short'])).stdout.trim(), '');
    assert.equal((await fixture.git(['branch', '--show-current'])).stdout.trim(), '');
    assert.equal((await fixture.git(['rev-parse', 'HEAD'])).stdout.trim(), mainBefore);
    const localLoopBranches = await fixture.git(['for-each-ref', `refs/heads/${LOOP_BRANCH_PREFIX}`]);
    assert.equal(localLoopBranches.stdout.trim(), '');
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop branches from the latest remote base, not a stale local one', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-latest-base-test-');
  const otherDir = path.join(fixture.tempRoot, 'other');

  try {
    await execFileAsync('git', ['clone', '--branch', 'main', fixture.remoteDir, otherDir], { cwd: fixture.tempRoot });
    await execFileAsync('git', ['config', 'user.name', 'Remote Writer'], { cwd: otherDir });
    await execFileAsync('git', ['config', 'user.email', 'remote@example.com'], { cwd: otherDir });
    await fs.writeFile(path.join(otherDir, 'remote-only.txt'), 'remote\n', 'utf8');
    await execFileAsync('git', ['add', 'remote-only.txt'], { cwd: otherDir });
    await execFileAsync('git', ['commit', '-m', 'remote advance'], { cwd: otherDir });
    await execFileAsync('git', ['push', 'origin', 'main'], { cwd: otherDir });
    const advancedMain = await fixture.remoteMainSha();

    await fixture.runLoop(await fixture.writeConfig());

    assert.equal(await fixture.remoteMainSha(), advancedMain);
    const [branch] = await fixture.remoteLoopBranches();
    const parent = (await fixture.remoteGit(['rev-parse', `refs/heads/${branch}~1`])).stdout.trim();
    assert.equal(parent, advancedMain);
    const files = await fixture.remoteGit(['ls-tree', '--name-only', `refs/heads/${branch}`]);
    assert.deepEqual(files.stdout.trim().split('\n').sort(), ['remote-only.txt', 'tracked.txt']);
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop honors --allow-dirty for tracked baseline changes without committing them', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-allow-dirty-test-');

  try {
    await fs.writeFile(path.join(fixture.repoDir, 'tracked.txt'), 'before\nlocal dirty change\n', 'utf8');
    const configPath = await fixture.writeConfig({
      implementationCommand: "printf 'created by loop\\n' > new-file.txt",
    });

    await fixture.runLoop(configPath, ['--allow-dirty']);

    const [branch] = await fixture.remoteLoopBranches();
    const diff = await fixture.remoteGit(['diff', '--name-only', 'refs/heads/main', `refs/heads/${branch}`]);
    assert.deepEqual(diff.stdout.trim().split('\n').filter(Boolean), ['new-file.txt']);

    const status = await fixture.git(['status', '--short']);
    assert.match(status.stdout, /^ M tracked\.txt$/m);
    assert.equal(
      await fs.readFile(path.join(fixture.repoDir, 'tracked.txt'), 'utf8'),
      'before\nlocal dirty change\n',
    );
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop discards an iteration whose verification fails and publishes nothing', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-verify-fail-test-');

  try {
    const mainBefore = await fixture.remoteMainSha();
    const configPath = await fixture.writeConfig({
      implementationCommand: "printf 'after\\n' >> tracked.txt && printf 'junk\\n' > junk.txt",
      verifyCommand: "printf 'tests failed\\n' >&2; exit 3",
    });

    await assert.rejects(() => fixture.runLoop(configPath), /Verification failed with exit code 3/);

    assert.equal(await fixture.remoteMainSha(), mainBefore);
    assert.deepEqual(await fixture.remoteLoopBranches(), []);
    assert.equal((await fixture.git(['status', '--short'])).stdout.trim(), '');
    assert.equal(await fs.readFile(path.join(fixture.repoDir, 'tracked.txt'), 'utf8'), 'before\n');
    await assert.rejects(() => fs.access(path.join(fixture.repoDir, 'junk.txt')));
    assert.equal((await fixture.git(['rev-parse', 'HEAD'])).stdout.trim(), mainBefore);
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop discards partial edits when a worker step fails', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-worker-fail-test-');

  try {
    const configPath = await fixture.writeConfig({
      implementationCommand: "printf 'half done\\n' >> tracked.txt; printf 'junk\\n' > junk.txt; exit 75",
    });

    await assert.rejects(() => fixture.runLoop(configPath), /Implementation failed with exit code 75/);

    assert.deepEqual(await fixture.remoteLoopBranches(), []);
    assert.equal((await fixture.git(['status', '--short'])).stdout.trim(), '');
    assert.equal(await fs.readFile(path.join(fixture.repoDir, 'tracked.txt'), 'utf8'), 'before\n');
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop moves on to the next iteration after a discarded one', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-continue-test-');
  const marker = path.join(fixture.tempRoot, 'first-attempt-done');

  try {
    const configPath = await fixture.writeConfig({
      failureDelaySeconds: 0,
      implementationCommand: `if [ -f '${marker}' ]; then printf 'after\\n' >> tracked.txt; else : > '${marker}'; printf 'bad\\n' >> tracked.txt; exit 1; fi`,
    });

    await execFileAsync('node', [loopScriptPath, '--config', configPath, '--max-iterations', '2'], {
      cwd: fixture.repoDir,
    });

    const branches = await fixture.remoteLoopBranches();
    assert.equal(branches.length, 1);
    assert.match(branches[0], /-2$/);
    const content = await fixture.remoteGit(['show', `refs/heads/${branches[0]}:tracked.txt`]);
    assert.equal(content.stdout, 'before\nafter\n');
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop stops a step that exceeds its timeout, including its children', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-timeout-test-');
  const pidFile = path.join(fixture.tempRoot, 'grandchild.pid');

  try {
    const configPath = await fixture.writeConfig({
      stepTimeoutMinutes: 0.02,
      implementationCommand: `sh -c 'sleep 300 & echo $! > "${pidFile}"; wait'`,
    });

    await assert.rejects(() => fixture.runLoop(configPath), /Implementation timed out after/);

    const grandchildPid = Number((await fs.readFile(pidFile, 'utf8')).trim());
    assert.ok(grandchildPid > 0);
    assert.throws(() => process.kill(grandchildPid, 0), /ESRCH/);
  } finally {
    await fixture.cleanup();
  }
});

test('continuous-improvement-loop recovers its own worktree after an interrupted run but never cleans a primary checkout', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-recover-test-');
  const linkedDir = path.join(fixture.tempRoot, 'loop-worktree');

  try {
    await fixture.git(['worktree', 'add', '--detach', linkedDir, 'origin/main']);
    await fs.writeFile(path.join(linkedDir, 'tracked.txt'), 'before\ninterrupted\n', 'utf8');
    await fs.writeFile(path.join(linkedDir, 'leftover.txt'), 'leftover\n', 'utf8');
    const linkedConfig = await fixture.writeConfig({ requireLinkedWorktree: true });

    await execFileAsync('node', [loopScriptPath, '--config', linkedConfig, '--max-iterations', '1'], { cwd: linkedDir });

    const [branch] = await fixture.remoteLoopBranches();
    const content = await fixture.remoteGit(['show', `refs/heads/${branch}:tracked.txt`]);
    assert.equal(content.stdout, 'before\nafter\n');
    const files = await fixture.remoteGit(['ls-tree', '--name-only', `refs/heads/${branch}`]);
    assert.deepEqual(files.stdout.trim().split('\n'), ['tracked.txt']);

    // In a primary checkout the same situation is an error and the files are left alone.
    await fs.writeFile(path.join(fixture.repoDir, 'tracked.txt'), 'before\nmy work\n', 'utf8');
    const primaryConfig = await fixture.writeConfig();
    await assert.rejects(() => fixture.runLoop(primaryConfig), /requires a clean tracked worktree/);
    assert.equal(await fs.readFile(path.join(fixture.repoDir, 'tracked.txt'), 'utf8'), 'before\nmy work\n');
  } finally {
    await fixture.cleanup();
  }
});

test('prepareIterationBranch clears leftovers from an interrupted iteration', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-prepare-test-');

  try {
    const mainSha = await fixture.remoteMainSha();
    await fixture.git(['checkout', '-b', `${LOOP_BRANCH_PREFIX}stale-1`]);
    await fs.writeFile(path.join(fixture.repoDir, 'tracked.txt'), 'before\nabandoned\n', 'utf8');
    await fs.writeFile(path.join(fixture.repoDir, 'abandoned.txt'), 'abandoned\n', 'utf8');
    await fixture.git(['branch', 'feature/keep-me']);

    const branch = await prepareIterationBranch(fixture.repoDir, { pushRemote: 'origin' }, 7);

    assert.equal(branch.baseSha, mainSha);
    assert.equal(branch.baseRef, 'origin/main');
    assert.equal((await fixture.git(['branch', '--show-current'])).stdout.trim(), branch.branchName);
    assert.equal((await fixture.git(['status', '--short'])).stdout.trim(), '');
    assert.equal(await fs.readFile(path.join(fixture.repoDir, 'tracked.txt'), 'utf8'), 'before\n');

    const localBranches = (await fixture.git(['for-each-ref', '--format=%(refname:short)', 'refs/heads/'])).stdout
      .trim()
      .split('\n')
      .sort();
    // Only the loop's own stale branch is removed; unrelated branches are never touched.
    assert.deepEqual(localBranches, [branch.branchName, 'feature/keep-me', 'main'].sort());
  } finally {
    await fixture.cleanup();
  }
});

test('pull requests are opened as drafts against the base branch and counted for the backlog limit', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-pr-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);
  const originalFetch = global.fetch;
  const requests = [];

  try {
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await execFileAsync('git', ['remote', 'add', 'origin', 'https://github.com/example/project.git'], { cwd: repoDir });

    global.fetch = async (url, options = {}) => {
      requests.push({ url: String(url), method: options.method, body: options.body ? JSON.parse(options.body) : null });
      const payload =
        options.method === 'POST'
          ? { number: 42, html_url: 'https://github.com/example/project/pull/42' }
          : [
              { head: { ref: 'continuous-improvement-loop/20261113-000000-1' } },
              { head: { ref: 'feature/unrelated' } },
              { head: { ref: 'continuous-improvement-loop/20261113-000100-2' } },
            ];
      return new Response(JSON.stringify(payload), { status: 200, headers: { 'Content-Type': 'application/json' } });
    };

    const config = { pushRemote: 'origin', verifyCommand: 'npm test' };
    assert.equal(await countOpenLoopPullRequests(repoDir, config), 2);

    const branch = {
      branchName: 'continuous-improvement-loop/20261113-000200-3',
      baseBranch: 'main',
      baseRef: 'origin/main',
      baseSha: 'abc123',
    };
    const context = {
      last_commit_message: 'fix(ui): label the close button\n\nbody',
      idea_title: 'Label the close button',
      idea_source: 'ui-audit',
      idea_target: 'tdf-hq-ui/src/Close.tsx:3',
      idea_reason: 'Icon button has no accessible name.',
    };
    const pullRequest = await openPullRequest(repoDir, config, context, branch);
    assert.deepEqual(pullRequest, { number: 42, url: 'https://github.com/example/project/pull/42' });

    const post = requests.find((request) => request.method === 'POST');
    assert.match(post.url, /\/repos\/example\/project\/pulls$/);
    assert.equal(post.body.title, 'fix(ui): label the close button');
    assert.equal(post.body.head, branch.branchName);
    assert.equal(post.body.base, 'main');
    assert.equal(post.body.draft, true);
    assert.equal(post.body.body, buildPullRequestBody(config, context, branch));
    assert.match(post.body.body, /stays a draft until a person reviews it/);
    assert.match(post.body.body, /`npm test` passed/);
  } finally {
    global.fetch = originalFetch;
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('selectVerificationGates always runs the repository gate and adds UI and backend gates by path', () => {
  assert.deepEqual(selectVerificationGates([]).gates, []);
  assert.deepEqual(selectVerificationGates(['docs/guide.md']).gates, ['repo']);
  assert.deepEqual(selectVerificationGates(['./tdf-hq-ui/src/App.tsx']).gates, ['repo', 'ui']);
  assert.deepEqual(selectVerificationGates(['package-lock.json']).gates, ['repo', 'ui']);
  assert.equal(selectVerificationGates(['package-lock.json']).reinstallDependencies, true);
  assert.deepEqual(selectVerificationGates(['tdf-hq/src/Main.hs']).gates, ['repo', 'backend']);
  assert.deepEqual(selectVerificationGates(['tdf-hq/src/Main.hs', 'tdf-hq-ui/src/App.tsx']).gates, [
    'repo',
    'ui',
    'backend',
  ]);

  const mobile = selectVerificationGates(['scripts/x.mjs', 'tdf-mobile']);
  assert.equal(mobile.ok, false);
  assert.match(mobile.reason, /tdf-mobile submodule cannot be verified/);

  assert.equal(VERIFICATION_GATES.backend.env.REQUIRE_STACK, '1');
  assert.ok(VERIFICATION_GATES.backend.minFreeMegabytes > 0);
});

test('parsePorcelainPaths reads NUL-separated status entries including both sides of a rename', () => {
  const status = ' M tdf-hq-ui/src/App.tsx\0?? new file.txt\0R  renamed/to.txt\0renamed/from.txt\0D  gone.txt\0';
  assert.deepEqual(parsePorcelainPaths(status), [
    'tdf-hq-ui/src/App.tsx',
    'new file.txt',
    'renamed/to.txt',
    'renamed/from.txt',
    'gone.txt',
  ]);
});

test('loop verify gate refuses to pass submodule changes and reports its plan', async () => {
  const fixture = await createLoopFixture('continuous-improvement-loop-verify-plan-test-');
  const verifyScriptPath = path.resolve(testDir, '..', 'continuous-improvement-loop-verify.mjs');

  try {
    const clean = await execFileAsync('node', [verifyScriptPath, '--plan'], { cwd: fixture.repoDir });
    assert.match(clean.stdout, /Nothing to verify/);

    await fs.mkdir(path.join(fixture.repoDir, 'tdf-hq-ui'), { recursive: true });
    await fs.writeFile(path.join(fixture.repoDir, 'tdf-hq-ui', 'App.tsx'), 'export {};\n', 'utf8');
    const plan = await execFileAsync('node', [verifyScriptPath, '--plan'], { cwd: fixture.repoDir });
    assert.match(plan.stdout, /gate\(s\): repo, ui/);

    await fs.mkdir(path.join(fixture.repoDir, 'tdf-mobile'), { recursive: true });
    await fs.writeFile(path.join(fixture.repoDir, 'tdf-mobile', 'index.js'), '\n', 'utf8');
    await assert.rejects(
      () => execFileAsync('node', [verifyScriptPath, '--plan'], { cwd: fixture.repoDir }),
      /Verification refused: changes under the tdf-mobile submodule/,
    );
  } finally {
    await fixture.cleanup();
  }
});

test('a second supervisor leaves the running supervisor lock and child PID file alone', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-lock-test-'));
  const stateDir = path.join(tempRoot, 'state');
  const lockDir = path.join(stateDir, 'lock');
  const childPidFile = path.join(stateDir, 'child.pid');
  const supervisorPath = path.resolve(testDir, '..', 'continuous-improvement-loop-supervisor.sh');

  try {
    await fs.mkdir(lockDir, { recursive: true });
    // This test process stands in for the supervisor that already holds the lock.
    await fs.writeFile(path.join(lockDir, 'pid'), `${process.pid}\n`, 'utf8');
    await fs.writeFile(childPidFile, '12345\n', 'utf8');

    await assert.rejects(
      () =>
        execFileAsync('bash', [supervisorPath, path.join(tempRoot, 'missing-config.json')], {
          env: {
            ...process.env,
            CONTINUOUS_LOOP_STATE_DIR: stateDir,
            CONTINUOUS_LOOP_LOG_FILE: path.join(tempRoot, 'loop.log'),
            CONTINUOUS_LOOP_PID_FILE: path.join(tempRoot, 'loop.pid'),
            CONTINUOUS_LOOP_CHILD_PID_FILE: childPidFile,
            CONTINUOUS_LOOP_STATUS_FILE: path.join(stateDir, 'status.json'),
            CONTINUOUS_LOOP_STOP_FILE: path.join(stateDir, 'stop'),
            CONTINUOUS_LOOP_HEARTBEAT_FILE: path.join(stateDir, 'heartbeat.txt'),
            CONTINUOUS_LOOP_LOCK_DIR: lockDir,
          },
        }),
      /supervisor already running \(pid \d+/,
    );

    assert.equal((await fs.readFile(path.join(lockDir, 'pid'), 'utf8')).trim(), String(process.pid));
    assert.equal(await fs.readFile(childPidFile, 'utf8'), '12345\n');
    await assert.rejects(() => fs.access(path.join(tempRoot, 'loop.pid')));
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('supervisor blocks instead of starting a child when no dedicated worktree is configured', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-supervisor-block-test-'));
  const stateDir = path.join(tempRoot, 'state');
  const statusFile = path.join(stateDir, 'status.json');
  const stopFile = path.join(stateDir, 'stop');
  const logFile = path.join(tempRoot, 'loop.log');
  const supervisorPath = path.resolve(testDir, '..', 'continuous-improvement-loop-supervisor.sh');
  const env = { ...process.env };
  delete env.CONTINUOUS_LOOP_WORKTREE;

  try {
    const supervisor = execFileAsync('bash', [supervisorPath, path.join(tempRoot, 'missing-config.json')], {
      env: {
        ...env,
        CONTINUOUS_LOOP_STATE_DIR: stateDir,
        CONTINUOUS_LOOP_LOG_FILE: logFile,
        CONTINUOUS_LOOP_PID_FILE: path.join(tempRoot, 'loop.pid'),
        CONTINUOUS_LOOP_CHILD_PID_FILE: path.join(stateDir, 'child.pid'),
        CONTINUOUS_LOOP_STATUS_FILE: statusFile,
        CONTINUOUS_LOOP_STOP_FILE: stopFile,
        CONTINUOUS_LOOP_HEARTBEAT_FILE: path.join(stateDir, 'heartbeat.txt'),
        CONTINUOUS_LOOP_LOCK_DIR: path.join(stateDir, 'lock'),
        CONTINUOUS_LOOP_SUPERVISOR_POLL_SECONDS: '1',
      },
    });

    let status = null;
    for (let attempt = 0; attempt < 100; attempt += 1) {
      try {
        status = JSON.parse(await fs.readFile(statusFile, 'utf8'));
        if (status.state === 'blocked') break;
      } catch {
        // status file not written yet
      }
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    assert.equal(status?.state, 'blocked');
    assert.match(status.details, /CONTINUOUS_LOOP_WORKTREE is not set/);
    assert.equal(status.childPid, null);

    // Let it poll a few times, then ask it to stop; the blocker is logged once, not per poll.
    await new Promise((resolve) => setTimeout(resolve, 3500));
    await fs.writeFile(stopFile, '', 'utf8');
    await supervisor;

    const log = await fs.readFile(logFile, 'utf8');
    assert.equal(log.match(/CONTINUOUS_LOOP_WORKTREE is not set/g).length, 1);
    assert.match(log, /stop requested; supervisor exiting/);
    await assert.rejects(() => fs.access(path.join(stateDir, 'lock')));
  } finally {
    await fs.writeFile(stopFile, '', 'utf8').catch(() => {});
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
          requireLinkedWorktree: false,
          openPullRequest: false,
          verifyCommand: 'true',
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
    await git(['push', 'origin', 'main']);

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
          requireLinkedWorktree: false,
          openPullRequest: false,
          verifyCommand: 'true',
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
    await git(['push', 'origin', 'main']);

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

test('continuous-improvement-loop never merges branches into the base and repairs base CI before idea discovery', async () => {
  const script = await fs.readFile(loopScriptPath, 'utf8');
  // The branch sweep that merged every branch into main and deleted it is gone for good.
  assert.doesNotMatch(script, /reconcileNonMainBranchesOntoMain|pruneMergedRefsOnMain|mergeRefOntoMain/);
  assert.doesNotMatch(script, /'--delete'/);
  assert.doesNotMatch(script, /\['merge', '(?!--abort)/);
  assert.doesNotMatch(script, /\['rebase', '--autostash'/);
  assert.match(script, /await execText\('git', \['push', remoteName, `HEAD:refs\/heads\/\$\{branchName\}`\], repoRoot\);/);
  assert.match(script, /const branch = await prepareIterationBranch\(repoRoot, config, iteration\);/);
  assert.match(script, /const baseCiResult = await pollBaseBranchCi\(repoRoot, config, context, branch\);/);
  assert.match(script, /if \(baseCiResult && !baseCiResult\.ok\)/);
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
  assert.match(script, /function envGitHubToken\(\)/);
  assert.match(script, /function envGitHubTokenLooksInvalid\(statusOutput = ''\)/);
  assert.match(script, /env: buildSanitizedGhAuthEnv\(\),/);
  assert.match(script, /execFileSync\('gh', \['auth', 'token'\], \{/);
  assert.match(script, /execFileSync\('gh', \['auth', 'status'\], \{/);
  assert.match(script, /if \(envGitHubTokenLooksInvalid\(statusOutput\)\) \{/);
});

test('loop start path validates config before launching the supervisor', async () => {
  const startScript = await fs.readFile(path.resolve(testDir, '..', 'start-continuous-improvement-loop.sh'), 'utf8');
  const supervisorScript = await fs.readFile(
    path.resolve(testDir, '..', 'continuous-improvement-loop-supervisor.sh'),
    'utf8',
  );

  assert.match(startScript, /--validate-config-only/);
  assert.match(supervisorScript, /--validate-config-only/);
  assert.match(supervisorScript, /gh_auth_token_available\(\)/);
  assert.match(supervisorScript, /GH_TOKEN= GITHUB_TOKEN= GITHUB_PAT= gh auth token/);
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
