import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';
import {
  buildCommitContext,
  expandTemplate,
  parseIdeaMarkdown,
  parseGitHubRemote,
  verifyImprovementLoopModel,
} from '../lib/continuous-improvement-loop.mjs';
import { auditUiSource } from '../lib/ui-static-audit.mjs';

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

test('verifyImprovementLoopModel passes its own safety checks', () => {
  const report = verifyImprovementLoopModel();

  assert.equal(report.ok, true);
  assert.equal(report.findings.length, 0);
  assert.ok(report.reachableStates.includes('pollCi'));
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
