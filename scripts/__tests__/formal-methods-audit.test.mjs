import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

import {
  collectFailingFormalFindings,
  collectFormalFindings,
  findingMeetsSeverityThreshold,
  formatFormalFinding,
  summarizeFormalFindings,
} from '../lib/formal-methods-audit.mjs';

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'audit-formal-methods.mjs');

async function writeFile(repoDir, filePath, content) {
  const fullPath = path.join(repoDir, filePath);
  await fs.mkdir(path.dirname(fullPath), { recursive: true });
  await fs.writeFile(fullPath, content, 'utf8');
}

async function initTrackedRepo(files) {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'formal-methods-audit-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  await fs.mkdir(repoDir);
  await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });

  for (const [filePath, content] of files) {
    await writeFile(repoDir, filePath, content);
  }

  await execFileAsync('git', ['add', '.'], { cwd: repoDir });
  return { tempRoot, repoDir };
}

test('formal audit scans active source and skips archive or generated code', async () => {
  const { tempRoot, repoDir } = await initTrackedRepo([
    [
      'src/Active.hs',
      [
        'module Active where',
        '',
        'import System.IO (IOMode(ReadMode), openFile)',
        '',
        'readActive :: IO ()',
        'readActive = do',
        '  _ <- openFile "active.txt" ReadMode',
        '  pure ()',
        '',
      ].join('\n'),
    ],
    [
      'archives/Legacy.hs',
      [
        'module Legacy where',
        '',
        'legacyValue :: Int',
        'legacyValue = 42',
        '',
      ].join('\n'),
    ],
    [
      'src/api/generated/types.ts',
      [
        'export class GeneratedClient {',
        '  readonly value = 42;',
        '}',
        '',
      ].join('\n'),
    ],
  ]);

  try {
    const findings = await collectFormalFindings(repoDir);
    const relativeFindings = findings.map((finding) => ({
      file: path.relative(repoDir, finding.file),
      rule: finding.rule,
    }));

    assert.deepEqual(
      relativeFindings,
      [
        { file: 'src/Active.hs', rule: 'implicit-module-exports' },
        { file: 'src/Active.hs', rule: 'unbracketed-resource' },
      ],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('formal audit accepts multiline Haskell export lists and with-pattern resources', async () => {
  const { tempRoot, repoDir } = await initTrackedRepo([
    [
      'src/Safe.hs',
      [
        'module Safe',
        '  ( readSafely',
        '  ) where',
        '',
        'import System.IO (IOMode(ReadMode), withFile)',
        '',
        'readSafely :: FilePath -> IO ()',
        'readSafely filePath = withFile filePath ReadMode (\\_ -> pure ())',
        '',
      ].join('\n'),
    ],
  ]);

  try {
    const findings = await collectFormalFindings(repoDir);
    assert.equal(findings.length, 0);
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('formal severity threshold helpers separate blocking findings from advisory debt', () => {
  const findings = [
    { severity: 'info', file: '/repo/A.hs', line: 1, rule: 'advisory', message: 'Advisory only' },
    { severity: 'warning', file: '/repo/B.hs', line: 2, rule: 'warning-rule', message: 'Warning only' },
    { severity: 'error', file: '/repo/C.hs', line: 3, rule: 'error-rule', message: 'Blocks the gate' },
    { severity: 'critical', file: '/repo/D.hs', line: 4, rule: 'critical-rule', message: 'Blocks the gate' },
  ];

  assert.equal(findingMeetsSeverityThreshold(findings[1], 'error'), false);
  assert.equal(findingMeetsSeverityThreshold(findings[2], 'error'), true);
  assert.deepEqual(
    collectFailingFormalFindings(findings, 'error').map((finding) => finding.rule),
    ['error-rule', 'critical-rule'],
  );
  assert.deepEqual(collectFailingFormalFindings(findings, 'none'), []);

  assert.deepEqual(summarizeFormalFindings(findings), {
    total: 4,
    critical: 1,
    errors: 1,
    warnings: 1,
    info: 1,
    totalImportance: 0,
  });
  assert.equal(formatFormalFinding(findings[2], '/repo'), '[error] C.hs:3 error-rule - Blocks the gate');
});

test('formal audit CLI exposes a nonblocking default and an explicit stricter gate', async () => {
  const { tempRoot, repoDir } = await initTrackedRepo([
    [
      'src/Active.hs',
      [
        'module Active where',
        '',
        'active :: Int',
        'active = 1',
        '',
      ].join('\n'),
    ],
  ]);

  try {
    const pass = await execFileAsync('node', [auditScriptPath, '--json', '--fail-on', 'error'], {
      cwd: repoDir,
    });
    const report = JSON.parse(pass.stdout);
    assert.equal(report.ok, true);
    assert.equal(report.summary.warnings, 1);
    assert.deepEqual(report.failingFindings, []);

    await assert.rejects(
      () => execFileAsync('node', [auditScriptPath, '--fail-on', 'warning'], { cwd: repoDir }),
      /Command failed/,
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});
