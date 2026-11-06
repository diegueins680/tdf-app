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
  countFormalFindingsByRule,
  findingMeetsSeverityThreshold,
  formalRuleCategory,
  formatFormalFinding,
  maskHaskellCommentsAndStrings,
  selectFormalFindingsInScope,
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

test('Haskell masking blanks comments, strings and char literals but keeps code and lines', () => {
  const source = [
    'x = 1 -- open with lock',
    '{- openFile {- nested -} connect -}',
    'y = "connect with \\"openFile\\"" ++ [\'"\']',
    "z' = openFile p",
    'q = a --> b',
  ].join('\n');
  const masked = maskHaskellCommentsAndStrings(source);
  assert.equal(masked.split('\n').length, 5);
  assert.equal(masked.length, source.length);
  assert.doesNotMatch(masked.split('\n').slice(0, 3).join('\n'), /openFile|connect|with|lock/);
  assert.match(masked.split('\n')[3], /openFile p/);
  assert.match(masked.split('\n')[4], /a --> b/);
});

test('resource rule ignores prose, SQL and messages and flags only unreleased handles (F-11)', async () => {
  const { tempRoot, repoDir } = await initTrackedRepo([
    [
      'src/Prose.hs',
      [
        'module Prose (prose, leak, released) where',
        '',
        'import System.IO (IOMode(ReadMode), openFile, withFile, hClose)',
        'import Control.Exception (finally)',
        '',
        '-- | Locks the row; acquire the lock with care.',
        'prose :: String',
        'prose = "WITH requested AS (SELECT 1) -- connect with lock"',
        '',
        'leak :: IO ()',
        'leak = do',
        '  _ <- openFile "leak.txt" ReadMode',
        '  pure ()',
        '',
        'released :: IO ()',
        'released = do',
        '  h <- openFile "ok.txt" ReadMode',
        '  pure () `finally` hClose h',
        '',
      ].join('\n'),
    ],
  ]);

  try {
    const findings = await collectFormalFindings(repoDir);
    assert.deepEqual(
      findings.map((finding) => [finding.rule, finding.line]),
      [['unbracketed-resource', 12]],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('rule categories separate semantic findings from style debt', () => {
  const findings = [
    { rule: 'magic-number', severity: 'info' },
    { rule: 'unbracketed-resource', severity: 'warning' },
    { rule: 'deep-nesting', severity: 'warning' },
    { rule: 'some-future-rule', severity: 'warning' },
  ];
  assert.equal(formalRuleCategory('magic-number'), 'style');
  assert.equal(formalRuleCategory('unbracketed-resource'), 'semantic');
  // An uncategorised rule is reported, never silently hidden.
  assert.equal(formalRuleCategory('some-future-rule'), 'semantic');
  assert.deepEqual(
    selectFormalFindingsInScope(findings, 'semantic').map((finding) => finding.rule),
    ['unbracketed-resource', 'some-future-rule'],
  );
  assert.deepEqual(
    selectFormalFindingsInScope(findings, 'style').map((finding) => finding.rule),
    ['magic-number', 'deep-nesting'],
  );
  assert.equal(selectFormalFindingsInScope(findings, 'all').length, 4);
  assert.deepEqual(countFormalFindingsByRule(findings), {
    'deep-nesting': 1,
    'magic-number': 1,
    'some-future-rule': 1,
    'unbracketed-resource': 1,
  });
});

test('formal audit CLI reports semantic findings by default and style debt on request', async () => {
  const { tempRoot, repoDir } = await initTrackedRepo([
    [
      'src/Active.hs',
      ['module Active where', '', 'active :: Int', 'active = 4242', ''].join('\n'),
    ],
    [
      'src/numbers.ts',
      ['export const timeoutMs = setTimeout(() => undefined, 4242);', ''].join('\n'),
    ],
  ]);

  try {
    const semantic = JSON.parse(
      (await execFileAsync('node', [auditScriptPath, '--json'], { cwd: repoDir })).stdout,
    );
    assert.equal(semantic.scope, 'semantic');
    assert.deepEqual(semantic.findings.map((finding) => finding.rule), ['implicit-module-exports']);
    assert.equal(semantic.outOfScope, 1);
    assert.deepEqual(semantic.byRule, { 'implicit-module-exports': 1, 'magic-number': 1 });

    const style = JSON.parse(
      (await execFileAsync('node', [auditScriptPath, '--json', '--scope', 'style'], { cwd: repoDir })).stdout,
    );
    assert.deepEqual(style.findings.map((finding) => finding.rule), ['magic-number']);

    const all = JSON.parse(
      (await execFileAsync('node', [auditScriptPath, '--json', '--scope', 'all'], { cwd: repoDir })).stdout,
    );
    assert.equal(all.summary.total, 2);

    await assert.rejects(
      () => execFileAsync('node', [auditScriptPath, '--scope', 'bogus'], { cwd: repoDir }),
      /Unsupported --scope value/,
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});
