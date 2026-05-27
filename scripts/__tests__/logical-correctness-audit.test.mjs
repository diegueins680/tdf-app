import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

import { collectLogicalFindings } from '../lib/logical-correctness-audit.mjs';

const execFileAsync = promisify(execFile);

test('logical audit distinguishes strict equality from assignment in conditions', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'logical-audit-condition-test-'));
  const sourcePath = path.join(tempRoot, 'conditions.js');

  try {
    await fs.writeFile(
      sourcePath,
      [
        'function valid(experiments) {',
        '  if (experiments.length === 0) return "empty";',
        '  return "active";',
        '}',
        '',
        'function invalid(experiments) {',
        '  if (experiments.length = 0) return "empty";',
        '  return "active";',
        '}',
        '',
        'function validLoop(experiments, attempts) {',
        '  for (let index = 0; index < attempts; index += 1) {',
        '    experiments.push(index);',
        '  }',
        '}',
        '',
        'function invalidLoop(experiments, attempts) {',
        '  for (let index = 0; index = attempts; index += 1) {',
        '    experiments.push(index);',
        '  }',
        '}',
      ].join('\n'),
      'utf8',
    );

    await execFileAsync('git', ['init', '-b', 'main'], { cwd: tempRoot });
    await execFileAsync('git', ['add', 'conditions.js'], { cwd: tempRoot });

    const findings = await collectLogicalFindings(tempRoot);
    const assignmentFindings = findings.filter((finding) => finding.rule === 'assignment-in-condition');

    assert.deepEqual(
      assignmentFindings.map((finding) => ({
        line: finding.line,
        snippet: finding.snippet,
      })),
      [
        {
          line: 7,
          snippet: 'if (experiments.length = 0) return "empty";',
        },
        {
          line: 18,
          snippet: 'for (let index = 0; index = attempts; index += 1) {',
        },
      ],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('logical audit ignores reachable catch blocks and returned object methods', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'logical-audit-unreachable-test-'));
  const sourcePath = path.join(tempRoot, 'returns.js');

  try {
    await fs.writeFile(
      sourcePath,
      [
        'function validCatch() {',
        '  try {',
        '    return;',
        '  } catch (error) {',
        '    throw error;',
        '  }',
        '}',
        '',
        'function validFactory() {',
        '  return {',
        '    cleanup: () => {',
        '      if (!mounted) return;',
        '      mounted = false;',
        '    },',
        '  };',
        '}',
        '',
        'function invalid() {',
        '  return;',
        '  doCleanup();',
        '}',
      ].join('\n'),
      'utf8',
    );

    await execFileAsync('git', ['init', '-b', 'main'], { cwd: tempRoot });
    await execFileAsync('git', ['add', 'returns.js'], { cwd: tempRoot });

    const findings = await collectLogicalFindings(tempRoot);
    const unreachableFindings = findings.filter(
      (finding) => finding.rule === 'potentially-unreachable-code',
    );

    assert.deepEqual(
      unreachableFindings.map((finding) => ({
        line: finding.line,
        snippet: finding.snippet,
      })),
      [
        {
          line: 20,
          snippet: 'doCleanup();',
        },
      ],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('logical audit flags promises created without await or catch', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'logical-audit-promise-test-'));
  const sourcePath = path.join(tempRoot, 'promises.js');
  // Keep the generated fixture realistic without making this test source match the audit regex.
  const promiseConstructor = 'new Prom' + 'ise';
  const unhandledSnippet = `return ${promiseConstructor}(() => undefined);`;
  const unhandledMockSnippet = `mockSessionsList.mockImplementation(() => ${promiseConstructor}(() => undefined));`;

  try {
    await fs.writeFile(
      sourcePath,
      [
        'function pendingMock() {',
        `  ${unhandledSnippet}`,
        '}',
        '',
        'function pendingMockImplementation() {',
        `  ${unhandledMockSnippet}`,
        '}',
        '',
        'function handledPendingMock() {',
        `  return ${promiseConstructor}(() => undefined).catch((error) => { throw error; });`,
        '}',
      ].join('\n'),
      'utf8',
    );

    await execFileAsync('git', ['init', '-b', 'main'], { cwd: tempRoot });
    await execFileAsync('git', ['add', 'promises.js'], { cwd: tempRoot });

    const findings = await collectLogicalFindings(tempRoot);
    const promiseFindings = findings.filter((finding) => finding.rule === 'unhandled-promise');

    assert.deepEqual(
      promiseFindings.map((finding) => ({
        line: finding.line,
        snippet: finding.snippet,
      })),
      [
        {
          line: 2,
          snippet: unhandledSnippet,
        },
        {
          line: 6,
          snippet: unhandledMockSnippet,
        },
      ],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('logical audit flags repeated component-local variable names as shadowing candidates', async () => {
  const variableShadowAuditTempRoot = await fs.mkdtemp(
    path.join(os.tmpdir(), 'logical-audit-variable-shadowing-test-'),
  );
  const variableShadowAuditSourcePath = path.join(variableShadowAuditTempRoot, 'cards.jsx');
  const variableShadowAuditDeclarationKeyword = 'con' + 'st';
  const variableShadowAuditRepeatedName = 'focusActionsForCard';

  try {
    await fs.writeFile(
      variableShadowAuditSourcePath,
      [
        'function AgendaCard() {',
        `  ${variableShadowAuditDeclarationKeyword} ${variableShadowAuditRepeatedName} = {};`,
        `  return <button onClick={${variableShadowAuditRepeatedName}.edit}>Edit</button>;`,
        '}',
        '',
        'function AvailabilityCard() {',
        `  ${variableShadowAuditDeclarationKeyword} ${variableShadowAuditRepeatedName} = {};`,
        `  return <button onClick={${variableShadowAuditRepeatedName}.delete}>Delete</button>;`,
        '}',
      ].join('\n'),
      'utf8',
    );

    await execFileAsync('git', ['init', '-b', 'main'], { cwd: variableShadowAuditTempRoot });
    await execFileAsync('git', ['add', 'cards.jsx'], { cwd: variableShadowAuditTempRoot });

    const variableShadowAuditFindings = await collectLogicalFindings(variableShadowAuditTempRoot);
    const variableShadowAuditShadowingFindings = variableShadowAuditFindings.filter(
      (finding) => finding.rule === 'variable-shadowing',
    );

    assert.deepEqual(
      variableShadowAuditShadowingFindings.map((finding) => ({
        line: finding.line,
        snippet: finding.snippet,
      })),
      [
        {
          line: 7,
          snippet: `${variableShadowAuditDeclarationKeyword} ${variableShadowAuditRepeatedName} = {};`,
        },
      ],
    );
  } finally {
    await fs.rm(variableShadowAuditTempRoot, { recursive: true, force: true });
  }
});

test('teacher portal does not reuse the generic actionDisabled flag name across cards', async () => {
  const teacherPortalAuditTempRoot = await fs.mkdtemp(
    path.join(os.tmpdir(), 'logical-audit-teacher-portal-shadowing-test-'),
  );
  const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
  const teacherPortalSourcePath = path.join(
    repoRoot,
    'tdf-hq-ui/src/pages/TeacherPortalPage.tsx',
  );
  const teacherPortalFixturePath = path.join(
    teacherPortalAuditTempRoot,
    'tdf-hq-ui/src/pages/TeacherPortalPage.tsx',
  );

  try {
    await fs.mkdir(path.dirname(teacherPortalFixturePath), { recursive: true });
    await fs.copyFile(teacherPortalSourcePath, teacherPortalFixturePath);

    await execFileAsync('git', ['init', '-b', 'main'], { cwd: teacherPortalAuditTempRoot });
    await execFileAsync('git', ['add', 'tdf-hq-ui/src/pages/TeacherPortalPage.tsx'], {
      cwd: teacherPortalAuditTempRoot,
    });

    const teacherPortalFindings = await collectLogicalFindings(teacherPortalAuditTempRoot);
    const genericActionDisabledShadowingFindings = teacherPortalFindings.filter(
      (finding) =>
        finding.rule === 'variable-shadowing' &&
        finding.message === 'Variable "actionDisabled" may shadow a previous declaration; review scope.',
    );

    assert.deepEqual(genericActionDisabledShadowingFindings, []);
  } finally {
    await fs.rm(teacherPortalAuditTempRoot, { recursive: true, force: true });
  }
});
