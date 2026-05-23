import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
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
