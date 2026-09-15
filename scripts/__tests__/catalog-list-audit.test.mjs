import assert from 'node:assert/strict';
import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import { promisify } from 'node:util';

const execFileAsync = promisify(execFile);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const auditScriptPath = path.resolve(testDir, '..', 'catalog-list-audit.mjs');
const decisionBuilderPath = path.resolve(testDir, '..', 'build-catalog-decisions.mjs');

async function writeFile(repoDir, filePath, content) {
  const fullPath = path.join(repoDir, filePath);
  await fs.mkdir(path.dirname(fullPath), { recursive: true });
  await fs.writeFile(fullPath, content, 'utf8');
}

test('catalog audit excludes ignored local source files from candidate fingerprints', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-list-audit-test-'));
  const repoDir = path.join(tempRoot, 'repo');
  const reportPath = path.join(tempRoot, 'report.json');

  try {
    await fs.mkdir(repoDir);
    await execFileAsync('git', ['init', '-b', 'main'], { cwd: repoDir });
    await writeFile(repoDir, '.gitignore', '*.env\n');
    await writeFile(
      repoDir,
      'scripts/tracked.mjs',
      "export const STATUS_OPTIONS = ['active', 'inactive'];\n",
    );
    await writeFile(repoDir, 'scripts/local.env', 'SUPPORTED_LOCALES=en,es\n');
    await execFileAsync('git', ['add', '.gitignore', 'scripts/tracked.mjs'], { cwd: repoDir });

    await execFileAsync(
      process.execPath,
      [auditScriptPath, '--root', repoDir, '--output', reportPath],
      { cwd: repoDir },
    );

    const report = JSON.parse(await fs.readFile(reportPath, 'utf8'));
    assert.deepEqual(
      report.candidates.map(({ file, name }) => ({ file, name })),
      [{ file: 'scripts/tracked.mjs', name: 'STATUS_OPTIONS' }],
    );
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});

test('decision reconciliation preserves reviewed entries, removes stale IDs, and records rule evidence', async () => {
  const tempRoot = await fs.mkdtemp(path.join(os.tmpdir(), 'catalog-decision-builder-test-'));
  const inventoryPath = path.join(tempRoot, 'inventory.json');
  const existingPath = path.join(tempRoot, 'existing.json');
  const outputPath = path.join(tempRoot, 'decisions.json');
  const preserved = {
    id: 'existing-id',
    classification: 'genuine-technical-constant',
    disposition: 'retain-in-code',
    specializedModel: 'technical_constant_allowlist',
    priority: 'P3-retain',
    risk: 'Existing reviewed risk remains unchanged.',
    justification: 'Existing reviewed justification remains unchanged.',
    reviewed: true,
  };
  const candidate = (overrides) => ({
    id: 'new-id',
    file: 'scripts/artist-enrichment.mjs',
    line: 10,
    kind: 'object-registry',
    name: 'delayOptions',
    values: ['baseMs', 'jitterRatio'],
    valueCount: 2,
    sourceKind: 'production',
    surface: 'automation',
    domain: 'cross-cutting-or-technical',
    consumerCount: 1,
    exactDuplicateIds: [],
    similarCandidateIds: [],
    ...overrides,
  });

  try {
    await fs.writeFile(inventoryPath, JSON.stringify({
      candidates: [
        candidate({ id: preserved.id, name: 'preserved' }),
        candidate({}),
        candidate({
          id: 'builder-options-id',
          file: 'scripts/build-catalog-decisions.mjs',
          name: 'options',
          values: ['input', 'output', 'existing', 'reviewBatch'],
        }),
        candidate({
          id: 'onboarding-id',
          file: 'tdf-hq/docs/openapi/api.yaml',
          kind: 'openapi-enum',
          name: 'firstValue',
          values: ['artist_followed', 'access_requested'],
        }),
      ],
    }));
    await fs.writeFile(existingPath, JSON.stringify({
      schemaVersion: 1,
      decisions: [
        preserved,
        { ...preserved, id: 'stale-id' },
        {
          ...preserved,
          id: 'builder-options-id',
          classification: 'dynamic-business-catalog',
          disposition: 'migrate-and-remove-authority-from-code',
          reviewMethod: 'deterministic-policy-v2',
        },
      ],
    }));

    await execFileAsync(process.execPath, [
      decisionBuilderPath,
      '--input', inventoryPath,
      '--output', outputPath,
      '--existing', existingPath,
      '--review-batch', 'unit-test-review',
    ]);

    const report = JSON.parse(await fs.readFile(outputPath, 'utf8'));
    assert.deepEqual(report.decisions[0], preserved);
    assert.equal(report.decisions[1].classification, 'genuine-technical-constant');
    assert.equal(report.decisions[1].reviewBatch, 'unit-test-review');
    assert.equal(report.decisions[1].evidence.file, 'scripts/artist-enrichment.mjs');
    assert.equal(report.decisions[2].classification, 'genuine-technical-constant');
    assert.equal(report.decisions[2].disposition, 'retain-in-code');
    assert.equal(report.decisions[2].reviewMethod, 'deterministic-policy-v3');
    assert.match(report.decisions[2].justification, /CLI execution mechanics/);
    assert.equal(report.decisions[3].classification, 'dynamic-business-catalog');
    assert.equal(report.decisions[3].specializedModel, 'onboarding_intent, onboarding_first_value, user_onboarding_progress');
    assert.deepEqual(report.reconciliation, {
      preservedDecisions: 1,
      reviewedByPolicy: 3,
      removedStaleDecisions: 1,
      reviewBatch: 'unit-test-review',
    });
  } finally {
    await fs.rm(tempRoot, { recursive: true, force: true });
  }
});
