import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, join } from 'node:path';
import test from 'node:test';

import {
  musicDirectoryVisualArtifactPaths,
  writeMusicDirectoryVisualArtifacts,
} from '../music-directory-visual-artifacts.mjs';

test('scoped captures preserve aggregate visual evidence', (t) => {
  const outputDir = mkdtempSync(join(tmpdir(), 'tdf-directory-visual-artifacts-'));
  t.after(() => rmSync(outputDir, { recursive: true, force: true }));

  const aggregateAccessibility = [{ name: 'aggregate', violations: [], passes: 189 }];
  const aggregateBrowserErrors = [];
  const aggregatePaths = writeMusicDirectoryVisualArtifacts({
    outputDir,
    captureScope: 'all',
    accessibility: aggregateAccessibility,
    browserErrors: aggregateBrowserErrors,
  });
  const aggregateAccessibilityBefore = readFileSync(aggregatePaths.accessibility, 'utf8');
  const aggregateBrowserErrorsBefore = readFileSync(aggregatePaths.browserErrors, 'utf8');

  for (const captureScope of ['web-managed', 'mobile-managed']) {
    const scopedAccessibility = [{ name: captureScope, violations: [], passes: 1 }];
    const scopedBrowserErrors = [{ surface: captureScope, message: 'synthetic scoped error' }];
    const scopedPaths = writeMusicDirectoryVisualArtifacts({
      outputDir,
      captureScope,
      accessibility: scopedAccessibility,
      browserErrors: scopedBrowserErrors,
    });

    assert.equal(basename(scopedPaths.accessibility), `accessibility-results-${captureScope}.json`);
    assert.equal(basename(scopedPaths.browserErrors), `browser-errors-${captureScope}.json`);
    assert.deepEqual(JSON.parse(readFileSync(scopedPaths.accessibility, 'utf8')), scopedAccessibility);
    assert.deepEqual(JSON.parse(readFileSync(scopedPaths.browserErrors, 'utf8')), scopedBrowserErrors);
    assert.equal(readFileSync(aggregatePaths.accessibility, 'utf8'), aggregateAccessibilityBefore);
    assert.equal(readFileSync(aggregatePaths.browserErrors, 'utf8'), aggregateBrowserErrorsBefore);
  }
});

test('artifact path selection rejects unknown scopes', () => {
  assert.throws(
    () => musicDirectoryVisualArtifactPaths('/tmp/unused', 'unexpected'),
    /Unsupported directory visual scope: unexpected/,
  );
});
