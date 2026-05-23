const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const test = require('node:test');

const { loadExperimentData } = require('./analyze-experiments');

test('loadExperimentData returns null for malformed metrics JSON', (t) => {
  const metricsDir = fs.mkdtempSync(path.join(os.tmpdir(), 'analyze-experiments-'));
  t.after(() => fs.rmSync(metricsDir, { recursive: true, force: true }));

  fs.writeFileSync(path.join(metricsDir, 'IDEA-999.json'), '{ malformed json', 'utf8');

  assert.equal(loadExperimentData('IDEA-999', metricsDir), null);
});
