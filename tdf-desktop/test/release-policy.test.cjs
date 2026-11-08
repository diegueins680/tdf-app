'use strict';
const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { spawnSync } = require('node:child_process');

// Windows checkouts carry CRLF endings; the gate is extracted from LF-normalized text.
const workflow = fs.readFileSync(path.join(__dirname, '../..', '.github/workflows/desktop.yml'), 'utf8').replace(/\r\n/g, '\n');
const gate = workflow.split('      - name: Select release assets and refuse unsigned test packages\n')[1]
  .split('      - name: Create the versioned release')[0]
  .split('        run: |\n')[1].split('\n').map((line) => line.replace(/^          /, '')).join('\n');

function checkFeed({ notarizationConfigured, unsigned = false }) {
  const cwd = fs.mkdtempSync(path.join(os.tmpdir(), 'tdf-feed-policy-'));
  try {
    fs.mkdirSync(path.join(cwd, 'release'));
    fs.writeFileSync(path.join(cwd, 'release', `TDF-Records-0.1.0-mac-arm64${unsigned ? '-UNSIGNED-TEST' : ''}.zip`), 'fixture');
    return spawnSync('bash', ['-c', gate], {
      // The full environment keeps bash usable on Windows runners (SystemRoot, TEMP); only the gate input is overridden.
      cwd, encoding: 'utf8', env: { ...process.env, MAC_NOTARIZATION_CONFIGURED: notarizationConfigured },
    });
  } finally {
    fs.rmSync(cwd, { recursive: true, force: true });
  }
}

test('a certificate-named package cannot enter the feed without notarization configuration', () => {
  const result = checkFeed({ notarizationConfigured: 'false' });
  assert.equal(result.error, undefined);
  assert.notEqual(result.status, 0);
});

test('signed packages with the complete notarization configuration pass this configuration gate', () => {
  assert.equal(checkFeed({ notarizationConfigured: 'true' }).status, 0);
});

test('unsigned packages remain rejected even when notarization is configured', () => {
  assert.notEqual(checkFeed({ notarizationConfigured: 'true', unsigned: true }).status, 0);
});

test('unset and nonliteral notarization configuration fail closed', () => {
  for (const value of ['', 'TRUE', '1']) {
    assert.notEqual(checkFeed({ notarizationConfigured: value }).status, 0);
  }
});

test('the configuration gate requires the certificate and all Apple notarization credentials', () => {
  const expected = "MAC_NOTARIZATION_CONFIGURED: ${{ secrets.DESKTOP_MAC_CSC_LINK != '' && secrets.DESKTOP_APPLE_ID != '' && secrets.DESKTOP_APPLE_APP_SPECIFIC_PASSWORD != '' && secrets.DESKTOP_APPLE_TEAM_ID != '' }}";
  assert.ok(workflow.includes(expected));
  assert.ok(workflow.includes('    needs: build'));
});
