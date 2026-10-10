// Real xmllint/zip/files, pinned local official XSD. Resource bytes are plainly
// synthetic: these cases test packaging/security, NOT audio decoding or rights.
import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { copyFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync,
  symlinkSync, utimesSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

assert(process.argv[2], 'Usage: node scripts/test-music-ddex-package.mjs OFFICIAL_SCHEMA_DIR');
const schema = resolve(process.argv[2]);
const root = dirname(dirname(fileURLToPath(import.meta.url)));
const runtime = mkdtempSync(join(tmpdir(), 'tdf-ddex-package-test-'));
const audioName = '012345678905_T1_SoundRecording.wav';
const coverName = '012345678905_T2_CoverArt.jpg';
const source = readFileSync(join(root, 'tdf-hq/test/fixtures/ddex/ern432-audio-valid.xml'), 'utf8')
  .replace('01-synthetic-track.wav', audioName).replace('cover.jpg', coverName);
const sha = value => createHash('sha256').update(value).digest('hex');
const run = (program, args) => spawnSync(program, args, { encoding: 'utf8', timeout: 30000 });
let passed = 0;
function fixture(name, xml = source) {
  const dir = join(runtime, name); mkdirSync(dir);
  const resources = join(dir, 'resources'); mkdirSync(resources);
  writeFileSync(join(resources, audioName), 'synthetic audio packaging bytes');
  writeFileSync(join(resources, coverName), 'synthetic cover packaging bytes');
  const input = join(dir, 'input.xml'); writeFileSync(input, xml);
  return { dir, resources, input, output: join(dir, 'package.zip') };
}
const args = (f, xsd = schema) => [join(root, 'scripts/build-ddex-ern432-package.sh'),
  xsd, f.input, f.resources, f.output, 'synthetic-author', 'a'.repeat(64)];
function build(f, xsd) {
  const result = run('sh', args(f, xsd));
  assert.equal(result.error, undefined);
  return result;
}
function reject(f, expression, xsd) {
  const result = build(f, xsd);
  assert.notEqual(result.status, 0, result.stdout);
  assert.match(result.stderr, expression);
  assert.equal(existsSync(f.output), false, 'failure must not publish a partial archive');
  passed++; console.log(`PASS reject ${expression}`);
}
try {
  const f = fixture('valid');
  writeFileSync(join(f.resources, 'private-master-do-not-export.wav'), 'private unreferenced bytes');
  assert.equal(build(f).status, 0);
  const entries = run('unzip', ['-Z1', f.output]).stdout.trim().split('\n');
  assert.deepEqual(entries, ['012345678905.xml', 'manifest.json', `resources/${audioName}`, `resources/${coverName}`]);
  const manifest = JSON.parse(run('unzip', ['-p', f.output, 'manifest.json']).stdout);
  assert.equal(manifest.manifestVersion, 3);
  assert.equal(manifest.messageFile, '012345678905.xml');
  assert.equal(manifest.validation.serverLayout, 'not-performed');
  assert.equal(manifest.deliveryPerformed, false);
  assert.equal(manifest.validation.xsd, 'passed');
  for (const file of manifest.files) {
    const value = spawnSync('unzip', ['-p', f.output, file.path]).stdout;
    assert.equal(sha(value), file.sha256); assert.equal(value.length, file.bytes);
  }
  const first = readFileSync(f.output);
  f.output = join(f.dir, 'again.zip');
  utimesSync(join(f.resources, coverName), 1700000000, 1700000000);
  assert.equal(build(f).status, 0);
  assert.deepEqual(readFileSync(f.output), first, 'identical bytes despite input mtime/path changes');
  const duplicate = build(f); assert.notEqual(duplicate.status, 0);
  assert.deepEqual(readFileSync(f.output), first, 'existing package is never overwritten');
  passed++; console.log('PASS XSD, referenced files only, all checksums, reproducible bytes and no overwrite');

  const gridId = 'A12425GABC1234002M'; // Synthetic fixture, not an allocated code.
  const grid = fixture('grid', source.replaceAll('012345678905', gridId)
    .replace(`<ICPN>${gridId}</ICPN>`, `<GRid>${gridId}</GRid>`));
  for (const name of [audioName, coverName]) {
    copyFileSync(join(grid.resources, name), join(grid.resources, name.replace('012345678905', gridId)));
  }
  assert.equal(build(grid).status, 0);
  const gridManifest = JSON.parse(run('unzip', ['-p', grid.output, 'manifest.json']).stdout);
  assert.equal(gridManifest.messageFile, `${gridId}.xml`);
  assert.equal(gridManifest.files.length, 3);
  assert.ok(gridManifest.files.every(file => file.path.includes(gridId)));
  passed++; console.log('PASS provided GRid names match the XML without internal-ID fallback');

  reject(fixture('legacy-name', source.replace(coverName, 'cover.jpg')), /resources.fileName/);
  reject(fixture('wrong-anchor', source.replace('<TechnicalResourceDetailsReference>T2<', '<TechnicalResourceDetailsReference>T3<')), /resources.fileName/);
  reject(fixture('wrong-release', source.replace(`<ICPN>012345678905</ICPN>`, '<ICPN>012345678906</ICPN>')), /resources.fileName/);
  reject(fixture('unsafe-id', source.replace('<ICPN>012345678905</ICPN>', '<ICPN>../../private</ICPN>')), /releaseIdentifier|validity error/);
  reject(fixture('resource-manifest', source.replace(coverName, 'manifest.json')), /resources.extension/);

  const missing = fixture('missing'); rmSync(join(missing.resources, coverName));
  reject(missing, /missing resource/);
  reject(fixture('traversal', source.replace(`resources/${coverName}`, 'resources/../secret')), /Unsafe DDEX/);
  reject(fixture('encoded-traversal', source.replace(`resources/${coverName}`, 'resources/&#46;&#46;/secret')), /Unsafe DDEX/);
  reject(fixture('remote', source.replace(`resources/${coverName}`, 'https://example.invalid/private')), /local resources/);
  reject(fixture('dtd', source.replace('<ern:NewReleaseMessage', '<!DOCTYPE test [<!ENTITY private SYSTEM "file:///etc/passwd">]><ern:NewReleaseMessage')), /DTD\/entity/);
  const encoded = fixture('utf16');
  writeFileSync(encoded.input, Buffer.from(source.replace('UTF-8', 'UTF-16'), 'utf16le'));
  reject(encoded, /UTF-8 XML declaration/);
  const link = fixture('symlink'); rmSync(join(link.resources, coverName));
  symlinkSync(join(f.resources, coverName), join(link.resources, coverName));
  reject(link, /Symlink/);
  const drift = fixture('schema-drift'), modifiedSchema = join(drift.dir, 'schema'); mkdirSync(modifiedSchema);
  for (const name of ['release-notification.xsd', 'allowed-value-sets.xsd']) copyFileSync(join(schema, name), join(modifiedSchema, name));
  writeFileSync(join(modifiedSchema, 'allowed-value-sets.xsd'), 'untrusted schema');
  reject(drift, /checksum mismatch/, modifiedSchema);

  const race = fixture('race');
  const results = await Promise.all([1, 2].map(() => new Promise((resolve, reject) => {
    const child = spawn('sh', args(race), { stdio: 'ignore' });
    child.once('error', reject); child.once('close', resolve);
  })));
  assert.equal(results.filter(code => code === 0).length, 1);
  assert.equal(run('unzip', ['-t', race.output]).status, 0);
  passed++; console.log('PASS concurrent publication has exactly one complete archive winner');
  console.log(`DDEX package: ${passed} scenarios passed; no remote validation or delivery.`);
} finally { rmSync(runtime, { recursive: true, force: true }); }
