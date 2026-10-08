import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { copyFileSync, chmodSync, existsSync, mkdtempSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';
import test from 'node:test';
const root = dirname(dirname(dirname(fileURLToPath(import.meta.url))));
function fixture() {
  const dir = mkdtempSync(join(tmpdir(), 'tdf-upload-test-'));
  const bin = join(dir, 'bin'); mkdirSync(bin);
  copyFileSync(join(root, 'test/fixtures/music-worker/upload-curl.mjs'), join(bin, 'curl'));
  chmodSync(join(bin, 'curl'), 0o755);
  const source = join(dir, 'source'); writeFileSync(source, Buffer.alloc(5 * 1024 * 1024 + 65536, 37));
  const trace = join(dir, 'trace');
  const env = { PATH: `${bin}:${process.env.PATH}`, LANG: 'C', LC_ALL: 'C', TMPDIR: dir,
    MUSIC_S3_ENDPOINT: 'https://music-upload.test.invalid', MUSIC_S3_REGION: 'test',
    MUSIC_S3_ACCESS_KEY_ID: 'synthetic-access', MUSIC_S3_SECRET_ACCESS_KEY: 'synthetic-secret',
    MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES: '5242880', MUSIC_WORKER_MULTIPART_PART_BYTES: '5242880',
    MUSIC_TEST_TRACE: trace, MUSIC_TEST_SOURCE: source };
  const args = [join(root, 'scripts/music-s3-upload.pl'), source, 'music-test', 'opaque/key', 'audio/wav'];
  return { dir, trace, env, args, actions: () => readFileSync(trace, 'utf8').trim().split('\n'),
    cleanup: () => rmSync(dir, { recursive: true, force: true }) };
}
for (const fault of ['none', 'part_failure', 'embedded_error', 'duplicate_result', 'unsafe_xml', 'duplicate_etag', 'mutation']) {
  test(`worker multipart ${fault}: integrity, strict responses and cleanup`, () => {
    const f = fixture();
    try {
      const result = spawnSync('perl', f.args, { env: { ...f.env, MUSIC_TEST_FAULT: fault },
        encoding: 'utf8', timeout: 60000 });
      assert.ifError(result.error);
      assert.equal(result.status, fault === 'none' ? 0 : 1, result.stderr);
      if (fault === 'none') {
        assert.equal(JSON.parse(result.stdout).parts, 2);
        assert.deepEqual(f.actions(), ['create', 'part1', 'part2', 'complete']);
      } else {
        assert.equal(f.actions().at(-1), 'abort');
        assert.equal(result.stdout, '');
        if (['mutation', 'part_failure', 'duplicate_etag'].includes(fault)) assert.ok(!f.actions().includes('complete'));
      }
      assert.doesNotMatch(result.stderr, /synthetic-secret|synthetic-access|root:/);
      assert.ok(!readdirSync(f.dir).some((name) => name.startsWith('tdf-music-upload-')));
    } finally { f.cleanup(); }
  });
}
test('TERM during a part stops the transfer and aborts its exact upload', async () => {
  const f = fixture(); let child;
  try {
    child = spawn('perl', f.args, { env: { ...f.env, MUSIC_TEST_FAULT: 'cancel' }, stdio: ['ignore', 'pipe', 'pipe'] });
    let stderr = ''; child.stderr.on('data', (data) => { stderr += data; });
    const done = new Promise((resolve, reject) => { child.on('error', reject); child.on('close', resolve); });
    const deadline = Date.now() + 30000;
    while (!existsSync(f.trace) || !f.actions().includes('part2')) {
      assert.ok(Date.now() < deadline, stderr); await delay(100);
    }
    child.kill('SIGTERM');
    assert.equal(await done, 143, stderr);
    assert.deepEqual(f.actions(), ['create', 'part1', 'part2', 'abort']);
    assert.ok(!readdirSync(f.dir).some((name) => name.startsWith('tdf-music-upload-')));
  } finally { if (child?.exitCode === null) child.kill('SIGKILL'); f.cleanup(); }
});
test('invalid part limits fail before any remote operation', () => {
  const f = fixture();
  try {
    for (const value of ['0', '5242879', '268435457', '5e6', '-1', '99999999999999999999']) {
      const result = spawnSync('perl', f.args, { env: { ...f.env, MUSIC_WORKER_MULTIPART_PART_BYTES: value }, encoding: 'utf8' });
      assert.notEqual(result.status, 0); assert.ok(!existsSync(f.trace));
    }
  } finally { f.cleanup(); }
});
