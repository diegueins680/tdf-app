import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';

// Run the actual child/timing helpers without starting the database worker.
const prefix = readFileSync('scripts/run-music-release-worker-once.sh', 'utf8').split('\nfor variable in ')[0];
test('TERM in child wait survives EXIT cleanup and returns 143', async () => {
  const source = readFileSync('scripts/run-music-release-worker-once.sh', 'utf8');
  const finish = source.match(/finish_worker\(\) \{[\s\S]*?\n\}/)?.[0];
  assert.ok(finish);
  const script = `${prefix}
stop_heartbeat() { :; }
mark_failed() { local result=0; :; }
cleanup() { :; }
error_log=/dev/null
job_kind=synthetic
exec 3>&2
${finish}
trap finish_worker EXIT
trap 'exit 143' TERM
run_child bash -c 'echo child-ready; sleep 30'
`;
  const child = spawn('bash', ['-c', script], { env: { PATH: process.env.PATH }, stdio: ['ignore', 'pipe', 'pipe'] });
  let stderr = ''; child.stderr.on('data', (data) => { stderr += data; });
  child.stdout.once('data', () => child.kill('SIGTERM'));
  const timer = setTimeout(() => child.kill('SIGTERM'), 10000);
  try {
    const status = await new Promise((resolve, reject) => { child.on('error', reject); child.on('close', resolve); });
    assert.equal(status, 143, stderr);
  } finally { clearTimeout(timer); }
});
for (const enabled of ['false', 'true']) {
  for (const status of [0, 7]) {
    test(`worker timing ${enabled} preserves child status ${status} and excludes arguments`, () => {
      const result = spawnSync('bash', ['-c', `${prefix}\nrun_child bash -c 'exit ${status}' synthetic-secret-do-not-log`], {
        encoding: 'utf8', timeout: 30000,
        env: { PATH: process.env.PATH, MUSIC_WORKER_DIAGNOSTICS: enabled },
      });
      assert.ifError(result.error); assert.equal(result.status, status);
      assert.equal(result.stdout, ''); assert.doesNotMatch(result.stderr, /synthetic-secret/);
      const events = result.stderr.split('\n').filter((line) => line.startsWith('{')).map((line) => JSON.parse(line));
      if (enabled === 'true') {
        assert.equal(events.length, 2);
        assert.equal(events[0].phase, 'start'); assert.equal(events[1].phase, 'finish');
        assert.equal(events[1].stage, 'external'); assert.equal(events[1].status, status);
        assert.ok(Number.isInteger(events[1].elapsedSeconds));
      } else assert.deepEqual(events, []);
    });
  }
}
test('single-pass derivative metadata retains preview, loudness, quality and immutability fields', () => {
  const source = readFileSync('scripts/run-music-release-worker-once.sh', 'utf8');
  const helper = source.match(/audio_derivative_metadata\(\) \{[\s\S]*?\n\}/)?.[0];
  assert.ok(helper);
  const dir = mkdtempSync(join(tmpdir(), 'tdf-music-metadata-test-'));
  try {
    const file = join(dir, 'manifest.json');
    const preview = { startMs: 2500, durationMs: 1750, selection: 'explicit' };
    const loudness = { input_i: '-24.61', input_tp: '-18.2', target_offset: '0.01' };
    writeFileSync(file, JSON.stringify({ preview, normalization: { measurement: loudness } }));
    for (const bitrate of [0, 96, 160, 256]) {
      const result = spawnSync('bash', ['-c', `${helper}\naudio_derivative_metadata "$1" "$2"`, 'metadata-test', file, String(bitrate)], {
        encoding: 'utf8', timeout: 30000,
      });
      assert.ifError(result.error); assert.equal(result.status, 0, result.stderr);
      assert.deepEqual(JSON.parse(result.stdout), { pipeline: 'audio-v2', preview, bitrate_kbps: bitrate,
        loudness, normalized: true, masterModified: false });
    }
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
