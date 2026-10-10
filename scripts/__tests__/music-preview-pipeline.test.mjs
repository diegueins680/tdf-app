import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';

const run = (program, args, extra = {}) => spawnSync(program, args, {
  encoding: 'utf8', timeout: 120000, ...extra,
});
const success = (program, args, extra) => {
  const result = run(program, args, extra);
  assert.equal(result.error, undefined);
  assert.equal(result.status, 0, result.stderr?.toString());
  return result.stdout;
};
const spec = (source, start, duration) => run('jq', ['-nc', '--argjson', 'sourceDurationMs', String(source),
  '--argjson', 'start', String(start), '--argjson', 'duration', String(duration), '-f', 'scripts/music-preview-spec.jq']);

test('preview millisecond contract: defaults, fractions, limits and explicit errors', () => {
  for (const [source, start, length, expected] of [
    [2000, null, null, { startMs: 0, durationMs: 2000, selection: 'auto' }],
    [100000, null, null, { startMs: 30000, durationMs: 30000, selection: 'auto' }],
    [4000, 2500, 1500, { startMs: 2500, durationMs: 1500, selection: 'explicit' }],
    [4000, null, 1250, { startMs: 0, durationMs: 1250, selection: 'explicit' }],
  ]) {
    const result = spec(source, start, length);
    assert.equal(result.status, 0, result.stderr);
    assert.deepEqual(JSON.parse(result.stdout), expected);
  }
  for (const [start, length] of [[-1, 50], [1.5, 50], [0, 0], [0, 1.5], [0, null], [4000, 1], [3000, 1001]]) {
    assert.notEqual(spec(4000, start, length).status, 0);
  }
});

test('real audio preview selects the requested tone and duration; retries preserve bytes', () => {
  const dir = mkdtempSync(join(tmpdir(), 'tdf-preview-pipeline-'));
  try {
    const master = join(dir, 'master.wav'), output = join(dir, 'audio-v2');
    success('ffmpeg', ['-nostdin', '-v', 'error', '-f', 'lavfi', '-i',
      "aevalsrc='0.1*sin(2*PI*if(lt(t,2),440,880)*t)':s=48000:d=4", '-c:a', 'pcm_s24le', master]);
    const sha = (path) => createHash('sha256').update(readFileSync(path)).digest('hex');
    const original = sha(master);
    success('sh', ['scripts/process-music-release-audio.sh', master, output, '2500', '1250']);
    const manifest = JSON.parse(readFileSync(join(output, 'manifest.json')));
    assert.deepEqual(manifest.preview, { startMs: 2500, durationMs: 1250, selection: 'explicit' });
    assert.equal(manifest.pipelineVersion, 'audio-v2');
    const preview = join(output, 'preview.m4a'), previewHash = sha(preview);
    const duration = Number(success('ffprobe', ['-v', 'error', '-show_entries', 'format=duration', '-of', 'default=nw=1:nk=1', preview]));
    assert.ok(Math.abs(duration - 1.25) < 0.04, `Actual duration ${duration}`);
    const pcm = success('ffmpeg', ['-v', 'error', '-i', preview, '-ss', '0.2', '-t', '0.5', '-ac', '1', '-ar', '48000', '-f', 'f32le', 'pipe:1'], { encoding: null });
    const power = (frequency) => {
      let sine = 0, cosine = 0;
      for (let n = 0; n < pcm.length / 4; n += 1) {
        const sample = pcm.readFloatLE(n * 4), angle = 2 * Math.PI * frequency * n / 48000;
        sine += sample * Math.sin(angle); cosine += sample * Math.cos(angle);
      }
      return sine * sine + cosine * cosine;
    };
    assert.ok(power(880) > power(440) * 100, 'Preview must select the second tone, not the first');
    success('sh', ['scripts/process-music-release-audio.sh', master, output, '2500', '1250']);
    assert.equal(sha(preview), previewHash);
    assert.notEqual(run('sh', ['scripts/process-music-release-audio.sh', master, output, '0', '1250']).status, 0);
    assert.equal(sha(preview), previewHash);
    assert.equal(sha(master), original);
    assert.notEqual(run('sh', ['scripts/process-music-release-audio.sh', master, join(dir, 'invalid'), '3500', '1000']).status, 0);
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
