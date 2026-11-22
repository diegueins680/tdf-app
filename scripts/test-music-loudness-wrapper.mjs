// MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md).
// Runs scripts/measure-music-loudness.pl with the real FFmpeg on:
//   - the EBU Tech 3341 (v4.0, November 2023) Table 1 minimum-requirements
//     signals that the standard defines by formula: cases 1-6 (integrated
//     loudness, +/-0.1 LU) and 15-19 (maximum true peak, +0.2/-0.4 dB);
//   - malformed, truncated, silent, missing and never-ending inputs.
// Cases 7-8 (programme material) and 20-23 (filtered, downsampled impulses)
// exist only as EBU downloads and are NOT covered here. Tests, not proofs.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { chmodSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = dirname(dirname(fileURLToPath(import.meta.url)));
const wrapper = join(root, 'scripts/measure-music-loudness.pl');
const work = mkdtempSync(join(tmpdir(), 'tdf-music-loudness-'));
let passed = 0;

function ffmpeg(args) {
  const result = spawnSync('ffmpeg', ['-nostdin', '-v', 'error', '-y', ...args], { encoding: 'utf8', timeout: 120000 });
  assert.equal(result.status, 0, `ffmpeg ${args.join(' ')}: ${result.stderr}`);
}
function measure(file, extra = {}) {
  const started = performance.now();
  const result = spawnSync('perl', [wrapper, file], {
    encoding: 'utf8', timeout: 120000, env: { PATH: process.env.PATH, LANG: 'C', ...extra },
  });
  assert.equal(result.error, undefined, String(result.error));
  assert.equal(result.status, 0, `Every outcome for a named file exits 0: ${result.stderr}`);
  const lines = result.stdout.split('\n').filter(Boolean);
  assert.equal(lines.length, 1, `Exactly one JSON object: ${result.stdout}`);
  const outcome = JSON.parse(lines[0]);
  assert.equal(outcome.tool, 'ffmpeg-ebur128-v1');
  if (outcome.state === 'measured') {
    assert.deepEqual(Object.keys(outcome).filter((key) => key !== 'ffmpegVersion'),
      ['tool', 'state', 'integratedLufsTenths', 'truePeakDbtpTenths']);
    assert.ok(Number.isInteger(outcome.integratedLufsTenths) && Number.isInteger(outcome.truePeakDbtpTenths));
  } else {
    assert.equal(outcome.state, 'failed');
    assert.deepEqual(Object.keys(outcome).filter((key) => key !== 'ffmpegVersion'), ['tool', 'state', 'reason']);
  }
  return { ...outcome, seconds: (performance.now() - started) / 1000 };
}
function check(label, body) {
  body();
  passed += 1;
  console.log(`PASS ${label}`);
}
const amplitude = (dbfs) => String(10 ** (dbfs / 20));
// One segment of the Table 1 tone: 1000 Hz, in phase on every channel.
function tone(name, levels, seconds) {
  const file = join(work, `${name}.wav`);
  const expression = levels.map((level) => `${amplitude(level)}*sin(2*PI*1000*t)`).join('|');
  const layout = levels.length === 5 ? ':c=5.0' : '';
  ffmpeg(['-f', 'lavfi', '-i', `aevalsrc='${expression}':s=48000:d=${seconds}${layout}`, '-c:a', 'pcm_s24le', file]);
  return file;
}
function sequence(name, segments) {
  const parts = segments.map(([level, seconds], index) => tone(`${name}-${index}`, [level, level], seconds));
  const list = join(work, `${name}.txt`);
  writeFileSync(list, parts.map((part) => `file '${part}'\n`).join(''));
  const file = join(work, `${name}.wav`);
  ffmpeg(['-f', 'concat', '-safe', '0', '-i', list, '-c', 'copy', file]);
  return file;
}
// Table 1 cases 15-19: fs/divisor Hz, given amplitude (fraction of full scale)
// and phase, 10 ms fade-in and fade-out. Float samples: case 19 exceeds 1.0.
function peakTone(name, divisor, fraction, degrees) {
  const file = join(work, `${name}.wav`);
  const expression = `${fraction}*sin(2*PI*(48000/${divisor})*t+${degrees}*PI/180)`;
  ffmpeg(['-f', 'lavfi', '-i', `aevalsrc='${expression}|${expression}':s=48000:d=3`,
    '-af', 'afade=t=in:d=0.01,afade=t=out:st=2.99:d=0.01', '-c:a', 'pcm_f32le', file]);
  return file;
}

try {
  const version = spawnSync('ffmpeg', ['-version'], { encoding: 'utf8' }).stdout.split('\n')[0];
  console.log(`FFmpeg under test: ${version}`);

  const integrated = [
    ['1 stereo 1 kHz at -23.0 dBFS, 20 s', () => tone('case1', [-23, -23], 20), -230],
    ['2 stereo 1 kHz at -33.0 dBFS, 20 s', () => tone('case2', [-33, -33], 20), -330],
    ['3 10 s -36.0, 60 s -23.0, 10 s -36.0', () => sequence('case3', [[-36, 10], [-23, 60], [-36, 10]]), -230],
    ['4 10 s -72.0, 10 s -36.0, 60 s -23.0, 10 s -36.0, 10 s -72.0',
      () => sequence('case4', [[-72, 10], [-36, 10], [-23, 60], [-36, 10], [-72, 10]]), -230],
    ['5 20 s -26.0, 20.1 s -20.0, 20 s -26.0', () => sequence('case5', [[-26, 20], [-20, 20.1], [-26, 20]]), -230],
    ['6 5.0 channels: L/R -28.0, C -24.0, Ls/Rs -30.0, 20 s', () => tone('case6', [-28, -28, -24, -30, -30], 20), -230],
  ];
  for (const [label, build, expected] of integrated) {
    check(`EBU Tech 3341 case ${label}: I = ${expected / 10} +/-0.1 LUFS`, () => {
      const outcome = measure(build());
      assert.equal(outcome.state, 'measured', JSON.stringify(outcome));
      assert.ok(Math.abs(outcome.integratedLufsTenths - expected) <= 1, `measured ${outcome.integratedLufsTenths} tenths`);
    });
  }
  const peaks = [
    ['15 fs/4, 0.50 FFS, phase 0.0', 4, 0.5, 0, -60],
    ['16 fs/4, 0.50 FFS, phase 45.0', 4, 0.5, 45, -60],
    ['17 fs/6, 0.50 FFS, phase 60.0', 6, 0.5, 60, -60],
    ['18 fs/8, 0.50 FFS, phase 67.5', 8, 0.5, 67.5, -60],
    ['19 fs/4, 1.41 FFS, phase 45.0', 4, 1.41, 45, 30],
  ];
  for (const [label, divisor, fraction, degrees, expected] of peaks) {
    check(`EBU Tech 3341 case ${label}: max true peak = ${expected / 10} +0.2/-0.4 dBTP`, () => {
      const outcome = measure(peakTone(`case${label.slice(0, 2)}`, divisor, fraction, degrees));
      assert.equal(outcome.state, 'measured', JSON.stringify(outcome));
      assert.ok(outcome.truePeakDbtpTenths <= expected + 2 && outcome.truePeakDbtpTenths >= expected - 4,
        `measured ${outcome.truePeakDbtpTenths} tenths`);
    });
  }

  const good = tone('good', [-20, -20], 3);
  const failure = (label, file, reason, extra) => check(`total: ${label} is a recorded ${reason}`, () => {
    const outcome = measure(file, extra);
    assert.equal(outcome.state, 'failed', JSON.stringify(outcome));
    assert.equal(outcome.reason, reason);
    return outcome;
  });
  const write = (name, bytes) => { const file = join(work, name); writeFileSync(file, bytes); return file; };
  failure('an empty file', write('empty.wav', ''), 'decode_error');
  failure('random bytes', write('random.wav', Buffer.from(Array.from({ length: 5000 }, (_, i) => (i * 131 + 7) % 256))), 'decode_error');
  failure('a text file named .flac', write('text.flac', 'not audio\n'), 'decode_error');
  const flac = join(work, 'good.flac');
  ffmpeg(['-i', good, '-c:a', 'flac', flac]);
  failure('a truncated FLAC', write('truncated.flac', readFileSync(flac).subarray(0, 40000)), 'decode_error');
  failure('a truncated WAV', write('truncated.wav', readFileSync(good).subarray(0, 100000)), 'decode_error');
  failure('a missing file', join(work, 'absent.wav'), 'decode_error');
  failure('a directory', work, 'decode_error');
  failure('a name that looks like an option', join(work, '-version'), 'decode_error');
  const silent = join(work, 'silent.wav');
  ffmpeg(['-f', 'lavfi', '-i', 'anullsrc=r=48000:cl=stereo:d=5', '-c:a', 'pcm_s16le', silent]);
  failure('digital silence', silent, 'silent');
  failure('a missing tool', good, 'tool_unavailable', { MUSIC_LOUDNESS_FFMPEG: join(work, 'no-such-ffmpeg') });
  const fake = (name, body) => { const file = join(work, name); writeFileSync(file, `#!/bin/sh\n${body}\n`); chmodSync(file, 0o755); return file; };
  failure('a tool that prints nothing', good, 'no_summary', { MUSIC_LOUDNESS_FFMPEG: fake('quiet', 'exit 0') });
  failure('a tool that fails', good, 'decode_error', { MUSIC_LOUDNESS_FFMPEG: fake('failing', 'echo broken >&2; exit 3') });
  failure('a summary without numbers', good, 'unparseable', { MUSIC_LOUDNESS_FFMPEG:
    fake('garbled', "printf 'Summary:\\n\\n  Integrated loudness:\\n    I:         nan LUFS\\n\\n  True peak:\\n    Peak:      1e9 dBFS\\n' >&2") });
  failure('a summary missing the peak', good, 'unparseable', { MUSIC_LOUDNESS_FFMPEG:
    fake('partial', "printf 'Summary:\\n\\n  Integrated loudness:\\n    I:         -14.0 LUFS\\n' >&2") });
  failure('an impossible level', good, 'out_of_range', { MUSIC_LOUDNESS_FFMPEG:
    fake('impossible', "printf 'Summary:\\n\\n  Integrated loudness:\\n    I:         -14.0 LUFS\\n\\n  True peak:\\n    Peak:      25.0 dBFS\\n' >&2") });
  check('total: megabytes of tool output do not grow the wrapper or hide the summary', () => {
    const noisy = fake('noisy', "i=0; while [ $i -lt 20000 ]; do echo 'Summary: decoy line of repeated diagnostic output padding padding padding' >&2; i=$((i+1)); done; printf 'Summary:\\n\\n  Integrated loudness:\\n    I:         -14.2 LUFS\\n\\n  True peak:\\n    Peak:      -1.3 dBFS\\n' >&2");
    const outcome = measure(good, { MUSIC_LOUDNESS_FFMPEG: noisy });
    assert.equal(outcome.state, 'measured');
    assert.equal(outcome.integratedLufsTenths, -142);
    assert.equal(outcome.truePeakDbtpTenths, -13);
  });
  for (const [label, body] of [
    ['a tool that never ends', 'exec sleep 300'],
    ['a tool that ignores TERM', 'trap "" TERM; while :; do sleep 1; done'],
    ['a tool whose descendant keeps the output open', 'sleep 20 &\ntrap "" TERM\nwait'],
  ]) {
    check(`total: ${label} ends in timeout within the limit plus the grace`, () => {
      const outcome = measure(good, { MUSIC_LOUDNESS_FFMPEG: fake('stalling', body),
        MUSIC_LOUDNESS_TIMEOUT_SECONDS: '1', MUSIC_LOUDNESS_KILL_GRACE_SECONDS: '1' });
      assert.equal(outcome.state, 'failed');
      assert.equal(outcome.reason, 'timeout');
      assert.ok(outcome.seconds < 4, `took ${outcome.seconds.toFixed(2)} s`);
    });
  }
  check('usage errors exit 2 and print no result', () => {
    for (const [args, extra] of [[[], {}], [[good, good], {}], [[good], { MUSIC_LOUDNESS_TIMEOUT_SECONDS: '0' }],
      [[good], { MUSIC_LOUDNESS_TIMEOUT_SECONDS: '99999' }], [[good], { MUSIC_LOUDNESS_KILL_GRACE_SECONDS: 'soon' }]]) {
      const result = spawnSync('perl', [wrapper, ...args], { encoding: 'utf8', env: { PATH: process.env.PATH, ...extra } });
      assert.equal(result.status, 2);
      assert.equal(result.stdout, '');
    }
  });
  console.log(`Music loudness wrapper: ${passed} checks passed (EBU Tech 3341 cases 1-6 and 15-19; cases 7-8 and 20-23 not covered).`);
} finally {
  rmSync(work, { recursive: true, force: true });
}
