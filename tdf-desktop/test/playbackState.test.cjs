'use strict';
const test = require('node:test');
const assert = require('node:assert/strict');
const { PlaybackState, describeForMenu, isCommand, sanitizeReport } = require('../src/main/playbackState.cjs');

test('sanitizes playback reports from the renderer', () => {
  assert.deepEqual(sanitizeReport({ owner: 'music', status: 'playing', title: ' Pista\u0000 ', artist: 'Artista', canNext: true, canPrevious: 'yes' }),
    { owner: 'music', status: 'playing', title: 'Pista', artist: 'Artista', canNext: true, canPrevious: false });
  assert.equal(sanitizeReport({ owner: 'tv', status: 'playing' }), null);
  assert.equal(sanitizeReport({ owner: 'music', status: 'dancing' }), null);
  assert.equal(sanitizeReport('music'), null);
  assert.equal(sanitizeReport({ owner: null, status: 'idle' }).title, null);
  assert.equal(sanitizeReport({ owner: 'radio', status: 'paused', title: 'x'.repeat(500) }).title.length, 200);
});

test('menu model reflects the active owner and its capabilities', () => {
  assert.deepEqual(describeForMenu(null), { nowPlaying: 'Nada en reproducción', toggleLabel: 'Reproducir', toggleEnabled: false, nextEnabled: false, previousEnabled: false, owner: null });
  const radio = describeForMenu(sanitizeReport({ owner: 'radio', status: 'playing', title: 'Emisora', artist: 'Radio TDF' }));
  assert.equal(radio.nowPlaying, 'Emisora · Radio TDF');
  assert.equal(radio.toggleLabel, 'Pausar');
  assert.equal(radio.toggleEnabled, true);
  assert.equal(radio.nextEnabled, false);
  const music = describeForMenu(sanitizeReport({ owner: 'music', status: 'paused', title: 'Pista', artist: null, canNext: true, canPrevious: true }));
  assert.equal(music.toggleLabel, 'Reproducir');
  assert.equal(music.nextEnabled, true);
  assert.equal(describeForMenu(sanitizeReport({ owner: 'music', status: 'idle' })).toggleEnabled, false);
});

test('state notifies listeners only for valid reports', () => {
  const state = new PlaybackState();
  const seen = [];
  state.onChange((report) => seen.push(report.status));
  assert.equal(state.update({ owner: 'music', status: 'loading' }), true);
  assert.equal(state.update({ nonsense: true }), false);
  state.reset();
  assert.deepEqual(seen, ['loading', 'idle']);
  assert.equal(state.get().owner, null);
});

test('only known commands reach the renderer', () => {
  for (const command of ['play', 'pause', 'toggle', 'next', 'previous']) assert.equal(isCommand(command), true);
  assert.equal(isCommand('eval'), false);
  assert.equal(isCommand(1), false);
});
