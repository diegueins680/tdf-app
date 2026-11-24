import { render, screen } from '@testing-library/react';

import type { MusicLoudnessTrack } from '../../api/musicLoudness';
import TrackLoudnessCheck, { formatTenths, loudnessMessages, newestMasterAssetId } from './TrackLoudnessCheck';

const limits = { integratedLufsTenths: -140, toleranceTenths: 20, truePeakCeilingDbtpTenths: -10 };
const master = '10000000-0000-4000-8000-000000000001';
const track = (overrides: Partial<MusicLoudnessTrack> = {}): MusicLoudnessTrack => ({
  recordingId: '20000000-0000-4000-8000-000000000001', masterAssetId: master, status: 'pass', checked: true,
  flags: [], integratedLufsTenths: -142, truePeakDbtpTenths: -13, failureReason: null, ...overrides,
});
const show = (value: MusicLoudnessTrack | undefined, current: string | null = master) =>
  render(<TrackLoudnessCheck track={value} limits={limits} label="la pista 1" currentMasterAssetId={current} />);

describe('TrackLoudnessCheck', () => {
  it('formats tenths without floating point', () => {
    expect([-142, -5, 0, 7, -1000].map(formatTenths)).toEqual(['−14.2', '−0.5', '0.0', '0.7', '−100.0']);
  });

  it('reports a pass with both values', () => {
    show(track());
    expect(screen.getByRole('status').textContent).toBe('Sonoridad de la pista 1: −14.2 LUFS, pico real −1.3 dBTP. Dentro del objetivo.');
  });

  it('explains every warning and says it does not block publication', () => {
    const loud = track({ status: 'warn', flags: ['too_loud', 'true_peak_over'], integratedLufsTenths: -80, truePeakDbtpTenths: 3 });
    expect(loudnessMessages(loud, limits)).toHaveLength(2);
    show(loud);
    const text = screen.getByRole('status').textContent ?? '';
    expect(text).toContain('−8.0 LUFS');
    expect(text).toContain('más fuerte que el objetivo de −14.0 LUFS (±2.0)');
    expect(text).toContain('supera −1.0 dBTP');
    expect(text).toContain('no bloquea la publicación');
    expect(loudnessMessages(track({ status: 'warn', flags: ['too_quiet'] }), limits)[0]).toContain('más bajo');
  });

  it('reports a failed or pending measurement without values', () => {
    const failed = track({ status: 'failed', checked: false, integratedLufsTenths: null, truePeakDbtpTenths: null, failureReason: 'timeout' });
    const { unmount } = show(failed);
    expect(screen.getByRole('status').textContent).toContain('superó el tiempo límite');
    expect(screen.getByRole('status').textContent).toContain('no bloquea la publicación');
    unmount();
    show({ ...failed, status: 'pending', failureReason: null });
    expect(screen.getByRole('status').textContent).toContain('Midiendo la sonoridad');
  });

  it('shows nothing for another file, an unmeasured track or a missing result', () => {
    for (const [value, current] of [
      [track(), '10000000-0000-4000-8000-000000000002'], [track(), null], [undefined, master],
      [track({ status: 'not_measured', checked: false, integratedLufsTenths: null, truePeakDbtpTenths: null }), master],
      [track({ masterAssetId: null, status: 'not_measured', checked: false, integratedLufsTenths: null, truePeakDbtpTenths: null }), null],
    ] as const) {
      const { container, unmount } = show(value, current);
      expect(container.textContent).toBe('');
      unmount();
    }
  });

  it('chooses the newest master of the recording that was not deleted', () => {
    const asset = (id: string, createdAt: string, extra = {}) => ({
      id, recordingId: 'r1', role: 'master_audio', processingState: 'valid', createdAt, ...extra });
    const assets = [
      asset('a', '2026-11-21T10:00:00Z'), asset('c', '2026-11-21T11:00:00Z'), asset('b', '2026-11-21T11:00:00Z'),
      asset('d', '2026-11-21T12:00:00Z', { processingState: 'deleted' }),
      asset('e', '2026-11-21T13:00:00Z', { recordingId: 'r2' }),
      asset('f', '2026-11-21T14:00:00Z', { role: 'stream_audio' }),
    ];
    expect(newestMasterAssetId(assets, 'r1')).toBe('c');
    expect(newestMasterAssetId(assets, 'r3')).toBeNull();
    expect(newestMasterAssetId(assets, undefined)).toBeNull();
  });
});
