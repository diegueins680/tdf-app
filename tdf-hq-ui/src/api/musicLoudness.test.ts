import { jest } from '@jest/globals';

const get = jest.fn<(path: string) => Promise<unknown>>();
jest.unstable_mockModule('./client', () => ({ get, post: jest.fn(), put: jest.fn(), del: jest.fn() }));
const { parseMusicLoudnessCheck } = await import('./musicLoudness');
const { musicReleases } = await import('./musicReleases');

const limits = { integratedLufsTenths: -140, toleranceTenths: 20, truePeakCeilingDbtpTenths: -10 };
const track = {
  recordingId: '20000000-0000-4000-8000-000000000001', masterAssetId: '10000000-0000-4000-8000-000000000001',
  status: 'pass', checked: true, flags: [] as string[], integratedLufsTenths: -142, truePeakDbtpTenths: -13,
  failureReason: null as string | null,
};
const unmeasured = { ...track, status: 'not_measured', checked: false, integratedLufsTenths: null, truePeakDbtpTenths: null };

describe('music loudness client', () => {
  it('reads the version route and returns a valid answer unchanged', async () => {
    const answer = { limits, tracks: [track, { ...track, status: 'warn', flags: ['too_loud'] }, unmeasured,
      { ...unmeasured, masterAssetId: null }, { ...unmeasured, status: 'failed', failureReason: 'timeout' },
      { ...unmeasured, status: 'pending' }] };
    get.mockResolvedValueOnce(answer);
    await expect(musicReleases.getLoudness('release', 'version')).resolves.toBe(answer);
    expect(get).toHaveBeenCalledWith('/music/releases/release/versions/version/loudness');
  });

  it.each([
    ['a proxy page', '<html></html>'],
    ['missing limits', { tracks: [] }],
    ['fractional limits', { limits: { ...limits, toleranceTenths: 2.5 }, tracks: [] }],
    ['an unknown status', { limits, tracks: [{ ...track, status: 'approved' }] }],
    ['an unknown flag', { limits, tracks: [{ ...track, status: 'warn', flags: ['clipped'] }] }],
    ['checked without a decision', { limits, tracks: [{ ...unmeasured, checked: true }] }],
    ['a decision that is not checked', { limits, tracks: [{ ...track, checked: false }] }],
    ['a decision without values', { limits, tracks: [{ ...track, truePeakDbtpTenths: null }] }],
    ['values without a decision', { limits, tracks: [{ ...unmeasured, integratedLufsTenths: -140 }] }],
    ['a decision without a master file', { limits, tracks: [{ ...track, masterAssetId: null }] }],
    ['a warning without flags', { limits, tracks: [{ ...track, status: 'warn' }] }],
    ['a pass with flags', { limits, tracks: [{ ...track, flags: ['too_loud'] }] }],
    ['a non-numeric value', { limits, tracks: [{ ...track, integratedLufsTenths: '-14.2' }] }],
  ])('rejects %s', (_label, value) => {
    expect(() => parseMusicLoudnessCheck(value)).toThrow('Invalid music loudness response.');
  });
});
