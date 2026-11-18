import { isArtistMediaRecoveryEnabled } from './artistMediaRecoveryRollout';

describe('artist media recovery rollout selection', () => {
  it('selects the new integration only for exact string true', () => {
    expect(isArtistMediaRecoveryEnabled('true')).toBe(true);
  });
  it.each([undefined, null, false, true, '', 'false', 'TRUE', 'True', ' true', 'true ', '1', 1, 0])('leaves existing playback selected for %p', value => {
    expect(isArtistMediaRecoveryEnabled(value)).toBe(false);
  });
});
