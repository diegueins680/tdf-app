import { jest } from '@jest/globals';
import { act, cleanup, render, screen, waitFor } from '@testing-library/react';
import type { ArtistReleaseDTO } from '../api/types';
import type { ArtistFeaturedTrack, MusicPublicRelease, MusicPublicReleaseSummary } from '../api/musicReleases';
import type { PlayerTrack } from './types';

const beginArtist = jest.fn<(key: string) => void>();
const loadArtist = jest.fn<(key: string, tracks: PlayerTrack[]) => void>();
const play = jest.fn<() => Promise<void>>();
const getPublic = jest.fn<(slug: string, signal?: AbortSignal) => Promise<MusicPublicRelease>>();
const getArtistFeaturedTrack = jest.fn<(id: number, signal?: AbortSignal) => Promise<ArtistFeaturedTrack>>();
const engine = { beginArtist, loadArtist, play, status: 'idle' };
jest.unstable_mockModule('./PlayerProvider', () => ({ useOptionalPlayer: () => engine }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { getPublic, getArtistFeaturedTrack } }));
const { default: ArtistProfilePlayback } = await import('./ArtistProfilePlayback');
const legacy = (url: string) => ({ arReleaseId: 1, arArtistId: 17, arTitle: 'Public legacy', arSpotifyUrl: url }) as ArtistReleaseDTO;

beforeEach(() => { jest.clearAllMocks(); getPublic.mockReset(); getArtistFeaturedTrack.mockReset().mockResolvedValue({ artistPartyId: 17, recordingId: null, legacyReleaseId: null, available: false, updatedAt: null }); });
afterEach(cleanup);

it('prepares public direct legacy media when the canonical catalogue is unavailable', async () => {
  render(<ArtistProfilePlayback profileKey="artist-17" artistId={17} artistName="Artist"
    releases={[]} legacyReleases={[legacy('https://media.invalid/song.mp3')]} loading={false} failed />);
  await waitFor(() => expect(loadArtist).toHaveBeenCalledTimes(1));
  expect(loadArtist).toHaveBeenCalledWith('artist-17', [expect.objectContaining({ id: 'legacy-release-1' })]);
  expect(getPublic).not.toHaveBeenCalled();
  expect(screen.getByRole('button', { name: 'Escuchar a Artist' })).toBeTruthy();
});

it('offers radio for an empty playable catalogue without starting it or treating provider pages as audio', async () => {
  render(<ArtistProfilePlayback profileKey="artist-17" artistId={17} artistName="Artist"
    releases={[]} legacyReleases={[legacy('https://open.spotify.com/track/reference')]} loading={false} failed={false} />);
  await waitFor(() => expect(loadArtist).toHaveBeenCalledWith('artist-17', []));
  expect(screen.getByRole('link', { name: 'Ir a la radio' }).getAttribute('href')).toBe('/inicio#radio');
  expect(play).not.toHaveBeenCalled();
});

it('discards late release details after navigation even if the server ignores abort', async () => {
  let complete: (value: MusicPublicRelease) => void = () => undefined;
  getPublic.mockImplementationOnce(() => new Promise((resolve) => { complete = resolve; }));
  const releases = [{ slug: 'first' }] as MusicPublicReleaseSummary[];
  const view = render(<ArtistProfilePlayback profileKey="artist-17" artistId={17} artistName="First"
    releases={releases} loading={false} failed={false} />);
  await waitFor(() => expect(getPublic).toHaveBeenCalledTimes(1));
  view.rerender(<ArtistProfilePlayback profileKey="artist-18" artistId={18} artistName="Second"
    releases={[]} loading={false} failed={false} />);
  await waitFor(() => expect(loadArtist).toHaveBeenCalledWith('artist-18', []));
  await act(async () => complete({ artistPartyId: 17, tracks: [] } as unknown as MusicPublicRelease));
  expect(loadArtist.mock.calls.map(([key]) => key)).toEqual(['artist-18']);
  expect(getPublic.mock.calls[0]?.[1]?.aborted).toBe(true);
});


it('loads a valid saved featured legacy selection first without changing the rest of the queue', async () => {
  getArtistFeaturedTrack.mockResolvedValue({ artistPartyId: 17, recordingId: null, legacyReleaseId: 2, available: true, updatedAt: null });
  render(<ArtistProfilePlayback profileKey="artist-17" artistId={17} artistName="Artist" releases={[]}
    legacyReleases={[legacy('https://media.invalid/one.mp3'), { ...legacy('https://media.invalid/two.mp3'), arReleaseId: 2 }]}
    loading={false} failed={false} />);
  await waitFor(() => expect(loadArtist).toHaveBeenCalledTimes(1));
  expect(loadArtist.mock.calls[0]?.[1].map((track) => track.id)).toEqual(['legacy-release-2', 'legacy-release-1']);
});

it('bounds an unavailable preference request without preventing the catalogue from loading', async () => {
  jest.useFakeTimers();
  getArtistFeaturedTrack.mockImplementation((_id, signal) => new Promise((_resolve, reject) => {
    signal?.addEventListener('abort', () => reject(new DOMException('Aborted', 'AbortError')), { once: true });
  }));
  try {
    render(<ArtistProfilePlayback profileKey="artist-17" artistId={17} artistName="Artist" releases={[]}
      legacyReleases={[legacy('https://media.invalid/song.mp3')]} loading={false} failed={false} />);
    await act(async () => { await Promise.resolve(); });
    expect(loadArtist).not.toHaveBeenCalled();
    await act(async () => { jest.advanceTimersByTime(2000); });
    expect(loadArtist.mock.calls[0]?.[1].map((track) => track.id)).toEqual(['legacy-release-1']);
  } finally { jest.useRealTimers(); }
});
