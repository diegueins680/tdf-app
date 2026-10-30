import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import type { ArtistFeaturedTrack } from '../api/musicReleases';
import type { SessionUser } from '../session/SessionContext';
import type { PlayerTrack } from './types';

let session: SessionUser | null;
const getMyFeaturedTrack = jest.fn<(signal?: AbortSignal) => Promise<ArtistFeaturedTrack>>();
const setMyFeaturedTrack = jest.fn<(value: Pick<ArtistFeaturedTrack, 'artistPartyId' | 'recordingId' | 'legacyReleaseId'>, signal?: AbortSignal) => Promise<ArtistFeaturedTrack>>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session, loading: false }), getActiveSession: () => session }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: { getMyFeaturedTrack, setMyFeaturedTrack } }));
const { default: ArtistFeaturedTrackEditor } = await import('./ArtistFeaturedTrackEditor');
const baseline = (recordingId: string | null = null): ArtistFeaturedTrack => ({ artistPartyId: 17, recordingId,
  legacyReleaseId: null, available: Boolean(recordingId), updatedAt: null });
const tracks: PlayerTrack[] = ['a', 'b'].map((id) => ({ id, recordingId: id, title: `Song ${id}`, artist: 'Artist', sources: [] }));
const save = () => screen.getByRole('button', { name: 'Guardar tema destacado' });
beforeEach(() => {
  session = { partyId: 17, username: 'owner', roles: ['Artist'] } as SessionUser;
  getMyFeaturedTrack.mockReset().mockResolvedValue(baseline()); setMyFeaturedTrack.mockReset();
});
afterEach(cleanup);

it('waits for the authoritative save and binds the command to the expected artist', async () => {
  let complete: (value: ArtistFeaturedTrack) => void = () => undefined;
  setMyFeaturedTrack.mockImplementation(() => new Promise((resolve) => { complete = resolve; }));
  render(<ArtistFeaturedTrackEditor artistId={17} tracks={tracks} />);
  await waitFor(() => expect(screen.getByLabelText('Tema destacado').hasAttribute('disabled')).toBe(false));
  fireEvent.change(screen.getByLabelText('Tema destacado'), { target: { value: 'recording:b' } });
  fireEvent.click(save());
  expect(setMyFeaturedTrack.mock.calls[0]?.[0]).toEqual({ artistPartyId: 17, recordingId: 'b', legacyReleaseId: null });
  expect(screen.queryByRole('status')).toBeNull();
  await act(async () => complete(baseline('b')));
  expect(screen.getByRole('status').textContent).toContain('guardado');
  expect(screen.getByLabelText<HTMLSelectElement>('Tema destacado').value).toBe('recording:b');
});

it('blocks writes after a failed settings read and permits an explicit retry', async () => {
  getMyFeaturedTrack.mockRejectedValueOnce(new Error('Offline'));
  render(<ArtistFeaturedTrackEditor artistId={17} tracks={tracks} />);
  await screen.findByRole('alert');
  expect(save().hasAttribute('disabled')).toBe(true);
  expect(setMyFeaturedTrack).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await waitFor(() => expect(screen.getByLabelText('Tema destacado').hasAttribute('disabled')).toBe(false));
});

it('does not expose settings for another artist', () => {
  render(<ArtistFeaturedTrackEditor artistId={18} tracks={tracks} />);
  expect(screen.queryByLabelText('Tema destacado')).toBeNull();
  expect(getMyFeaturedTrack).not.toHaveBeenCalled();
});

it('discards a read receipt identifying another account', async () => {
  getMyFeaturedTrack.mockResolvedValue({ ...baseline('a'), artistPartyId: 18 });
  render(<ArtistFeaturedTrackEditor artistId={17} tracks={tracks} />);
  await screen.findByRole('alert');
  expect(save().hasAttribute('disabled')).toBe(true);
  expect(screen.getByLabelText<HTMLSelectElement>('Tema destacado').value).toBe('');
});

it('retains an unavailable selection for its owner and can explicitly clear it', async () => {
  getMyFeaturedTrack.mockResolvedValue({ ...baseline('retired'), available: false });
  setMyFeaturedTrack.mockResolvedValue(baseline());
  render(<ArtistFeaturedTrackEditor artistId={17} tracks={[]} />);
  await screen.findByText(/El tema guardado ya no está disponible/);
  fireEvent.change(screen.getByLabelText('Tema destacado'), { target: { value: '' } });
  fireEvent.click(save());
  await screen.findByRole('status');
  expect(setMyFeaturedTrack.mock.calls[0]?.[0]).toEqual({ artistPartyId: 17, recordingId: null, legacyReleaseId: null });
});

it('aborts an old account write and ignores its late receipt after an account change', async () => {
  let complete: (value: ArtistFeaturedTrack) => void = () => undefined;
  setMyFeaturedTrack.mockImplementation(() => new Promise((resolve) => { complete = resolve; }));
  const view = render(<ArtistFeaturedTrackEditor artistId={17} tracks={tracks} />);
  await waitFor(() => expect(screen.getByLabelText('Tema destacado').hasAttribute('disabled')).toBe(false));
  fireEvent.change(screen.getByLabelText('Tema destacado'), { target: { value: 'recording:b' } });
  fireEvent.click(save());
  session = { ...session!, partyId: 18 };
  view.rerender(<ArtistFeaturedTrackEditor artistId={17} tracks={tracks} />);
  await act(async () => complete(baseline('b')));
  expect(setMyFeaturedTrack.mock.calls[0]?.[1]?.aborted).toBe(true);
  expect(screen.queryByRole('status')).toBeNull();
  expect(screen.queryByLabelText('Tema destacado')).toBeNull();
});
