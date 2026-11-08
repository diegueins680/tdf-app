import { jest } from '@jest/globals';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import type { MusicReleaseVersion, UploadMusicAssetOptions } from '../api/musicReleases';

// Studio master/cover uploads: drag and drop, per-file cancellation and a
// retry that keeps the same file identity and Idempotency-Key. API is synthetic.
jest.setTimeout(20_000);

const version = {
  id: 'version', releaseId: 'release', versionNumber: 1, state: 'draft', title: 'Borrador',
  displayArtist: 'Artista sintético', titleLanguage: 'es', explicitContent: 'unknown',
  tracks: [{ trackId: '00000000-0000-4000-8000-000000000002', recordingId: '00000000-0000-4000-8000-000000000003',
    discNumber: 1, trackNumber: 1, displayArtist: 'Artista sintético', isPrimaryResource: true,
    previewStartMs: null, previewDurationMs: null, title: 'Pista sintética', subtitle: null, versionTitle: null,
    titleLanguage: 'es', titleScript: null, durationMs: 60000, explicitContent: 'not_explicit' }],
  parties: [], credits: [], identifiers: [], rights: [], availability: [],
  terms: [], assets: [], comments: [], validation: { errors: [] }, updatedAt: '2026-09-15T00:00:00Z',
} as unknown as MusicReleaseVersion;
const uploadAsset = jest.fn<(options: UploadMusicAssetOptions) => Promise<unknown>>();
const getVersion = jest.fn<() => Promise<MusicReleaseVersion>>();
const transition = jest.fn<(releaseId: string, versionId: string, input: Record<string, unknown>, idempotencyKey: string) => Promise<unknown>>();
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: {
  partyId: 101, displayName: 'Artista sintético', roles: ['Artist'],
} }) }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: { listPublicItems: async () => ({ items: [] }) } }));
jest.unstable_mockModule('../api/musicReleases', () => ({ musicReleases: {
  getVersion, analytics: async () => null,
  saveMetadata: async () => version, saveContent: async () => version,
  acceptTerms: async () => undefined, validate: async () => ({ valid: true, errors: [] }), transition,
  uploadAsset,
} }));
const { default: Studio, acceptsUploadFile, sameUploadFile } = await import('./MusicReleaseStudioPage');

const wav = (name = 'master.wav') => new File([new Uint8Array([82, 73, 70, 70])], name, { type: 'audio/wav', lastModified: 1_700_000_000_000 });

beforeEach(() => {
  uploadAsset.mockReset();
  transition.mockReset();
  getVersion.mockReset().mockResolvedValue(version);
});

function openStudio() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0, staleTime: Infinity } } });
  client.setQueryData(['music-release-version', 'release', 'version'], version);
  client.setQueryData(['catalog', 'genres', 'music-release-studio'], { items: [] });
  client.setQueryData(['music-release-analytics', 'release', '', ''], null);
  const view = render(<QueryClientProvider client={client}><MemoryRouter initialEntries={['/release/version']}>
    <Routes><Route path="/:releaseId/:versionId" element={<Studio />} /></Routes>
  </MemoryRouter></QueryClientProvider>);
  expect(screen.getByDisplayValue('Borrador')).toBeTruthy();
  return () => { view.unmount(); client.clear(); };
}

async function dropMaster(file: File) {
  const zone = screen.getByTestId('upload-dropzone-master_audio');
  await act(async () => {
    fireEvent.dragOver(zone, { dataTransfer: { files: [file], types: ['Files'] } });
    fireEvent.drop(zone, { dataTransfer: { files: [file], types: ['Files'] } });
  });
}

it('admits dropped masters by type or extension and rejects foreign files', () => {
  expect(acceptsUploadFile('master_audio', { name: 'x.wav', type: 'audio/wav' })).toBe(true);
  expect(acceptsUploadFile('master_audio', { name: 'x.aiff', type: '' })).toBe(true);
  expect(acceptsUploadFile('master_audio', { name: 'x.mp3', type: 'audio/mpeg' })).toBe(false);
  expect(acceptsUploadFile('cover_original', { name: 'cover.PNG', type: '' })).toBe(true);
  expect(acceptsUploadFile('cover_original', { name: 'cover.gif', type: 'image/gif' })).toBe(false);
  expect(sameUploadFile(wav(), wav())).toBe(true);
  expect(sameUploadFile(wav(), wav('other.wav'))).toBe(false);
});

it('uploads a dropped master, then retries the same file with the same Idempotency-Key after a failure', async () => {
  uploadAsset.mockRejectedValueOnce(new Error('Corte de red sintético'));
  uploadAsset.mockResolvedValueOnce({});
  const close = openStudio();
  try {
    fireEvent.change(screen.getByDisplayValue('Borrador'), { target: { value: 'Título conservado' } });
    await dropMaster(wav());
    await screen.findByText('Corte de red sintético');
    expect(uploadAsset).toHaveBeenCalledTimes(1);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Reintentar pista 1' })); });
    await waitFor(() => expect(uploadAsset).toHaveBeenCalledTimes(2));
    const [first, second] = uploadAsset.mock.calls.map(([options]) => options);
    expect(first.file).toBe(second.file);
    expect(first.idempotencyKey).toMatch(/^music-upload-/);
    expect(second.idempotencyKey).toBe(first.idempotencyKey);
    expect(second.assetRole).toBe('master_audio');
    expect(second.recordingId).toBe('00000000-0000-4000-8000-000000000003');
    await screen.findByText('Carga confirmada; procesamiento en cola.');
    expect(screen.getByDisplayValue('Título conservado')).toBeTruthy();
  } finally { close(); }
});

it('cancels an in-flight upload through its signal and retries the same file as a new upload', async () => {
  uploadAsset.mockImplementationOnce(({ signal }) => new Promise((_resolve, reject) => {
    signal?.addEventListener('abort', () => reject(new DOMException('Upload cancelled', 'AbortError')));
  }));
  uploadAsset.mockResolvedValueOnce({});
  const close = openStudio();
  try {
    await dropMaster(wav());
    const cancel = await screen.findByRole('button', { name: 'Cancelar carga pista 1' });
    await act(async () => { fireEvent.click(cancel); });
    await screen.findByText('Carga cancelada. Puedes reintentar el mismo archivo.');
    expect(uploadAsset.mock.calls[0]?.[0].signal?.aborted).toBe(true);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Reintentar pista 1' })); });
    await waitFor(() => expect(uploadAsset).toHaveBeenCalledTimes(2));
    // The server deleted the cancelled session; replaying its key would return
    // a cancelled session, so the retry carries a fresh key for the same file.
    expect(uploadAsset.mock.calls[1]?.[0].file).toBe(uploadAsset.mock.calls[0]?.[0].file);
    expect(uploadAsset.mock.calls[1]?.[0].idempotencyKey).toMatch(/^music-upload-/);
    expect(uploadAsset.mock.calls[1]?.[0].idempotencyKey).not.toBe(uploadAsset.mock.calls[0]?.[0].idempotencyKey);
  } finally { close(); }
});

it('starts a new request identity for a different file and ignores unsupported drops', async () => {
  uploadAsset.mockResolvedValue({});
  const close = openStudio();
  try {
    await dropMaster(new File(['x'], 'song.mp3', { type: 'audio/mpeg' }));
    expect(uploadAsset).not.toHaveBeenCalled();
    await dropMaster(wav('take-1.wav'));
    await waitFor(() => expect(uploadAsset).toHaveBeenCalledTimes(1));
    await screen.findByText('Carga confirmada; procesamiento en cola.');
    await dropMaster(wav('take-2.wav'));
    await waitFor(() => expect(uploadAsset).toHaveBeenCalledTimes(2));
    expect(uploadAsset.mock.calls[1]?.[0].idempotencyKey).not.toBe(uploadAsset.mock.calls[0]?.[0].idempotencyKey);
  } finally { close(); }
});

it('replays a failed publication transition with the same Idempotency-Key and rotates it after success', async () => {
  transition.mockRejectedValueOnce(new Error('Sin conexión'));
  transition.mockResolvedValue({});
  getVersion.mockResolvedValue({ ...version, state: 'in_review' });
  const close = openStudio();
  try {
    const submit = () => act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Enviar a revisión' })); });
    await submit();
    await screen.findByText('Sin conexión');
    await submit();
    await waitFor(() => expect(transition).toHaveBeenCalledTimes(2));
    const [first, second] = transition.mock.calls;
    expect(first?.[2]).toEqual({ targetState: 'ready_for_review', reason: null });
    expect(first?.[3]).toMatch(/^music-transition-/);
    expect(second?.[3]).toBe(first?.[3]);
    await screen.findByText('Estado actualizado: in_review.');
  } finally { close(); }
});

it('allocates a new key when the server reports the replayed session is no longer usable', async () => {
  const unusable = Object.assign(new Error('La sesión de carga anterior quedó en estado expired. Reintenta para crear una nueva carga del mismo archivo.'), { name: 'MusicUploadSessionUnusableError' });
  uploadAsset.mockRejectedValueOnce(unusable);
  uploadAsset.mockResolvedValueOnce({});
  const close = openStudio();
  try {
    await dropMaster(wav());
    await screen.findByText(/quedó en estado expired/);
    await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Reintentar pista 1' })); });
    await waitFor(() => expect(uploadAsset).toHaveBeenCalledTimes(2));
    expect(uploadAsset.mock.calls[1]?.[0].file).toBe(uploadAsset.mock.calls[0]?.[0].file);
    expect(uploadAsset.mock.calls[1]?.[0].idempotencyKey).not.toBe(uploadAsset.mock.calls[0]?.[0].idempotencyKey);
    await screen.findByText('Carga confirmada; procesamiento en cola.');
  } finally { close(); }
});

it('keeps a confirmed upload confirmed when the version refresh fails', async () => {
  uploadAsset.mockResolvedValueOnce({});
  getVersion.mockRejectedValueOnce(new Error('refresh offline'));
  const close = openStudio();
  try {
    await dropMaster(wav());
    await screen.findByText(/Carga confirmada; no se pudo refrescar la versión/);
    expect(screen.queryByRole('button', { name: 'Reintentar pista 1' })).toBeNull();
    expect(uploadAsset).toHaveBeenCalledTimes(1);
  } finally { close(); }
});
