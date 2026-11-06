import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import type { VideoCampaign, VideoCampaignDocument } from '../features/campaigns/videoCampaign';

let session: SessionUser | null = { partyId: 51, username: 'artist', displayName: 'Artist', roles: ['Artist'] };
let persisted: VideoCampaign[] = [];
const save = jest.fn<(artist: number, id: string, revision: number, content: VideoCampaignDocument) => Promise<VideoCampaign>>();
const list = jest.fn<() => Promise<VideoCampaign[]>>();
const feature = jest.fn<(artist: number, id: string, revision: number) => Promise<void>>();
const getPublicArtist = jest.fn<(id: string) => Promise<{ apArtistId: number; apDisplayName: string }>>();
const listArtists = jest.fn<() => Promise<{ apArtistId: number; apDisplayName: string }[]>>();
jest.unstable_mockModule('../api/videoCampaigns', () => ({ VideoCampaigns: { list, save, feature, metrics: async () => ({ registrations: 0, follows: 0 }) } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session, loading: false }), getActiveSession: () => session }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  getMyArtistProfile: async () => ({ apArtistId: 51, apDisplayName: 'Liquid Paper Girl' }),
  getPublicArtist, listArtists,
} }));
jest.unstable_mockModule('../components/GoogleDriveUploadWidget', () => ({ default: () => null }));
class ApiError extends Error { constructor(message: string, public status: number) { super(message); } }
jest.unstable_mockModule('../api/client', () => ({ ApiError, get: async () => ({}), put: async () => ({}) }));
const { default: VideoCampaignsPage } = await import('./VideoCampaignsPage');
const show = (path = '/', client = new QueryClient({ defaultOptions: { queries: { retry: false } } })) => render(<QueryClientProvider client={client}>
  <MemoryRouter initialEntries={[path]}><VideoCampaignsPage /></MemoryRouter></QueryClientProvider>);

beforeEach(() => {
  jest.spyOn(window, 'scrollTo').mockImplementation(() => undefined);
  persisted = [];
  session = { partyId: 51, username: 'artist', displayName: 'Artist', roles: ['Artist'] };
  list.mockReset().mockImplementation(async () => persisted);
  getPublicArtist.mockReset().mockResolvedValue({ apArtistId: 51, apDisplayName: 'Liquid Paper Girl' });
  listArtists.mockReset().mockResolvedValue([]);
  feature.mockReset().mockResolvedValue(undefined);
  save.mockReset().mockImplementation(async (artistId, id, revision, content) => {
    const row = { id, artistId, revision: revision + 1, content,
      createdAt: '2026-10-20T00:00:00Z', updatedAt: '2026-10-20T00:00:00Z' };
    persisted = [row, ...persisted.filter((previous) => previous.id !== id)]; return row;
  });
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); });

it('saves a real API draft and resumes it after leaving the editor', async () => {
  show();
  fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.change(screen.getByLabelText('Título de la sesión'), { target: { value: 'Liquid Paper Girl · Live session' } });
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  fireEvent.change(screen.getByLabelText('Objetivo principal'), { target: { value: 'Priorizar nuevos seguidores en TDF' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save).toHaveBeenCalledTimes(1);
  expect(save.mock.calls[0]?.[0]).toBe(51);
  expect(save.mock.calls[0]?.[3].status).toBe('draft');
  fireEvent.click(screen.getByRole('button', { name: 'Volver a campañas' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Continuar' }));
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Liquid Paper Girl · Live session');
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  expect(screen.getByLabelText('Objetivo principal')).toHaveValue('Priorizar nuevos seguidores en TDF');
});

it('preserves the draft and reports a failed save so it can be retried', async () => {
  save.mockRejectedValueOnce(new Error('La campaña cambió. Recarga antes de guardar.'));
  show(); fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.change(screen.getByLabelText('Título de la sesión'), { target: { value: 'Keep my edit' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/La campaña cambió/)).toBeInTheDocument();
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Keep my edit');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  await waitFor(() => expect(save).toHaveBeenCalledTimes(2));
});

it.each([false, true])('retains editor edits, campaign ID and revision through failed background refreshes (saved=%s)', async (existing) => {
  if (existing) persisted = [completeCampaign()];
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  show('/', client);
  fireEvent.click(await screen.findByRole('button', { name: existing ? 'Continuar' : 'Crear campaña' }));
  fireEvent.change(screen.getByLabelText('Título de la sesión'), { target: { value: 'Cambio sin guardar' } });
  list.mockRejectedValue(new Error('Temporary network failure'));
  await act(async () => { await client.refetchQueries({ queryKey: ['video-campaigns'] }); });
  expect(await screen.findByText(/Tus cambios en el editor se conservan/)).toBeInTheDocument();
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Cambio sin guardar');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  await waitFor(() => expect(list).toHaveBeenCalledTimes(3));
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Cambio sin guardar');
  fireEvent.change(screen.getByLabelText('Título de la sesión'), { target: { value: 'Siguiente cambio' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  await waitFor(() => expect(save).toHaveBeenCalledTimes(2));
  expect(save.mock.calls[0]?.[2]).toBe(existing ? 3 : 0);
  expect(save.mock.calls[1]?.[1]).toBe(save.mock.calls[0]?.[1]);
  expect(save.mock.calls[1]?.[2]).toBe(existing ? 4 : 1);
  expect(save.mock.calls[1]?.[3].title).toBe('Siguiente cambio');
  list.mockImplementation(async () => persisted);
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
  await waitFor(() => expect(screen.queryByText(/Tus cambios en el editor se conservan/)).not.toBeInTheDocument());
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Siguiente cambio');
});

it.each([401, 403, 404])('hides the cached workspace when a refresh explicitly denies access (%s)', async (status) => {
  persisted = [completeCampaign()];
  const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  show('/', client);
  fireEvent.click(await screen.findByRole('button', { name: 'Continuar' }));
  list.mockRejectedValue(new ApiError('Access denied', status));
  await act(async () => { await client.refetchQueries({ queryKey: ['video-campaigns'] }); });
  expect(await screen.findByText(/No pudimos cargar tus campañas/)).toBeInTheDocument();
  expect(screen.queryByLabelText('Título de la sesión')).not.toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Crear campaña' })).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
});

it('prepares a newly selected or reselected channel without replacing the other channel edits', async () => {
  show(); fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  fireEvent.click(screen.getByLabelText('TikTok'));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar textos y calendario' }));
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  fireEvent.change(screen.getAllByLabelText('Texto de la publicación')[0]!, { target: { value: 'Mi edición de Instagram' } });
  fireEvent.click(screen.getByRole('button', { name: 'Anterior' }));
  fireEvent.click(screen.getByLabelText('TikTok'));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar textos y calendario' }));
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  expect(screen.getAllByLabelText('Texto de la publicación')).toHaveLength(14);
  expect(screen.getAllByLabelText('Texto de la publicación')[0]).toHaveValue('Mi edición de Instagram');
  fireEvent.click(screen.getByRole('button', { name: 'Anterior' }));
  fireEvent.click(screen.getByLabelText('TikTok'));
  fireEvent.click(screen.getByLabelText('TikTok'));
  fireEvent.click(screen.getByRole('button', { name: 'Preparar textos y calendario' }));
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  expect(screen.getAllByLabelText('Texto de la publicación')).toHaveLength(14);
  expect(screen.getAllByLabelText('Texto de la publicación')[0]).toHaveValue('Mi edición de Instagram');
});

it('does not show the editor or query campaigns to anonymous visitors', async () => {
  session = null; list.mockClear(); show();
  expect(screen.getByRole('link', { name: 'Iniciar sesión' })).toBeInTheDocument();
  expect(list).not.toHaveBeenCalled();
  expect(screen.queryByRole('button', { name: 'Crear campaña' })).not.toBeInTheDocument();
});

const completeCampaign = (): VideoCampaign => ({
  id: '126f208c-2b38-47c1-952b-6ba633122c87', artistId: 51, revision: 3,
  createdAt: '2026-10-20T00:00:00Z', updatedAt: '2026-10-20T00:00:00Z',
  content: {
    title: 'Liquid Paper Girl · Live session', status: 'draft', rightsConfirmed: true, technicalReviewed: true,
    videoUrl: 'https://media.example.com/session.mp4', coverUrl: '', durationSeconds: 610.08,
    channels: ['instagram'], pieces: [{
      id: 'instagram-1', channel: 'instagram', day: 0, format: 'vertical-video',
      caption: 'Descubre la sesión y sigue a Liquid Paper Girl en TDF.',
      assetUrl: 'https://media.example.com/clip.mp4', sourceStart: 94.55, sourceEnd: 124.55,
      status: 'prepared', publishedUrl: '',
    }],
  },
});

async function openReview() {
  show(); fireEvent.click(await screen.findByRole('button', { name: 'Continuar' }));
  for (let step = 0; step < 3; step += 1) {
    fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  }
}

it('keeps a review save private until the artist explicitly publishes its saved revision', async () => {
  persisted = [completeCampaign()];
  await openReview();
  expect(screen.queryByRole('button', { name: 'Publicar video en el perfil de TDF' })).not.toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Guardar para revisión' }));
  expect(await screen.findByText(/Campaña guardada para revisión/)).toBeInTheDocument();
  expect(feature).not.toHaveBeenCalled();
  expect(save.mock.calls[0]?.[3].status).toBe('ready');
  fireEvent.click(screen.getByRole('button', { name: 'Publicar video en el perfil de TDF' }));
  expect(await screen.findByText(/Video destacado actualizado en TDF/)).toBeInTheDocument();
  expect(feature).toHaveBeenCalledWith(51, persisted[0]!.id, 4);
});

it('reports a rejected public update without claiming publication or changing the review plan', async () => {
  const row = completeCampaign(); row.content.status = 'ready'; persisted = [row];
  feature.mockRejectedValueOnce(new Error('La campaña cambió. Recarga antes de publicar.'));
  await openReview();
  fireEvent.click(screen.getByRole('button', { name: 'Publicar video en el perfil de TDF' }));
  expect(await screen.findByText(/La campaña cambió/)).toBeInTheDocument();
  expect(screen.queryByText(/Video destacado actualizado en TDF/)).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
  expect(feature).toHaveBeenCalledWith(51, row.id, 3);
});

it('requires saved rights and technical review and invalidates them after edits', async () => {
  const row = completeCampaign(); row.content.status = 'ready';
  row.content.rightsConfirmed = false; row.content.technicalReviewed = false; persisted = [row];
  await openReview();
  expect(screen.queryByRole('button', { name: 'Publicar video en el perfil de TDF' })).not.toBeInTheDocument();
  fireEvent.click(screen.getByLabelText('Confirmo los permisos de música, imagen y video para publicar en TDF'));
  fireEvent.click(screen.getByLabelText('Revisé el video completo, el audio, los cortes y los créditos'));
  expect(feature).not.toHaveBeenCalled();
  fireEvent.click(screen.getByRole('button', { name: 'Guardar para revisión' }));
  expect(await screen.findByText(/Campaña guardada para revisión/)).toBeInTheDocument();
  expect(screen.getByRole('button', { name: 'Publicar video en el perfil de TDF' })).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Anterior' }));
  fireEvent.click(screen.getByRole('button', { name: 'Anterior' }));
  fireEvent.change(screen.getByLabelText('Fecha propuesta (opcional)'), { target: { value: '2026-12-01' } });
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  expect(screen.getByLabelText('Confirmo los permisos de música, imagen y video para publicar en TDF')).not.toBeChecked();
  expect(screen.queryByRole('button', { name: 'Publicar video en el perfil de TDF' })).not.toBeInTheDocument();
});

it('uses the authorized canonical artist target for an ordinary band manager', async () => {
  session = { partyId: 99, username: 'manager', displayName: 'Manager', roles: ['Customer'] };
  show('/mi-artista/campanas?artistId=51');
  fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save.mock.calls[0]?.[0]).toBe(51);
  expect(session.partyId).toBe(99);
  expect(session.roles).toEqual(['Customer']);
});

it('keeps the editor unavailable when the backend denies an artist target', async () => {
  session = { partyId: 99, username: 'manager', displayName: 'Manager', roles: ['Customer'] };
  list.mockRejectedValueOnce(new Error('No artist grant'));
  show('/mi-artista/campanas?artistId=51');
  expect(await screen.findByText(/No pudimos cargar tus campañas/)).toBeInTheDocument();
  expect(screen.queryByRole('button', { name: 'Crear campaña' })).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
});

function importedFile(row: VideoCampaign, title = row.content.title): File {
  const json = JSON.stringify({ artistId: row.artistId, campaignId: row.id, ...row.content, title });
  const file = new File([json], 'campaign.json', { type: 'application/json' });
  Object.defineProperty(file, 'text', { value: async () => json, configurable: true });
  return file;
}

it('reconciles an edited export against the current saved revision only after an explicit choice', async () => {
  const row = completeCampaign(); persisted = [row]; show();
  const input = await screen.findByLabelText('Archivo del plan');
  fireEvent.change(input, { target: { files: [importedFile(row, 'Edición importada')] } });
  fireEvent.click(await screen.findByRole('button', { name: 'Revisar cambios sobre esta campaña' }));
  expect(save).not.toHaveBeenCalled();
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Edición importada');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save.mock.calls[0]?.[2]).toBe(3);
  expect(save.mock.calls[0]?.[3].rightsConfirmed).toBe(false);
  expect(feature).not.toHaveBeenCalled();
});

it('imports a new copy with fresh links and leaves the saved campaign untouched', async () => {
  const row = completeCampaign(); persisted = [row]; show();
  fireEvent.change(await screen.findByLabelText('Archivo del plan para copia nueva'), { target: { files: [importedFile(row)] } });
  await screen.findByLabelText('Título de la sesión');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save.mock.calls[0]?.[1]).not.toBe(row.id);
  expect(save.mock.calls[0]?.[2]).toBe(0);
  expect(save.mock.calls[0]?.[3].technicalReviewed).toBe(false);
  expect(persisted.find((entry) => entry.id === row.id)).toEqual(row);
  expect(persisted).toHaveLength(2);
});

it('lets an authorized manager prepare drafts without public artist metadata', async () => {
  session = { partyId: 99, username: 'manager', displayName: 'Manager', roles: ['Customer'] };
  getPublicArtist.mockRejectedValue(new Error('Artist is not public'));
  show('/mi-artista/campanas?artistId=51');
  fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  expect(await screen.findByText(/Sus datos públicos no están disponibles/)).toBeInTheDocument();
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save.mock.calls[0]?.[0]).toBe(51);
});

it('allows an administrator to select an unlisted canonical artist ID', async () => {
  session = { partyId: 99, username: 'admin', displayName: 'Admin', roles: ['Admin'], modules: ['admin'] };
  show('/mi-artista/campanas?artistId=52');
  expect(await screen.findByLabelText('ID del artista')).toHaveValue(52);
  fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save.mock.calls[0]?.[0]).toBe(52);
  expect(listArtists).toHaveBeenCalled();
});

it('abandons an in-flight import after the selected artist changes', async () => {
  session = { partyId: 99, username: 'admin', displayName: 'Admin', roles: ['Admin'], modules: ['admin'] };
  show('/mi-artista/campanas?artistId=51');
  const row = completeCampaign(); const file = importedFile(row);
  let finish!: (value: string) => void;
  Object.defineProperty(file, 'text', { value: () => new Promise<string>((resolve) => { finish = resolve; }), configurable: true });
  fireEvent.change(await screen.findByLabelText('Archivo del plan'), { target: { files: [file] } });
  fireEvent.change(screen.getByLabelText('ID del artista'), { target: { value: '52' } });
  finish(JSON.stringify({ artistId: row.artistId, campaignId: row.id, ...row.content }));
  await waitFor(() => expect(screen.queryByRole('progressbar', { name: 'Leyendo plan' })).not.toBeInTheDocument());
  expect(screen.queryByLabelText('Título de la sesión')).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
});
