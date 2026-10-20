import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SessionUser } from '../session/SessionContext';
import type { VideoCampaign, VideoCampaignDocument } from '../features/campaigns/videoCampaign';

let session: SessionUser | null = { partyId: 51, username: 'artist', displayName: 'Artist', roles: ['Artist'] };
let persisted: VideoCampaign[] = [];
const save = jest.fn<(artist: number, id: string, revision: number, content: VideoCampaignDocument) => Promise<VideoCampaign>>();
const list = jest.fn<() => Promise<VideoCampaign[]>>();
const feature = jest.fn<(artist: number, id: string, revision: number) => Promise<void>>();
jest.unstable_mockModule('../api/videoCampaigns', () => ({ VideoCampaigns: { list, save, feature, metrics: async () => ({ registrations: 0, follows: 0 }) } }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session, loading: false }), getActiveSession: () => session }));
jest.unstable_mockModule('../api/fans', () => ({ Fans: {
  getMyArtistProfile: async () => ({ apArtistId: 51, apDisplayName: 'Liquid Paper Girl' }),
} }));
jest.unstable_mockModule('../api/drive', () => ({ uploadToDrive: async () => ({}) }));
jest.unstable_mockModule('../api/client', () => ({ get: async () => ({}), put: async () => ({}) }));
const { default: VideoCampaignsPage } = await import('./VideoCampaignsPage');
const show = () => render(<QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}>
  <MemoryRouter><VideoCampaignsPage /></MemoryRouter></QueryClientProvider>);

beforeEach(() => {
  jest.spyOn(window, 'scrollTo').mockImplementation(() => undefined);
  persisted = [];
  session = { partyId: 51, username: 'artist', displayName: 'Artist', roles: ['Artist'] };
  list.mockImplementation(async () => persisted);
  feature.mockReset().mockResolvedValue(undefined);
  save.mockReset().mockImplementation(async (artistId, id, revision, content) => {
    const row = { id, artistId, revision: revision + 1, content,
      createdAt: '2026-10-20T00:00:00Z', updatedAt: '2026-10-20T00:00:00Z' };
    persisted = [row]; return row;
  });
});
afterEach(() => { cleanup(); jest.restoreAllMocks(); });

it('saves a real API draft and resumes it after leaving the editor', async () => {
  show();
  fireEvent.click(await screen.findByRole('button', { name: 'Crear campaña' }));
  fireEvent.change(screen.getByLabelText('Título de la sesión'), { target: { value: 'Liquid Paper Girl · Live session' } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar borrador' }));
  expect(await screen.findByText(/Borrador guardado/)).toBeInTheDocument();
  expect(save).toHaveBeenCalledTimes(1);
  expect(save.mock.calls[0]?.[0]).toBe(51);
  expect(save.mock.calls[0]?.[3].status).toBe('draft');
  fireEvent.click(screen.getByRole('button', { name: 'Volver a campañas' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Continuar' }));
  expect(screen.getByLabelText('Título de la sesión')).toHaveValue('Liquid Paper Girl · Live session');
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
    title: 'Liquid Paper Girl · Live session', status: 'draft',
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
  expect(await screen.findByText(/Video publicado en el perfil de TDF/)).toBeInTheDocument();
  expect(feature).toHaveBeenCalledWith(51, persisted[0]!.id, 4);
});

it('reports a rejected public update without claiming publication or changing the review plan', async () => {
  const row = completeCampaign(); row.content.status = 'ready'; persisted = [row];
  feature.mockRejectedValueOnce(new Error('La campaña cambió. Recarga antes de publicar.'));
  await openReview();
  fireEvent.click(screen.getByRole('button', { name: 'Publicar video en el perfil de TDF' }));
  expect(await screen.findByText(/La campaña cambió/)).toBeInTheDocument();
  expect(screen.queryByText(/Video publicado en el perfil de TDF/)).not.toBeInTheDocument();
  expect(save).not.toHaveBeenCalled();
  expect(feature).toHaveBeenCalledWith(51, row.id, 3);
});
