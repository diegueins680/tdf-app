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
jest.unstable_mockModule('../api/videoCampaigns', () => ({ VideoCampaigns: { list, save, metrics: async () => ({ registrations: 0, follows: 0 }) } }));
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
  persisted = [];
  session = { partyId: 51, username: 'artist', displayName: 'Artist', roles: ['Artist'] };
  list.mockImplementation(async () => persisted);
  save.mockReset().mockImplementation(async (artistId, id, revision, content) => {
    const row = { id, artistId, revision: revision + 1, content,
      createdAt: '2026-10-20T00:00:00Z', updatedAt: '2026-10-20T00:00:00Z' };
    persisted = [row]; return row;
  });
});
afterEach(cleanup);

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
