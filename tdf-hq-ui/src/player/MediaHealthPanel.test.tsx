import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { SessionUser } from '../session/SessionContext';

interface Row { sourceKind: 'asset' | 'legacy'; sourceId: string; sourceVersion: string;
  status: 'valid' | 'invalid'; reason: string; checkedAt: null; nextCheckAt: string }
const row = (id: string, status: Row['status'] = 'valid'): Row => ({ sourceKind: 'legacy', sourceId: id,
  sourceVersion: 'version', status, reason: status === 'invalid' ? 'missing' : 'decodable', checkedAt: null, nextCheckAt: '2026-11-07T00:00:00Z' });
const getHealth = jest.fn<(url: string, options?: { signal?: AbortSignal }) => Promise<Row[]>>();
const revalidate = jest.fn<() => Promise<{ queued: boolean }>>();
let session: SessionUser | null;
jest.unstable_mockModule('../api/client', () => ({ get: getHealth, post: revalidate }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session }) }));
const { default: MediaHealthPanel } = await import('./MediaHealthPanel');
let client: QueryClient;
const show = () => render(<QueryClientProvider client={client}><MediaHealthPanel /></QueryClientProvider>);
beforeEach(() => {
  session = { partyId: 17, username: 'owner', displayName: 'Owner', roles: ['Artist'] };
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  getHealth.mockReset(); revalidate.mockReset().mockResolvedValue({ queued: true });
});
afterEach(() => { cleanup(); client.clear(); });

it('reaches an invalid source after a full page of valid sources and revalidates its exact identity', async () => {
  const first = Array.from({ length: 100 }, (_, i) => row(`${i + 1}:spotify`));
  getHealth.mockResolvedValueOnce(first).mockResolvedValueOnce([row('501:youtube', 'invalid')]);
  show();
  fireEvent.click(await screen.findByRole('button', { name: 'Cargar más archivos' }));
  await screen.findByText('legacy:501:youtube: Archivo no encontrado');
  expect(getHealth.mock.calls[0]?.[0]).toBe('/music/media-health?limit=100');
  expect(getHealth.mock.calls[1]?.[0]).toBe('/music/media-health?limit=100&afterKind=legacy&afterId=100%3Aspotify');
  expect(screen.queryByRole('button', { name: 'Cargar más archivos' })).toBeNull();
  fireEvent.click(screen.getByRole('button', { name: 'Comprobar de nuevo' }));
  await waitFor(() => expect(revalidate).toHaveBeenCalledWith('/music/media-health/legacy/501%3Ayoutube/revalidate', {}));
});

it('retains loaded diagnoses after a failed next page and offers a bounded retry', async () => {
  const first = Array.from({ length: 100 }, (_, i) => row(`${i + 1}:spotify`, i === 0 ? 'invalid' : 'valid'));
  getHealth.mockResolvedValueOnce(first).mockRejectedValueOnce(new Error('Offline')).mockResolvedValueOnce([]);
  show();
  await screen.findByText('legacy:1:spotify: Archivo no encontrado');
  fireEvent.click(await screen.findByRole('button', { name: 'Cargar más archivos' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Reintentar archivos' }));
  await waitFor(() => expect(getHealth).toHaveBeenCalledTimes(3));
  expect(screen.getByText('legacy:1:spotify: Archivo no encontrado')).toBeTruthy();
  expect(getHealth.mock.calls[2]?.[0]).toBe(getHealth.mock.calls[1]?.[0]);
});

it('discards the previous listener pages when the account changes', async () => {
  getHealth.mockResolvedValueOnce([row('17:spotify', 'invalid')]).mockResolvedValueOnce([]);
  const view = show();
  await screen.findByText('legacy:17:spotify: Archivo no encontrado');
  session = { partyId: 18, username: 'other', displayName: 'Other', roles: ['Artist'] };
  view.rerender(<QueryClientProvider client={client}><MediaHealthPanel /></QueryClientProvider>);
  await waitFor(() => expect(getHealth).toHaveBeenCalledTimes(2));
  expect(screen.queryByText('legacy:17:spotify: Archivo no encontrado')).toBeNull();
});
