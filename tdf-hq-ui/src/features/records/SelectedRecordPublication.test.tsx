import { jest } from '@jest/globals';
import { act } from 'react';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { InteractionSummary } from '../../api/interactions';

const summary = jest.fn<(...args: unknown[]) => Promise<InteractionSummary>>();
const panel = jest.fn(({ kind, entityKey }: { kind: string; entityKey: string }) => <button>Discuss {kind} {entityKey}</button>);
jest.unstable_mockModule('../../api/interactions', () => ({ Interactions: { summary } }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: null }), getActiveSession: () => null, getStoredSessionToken: () => null }));
jest.unstable_mockModule('../interactions/InteractionPanel', () => ({ InteractionPanel: panel }));
const { SelectedRecordPublication } = await import('./SelectedRecordPublication');
const { usePublicationSelection } = await import('../interactions/PublicationSelection');
const { ApiError } = await import('../../api/client');
const requested = '91000000-0000-4000-8000-000000000201';
const boundedFeed = Array.from({ length: 200 }, (_, i) => `91000000-0000-4000-8000-${String(i).padStart(12, '0')}`);
let client: QueryClient;
function Source({ kind = 'recording', ids = boundedFeed, loading = false }: { kind?: 'recording' | 'recording_session' | 'record_release'; ids?: string[]; loading?: boolean }) {
  const selection = usePublicationSelection('item', ids, loading);
  return <SelectedRecordPublication kind={kind} selection={selection} loading={loading} />;
}
function view(props = {}, query = requested) {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<QueryClientProvider client={client}><MemoryRouter initialEntries={[`/records?item=${query}`]}><Source {...props} /></MemoryRouter></QueryClientProvider>);
}
beforeEach(() => {
  jest.clearAllMocks();
  Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: jest.fn() });
  summary.mockResolvedValue({ id: 'target', title: 'Published item beyond the preview', kind: 'recording', key: requested } as InteractionSummary);
});
afterEach(() => { cleanup(); client?.clear(); });
test.each(['recording', 'recording_session', 'record_release'] as const)('loads one authorized %s beyond the 200-item window and focuses its discussion', async (kind) => {
  view({ kind });
  const heading = await screen.findByRole('heading', { name: 'Published item beyond the preview' });
  await waitFor(() => expect(document.activeElement).toBe(heading.closest('[tabindex="-1"]')));
  expect(summary).toHaveBeenCalledTimes(1);
  expect(summary.mock.calls[0]?.slice(0, 2)).toEqual([{ kind, entityKey: requested }, false]);
  expect(screen.getByRole('button', { name: `Discuss ${kind} ${requested}` })).toBeTruthy();
  expect(panel.mock.calls[0]?.[0]).toMatchObject({ kind, entityKey: requested, initiallyExpanded: true });
});
test.each(['recording', 'recording_session'] as const)('provides a destination for an authorized %s without a media preview', async (kind) => {
  view({ kind, ids: [] }); await screen.findByRole('heading', { name: 'Published item beyond the preview' });
  expect(summary).toHaveBeenCalledTimes(1); expect(screen.queryByText(/ya no está disponible/)).toBeNull();
});
test.each([401, 403, 404])('does not mount a discussion or expose cached metadata after a %i denial', async (status) => {
  view(); await screen.findByRole('heading'); summary.mockRejectedValue(new ApiError('Unavailable', status));
  await act(async () => { await client.invalidateQueries({ queryKey: ['interactions'] }); });
  await screen.findByText(/ya no está disponible/);
  expect(screen.queryByRole('heading')).toBeNull(); expect(screen.queryByRole('button', { name: /Discuss/ })).toBeNull();
});
test('retries a failed single-item lookup without scanning the feed', async () => {
  summary.mockRejectedValueOnce(new Error('offline')); view();
  fireEvent.click(await screen.findByRole('button', { name: 'Reintentar' }));
  await screen.findByRole('heading'); expect(summary).toHaveBeenCalledTimes(2);
});
test.each([{ ids: [requested] }, { loading: true }])('keeps the ordinary grid path without duplicate item requests: %j', async (props) => {
  view(props); await act(async () => {}); expect(summary).not.toHaveBeenCalled(); expect(panel).not.toHaveBeenCalled();
});
test('rejects malformed source identifiers without any lookup', () => {
  view({}, 'not-an-id'); expect(summary).not.toHaveBeenCalled(); expect(screen.getByRole('status')).toBeTruthy();
});
