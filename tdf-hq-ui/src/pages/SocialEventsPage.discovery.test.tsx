import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation } from 'react-router-dom';
import type { SocialEventDTO, SocialArtistDTO } from '../api/socialEvents';

interface Filters { city?: string; artistIds?: string[]; startAfter?: string; limit?: number; offset?: number; signal?: AbortSignal }
const listEvents = jest.fn<(filters: Filters) => Promise<SocialEventDTO[]>>();
const artists: SocialArtistDTO[] = [{ artistId: '7', artistName: 'Alpha' }, { artistId: '8', artistName: 'Beta' }];
const empty = jest.fn<() => Promise<never[]>>().mockResolvedValue([]);
const listedVenues = [{ venueId: 'v1', venueName: 'Sala Listada' }];
const getVenue = jest.fn<(venueId: string) => Promise<{ venueId: string; venueName: string }>>();
jest.unstable_mockModule('../api/socialEvents', () => ({ SocialEventsAPI: {
  listEvents, listVenues: async () => listedVenues, getVenue, listInvitations: empty, listTicketTiers: empty, listTicketOrders: empty,
  listArtists: async () => artists,
  getArtist: async (id: string) => artists.find(artist => artist.artistId === id),
} }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 42, roles: [], modules: [] } }) }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'es' }) }));
jest.unstable_mockModule('../analytics/useTicketFunnel', () => ({ useTicketFunnel: () => jest.fn() }));
jest.unstable_mockModule('../api/contracts', () => ({ ContractsAPI: {} }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: {
  listItems: async () => ({ items: [] }), getPublicWorkflowStates: async () => ({ states: [] }),
} }));
jest.unstable_mockModule('../components/StripeCheckoutModal', () => ({ StripeCheckoutModal: () => null }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ UserSelector: () => null }));
jest.unstable_mockModule('../components/events/EventRsvpControls', () => ({ default: () => null }));
jest.unstable_mockModule('../components/events/EventManualPaymentsPanel', () => ({ default: () => null }));
jest.unstable_mockModule('../components/events/EventTaxDocumentsPanel', () => ({ default: () => null }));
jest.unstable_mockModule('../components/RefundManagementPanel', () => ({ RefundManagementPanel: () => null }));
// Native calendar rendering is exercised by the browser suite. Jest's CJS
// picker entry cannot resolve this workspace's nested @mui/system peer.
jest.unstable_mockModule('@mui/x-date-pickers/DateCalendar', () => ({
  DateCalendar: ({ onChange }: { onChange: (value: { toISODate: () => string }) => void }) => (
    <button type="button" onClick={() => onChange({ toISODate: () => '2099-05-10' })}>Elegir 2099-05-10</button>
  ),
}));
const { default: SocialEventsPage } = await import('./SocialEventsPage');

const future = (id: number, artist = artists[0]): SocialEventDTO => ({ eventId: String(id), eventTitle: `Evento ${id}`, eventStart: '2099-05-03T20:00:00Z', eventArtists: [artist] });
let client: QueryClient;
function Location() { return <output data-testid="location">{useLocation().search}</output>; }
function mount(path = '/social/eventos') {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<MemoryRouter initialEntries={[path]}><QueryClientProvider client={client}><SocialEventsPage /><Location /></QueryClientProvider></MemoryRouter>);
}
beforeEach(() => {
  listEvents.mockReset().mockResolvedValue([]);
  getVenue.mockReset().mockImplementation(async venueId => ({ venueId, venueName: `Sala ${venueId}` }));
});
afterEach(() => { cleanup(); client?.clear(); });

it('keeps the past-events control available on an empty upcoming page and removes the time restriction', async () => {
  listEvents.mockImplementation(async filters => filters.startAfter ? [] : [{ ...future(1), eventStart: '2020-01-01T12:00:00Z' }]);
  mount('/social/eventos?city=New+York&artists=7,8');
  await screen.findByText(/No hay eventos que coincidan/);
  fireEvent.click(screen.getByRole('checkbox', { name: 'Incluir eventos pasados' }));
  await screen.findByText('Evento pasado');
  expect(listEvents).toHaveBeenLastCalledWith(expect.objectContaining({ city: 'New York', artistIds: ['7', '8'], startAfter: undefined, limit: 21, offset: 0, signal: expect.any(AbortSignal) }));
  expect(screen.getByTestId('location').textContent).toContain('past=1');
});

it('debounces the city request and retains server matches even when the selected date was today', async () => {
  listEvents.mockImplementation(async filters => filters.city === 'New York' ? [future(2)] : []);
  mount();
  await screen.findByText(/No hay eventos que coincidan/);
  fireEvent.change(screen.getByRole('textbox', { name: 'Ciudad' }), { target: { value: 'N' } });
  fireEvent.change(screen.getByRole('textbox', { name: 'Ciudad' }), { target: { value: 'New York' } });
  await screen.findByText('1 evento en esta página');
  expect(screen.getAllByText('Evento 2').length).toBeGreaterThan(0);
  expect(listEvents.mock.calls.some(([filters]) => filters.city === 'N')).toBe(false);
  expect(screen.queryByText(/Sin eventos programados para esta fecha/)).toBeNull();
});

it('selects several artists through autocomplete and keeps the OR selection in the request and URL', async () => {
  mount('/social/eventos?city=Quito');
  const input = screen.getByRole('combobox', { name: 'Artistas' });
  fireEvent.change(input, { target: { value: 'Alpha' } });
  fireEvent.click(await screen.findByRole('option', { name: 'Alpha · #7' }));
  await waitFor(() => expect(listEvents).toHaveBeenLastCalledWith(expect.objectContaining({ city: 'Quito', artistIds: ['7'] })));
  fireEvent.change(input, { target: { value: 'Beta' } });
  fireEvent.click(await screen.findByRole('option', { name: 'Beta · #8' }));
  await waitFor(() => expect(listEvents).toHaveBeenLastCalledWith(expect.objectContaining({ city: 'Quito', artistIds: ['7', '8'] })));
  expect(new URLSearchParams(screen.getByTestId('location').textContent ?? '').get('artists')).toBe('7,8');
  fireEvent.click(screen.getByRole('button', { name: 'Limpiar filtros' }));
  await waitFor(() => expect(listEvents).toHaveBeenLastCalledWith(expect.objectContaining({ city: undefined, artistIds: [] })));
});

it('paginates without presenting a loaded-page count as the total and restores shared filters', async () => {
  const rows = Array.from({ length: 23 }, (_value, index) => future(index + 1));
  listEvents.mockImplementation(async filters => rows.slice(filters.offset, (filters.offset ?? 0) + (filters.limit ?? 21)));
  mount('/social/eventos?city=New+York&artists=7&past=1');
  await screen.findByText('20 eventos en esta página');
  fireEvent.click(screen.getByRole('button', { name: 'Siguiente' }));
  await screen.findByText('3 eventos en esta página');
  expect(listEvents).toHaveBeenLastCalledWith(expect.objectContaining({ city: 'New York', artistIds: ['7'], startAfter: undefined, offset: 20 }));
  expect(screen.getByTestId('location').textContent).toContain('page=2');
});

it('does not let an older city response replace a newer search and distinguishes errors from no matches', async () => {
  let resolveOld!: (events: SocialEventDTO[]) => void;
  listEvents.mockImplementation(filters => filters.city === 'Old' ? new Promise(resolve => { resolveOld = resolve; }) : Promise.resolve([future(3)]));
  mount('/social/eventos?city=Old');
  await waitFor(() => expect(resolveOld).toBeDefined());
  fireEvent.change(screen.getByRole('textbox', { name: 'Ciudad' }), { target: { value: 'New York' } });
  await screen.findByText('1 evento en esta página');
  await act(async () => resolveOld([future(99)]));
  expect(screen.queryByText('Evento 99')).toBeNull();
  listEvents.mockRejectedValue(new Error('Offline'));
  fireEvent.click(screen.getByRole('button', { name: 'Refrescar' }));
  await screen.findByRole('alert', { name: '' });
  await screen.findByText('No pudimos cargar los eventos. Intenta de nuevo.');
  expect(screen.queryByText(/No hay eventos que coincidan/)).toBeNull();
});

it('keeps an explicitly selected date without events instead of snapping back to the first event date', async () => {
  listEvents.mockResolvedValue([future(4)]);
  mount('/social/eventos');
  await screen.findByText('1 evento en esta página');
  await waitFor(() => expect(screen.queryByText(/Sin eventos programados para esta fecha/)).toBeNull());
  fireEvent.click(screen.getByRole('button', { name: 'Elegir 2099-05-10' }));
  await screen.findByText(/Sin eventos programados para esta fecha/);
  await act(async () => { await Promise.resolve(); });
  expect(screen.getByText(/Sin eventos programados para esta fecha/)).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'Ver una fecha con eventos' }));
  await waitFor(() => expect(screen.queryByText(/Sin eventos programados para esta fecha/)).toBeNull());
});

it('resolves event venues missing from the bounded venue list by id', async () => {
  listEvents.mockResolvedValue([{ ...future(3), eventVenueId: 'v1' }, { ...future(4), eventVenueId: 'v900' }]);
  mount('/social/eventos?city=Quito');
  expect((await screen.findAllByText('Sala v900')).length).toBeGreaterThan(0);
  expect(screen.getAllByText('Sala Listada').length).toBeGreaterThan(0);
  expect(screen.queryByText('Venue por definir')).toBeNull();
  expect(getVenue.mock.calls.map(([venueId]) => venueId)).toEqual(['v900']);
});
