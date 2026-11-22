import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, useLocation } from 'react-router-dom';
import type { SocialEventDTO, SocialArtistDTO, SocialVenueDTO, SocialTicketTierDTO, SocialEventBudgetLineDTO, SocialEventFinanceEntryDTO } from '../api/socialEvents';

interface Filters { city?: string; artistIds?: string[]; startAfter?: string; limit?: number; offset?: number; signal?: AbortSignal }
const listEvents = jest.fn<(filters: Filters) => Promise<SocialEventDTO[]>>();
const createTicketTier = jest.fn<(id: string, body: SocialTicketTierDTO) => Promise<SocialTicketTierDTO>>();
const createBudgetLine = jest.fn<(id: string, body: SocialEventBudgetLineDTO) => Promise<SocialEventBudgetLineDTO>>();
const createFinanceEntry = jest.fn<(id: string, body: SocialEventFinanceEntryDTO) => Promise<SocialEventFinanceEntryDTO>>();
const createContract = jest.fn<(body: { amountCents: number; currency: string }) => Promise<{ id: string }>>();
const empty = jest.fn<() => Promise<never[]>>().mockResolvedValue([]);
const listArtistOptions = jest.fn<(options?: { name?: string; signal?: AbortSignal }) => Promise<SocialArtistDTO[]>>();
const getArtistOption = jest.fn<(id: string) => Promise<SocialArtistDTO>>();
const listVenues = jest.fn<() => Promise<SocialVenueDTO[]>>();
const getVenue = jest.fn<(venueId: string, signal?: AbortSignal) => Promise<SocialVenueDTO>>();
jest.unstable_mockModule('../api/socialEvents', () => ({ SocialEventsAPI: {
  createTicketTier, createBudgetLine, createFinanceEntry,
  listBudgetLines: empty, listFinanceEntries: empty, getFinanceSummary: async () => ({ efsEventId: '7', efsCurrency: 'EUR' }),
  listEvents, listVenues, getVenue, listInvitations: empty, listTicketTiers: empty, listTicketOrders: empty,
  listArtists: listArtistOptions,
  getArtist: getArtistOption,
} }));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 42, roles: [], modules: [] } }) }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'es' }) }));
jest.unstable_mockModule('../analytics/useTicketFunnel', () => ({ useTicketFunnel: () => jest.fn() }));
jest.unstable_mockModule('../api/contracts', () => ({ ContractsAPI: { create: createContract } }));
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

const event: SocialEventDTO = { eventId: '7', eventOrganizerPartyId: '42', eventTitle: 'Synthetic money inputs', eventStart: '2099-05-03T20:00:00Z', eventCurrency: 'EUR', eventArtists: [] };
let client: QueryClient;
function Location() { return <output data-testid="location">{useLocation().search}</output>; }
function mount(path = '/social/eventos') {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 5 * 60 * 1000 } } });
  return render(<MemoryRouter initialEntries={[path]}><QueryClientProvider client={client}><SocialEventsPage /><Location /></QueryClientProvider></MemoryRouter>);
}
beforeEach(() => {
  listEvents.mockReset().mockResolvedValue([event]);
  listArtistOptions.mockReset().mockResolvedValue([]);
  getArtistOption.mockReset();
  listVenues.mockReset().mockResolvedValue([]);
  getVenue.mockReset();
  createTicketTier.mockReset().mockImplementation(async (_id, body) => body);
  createBudgetLine.mockReset().mockImplementation(async (_id, body) => body);
  createFinanceEntry.mockReset().mockImplementation(async (_id, body) => body);
  createContract.mockReset().mockResolvedValue({ id: 'synthetic-contract' });
});
afterEach(() => { cleanup(); client?.clear(); });

async function ready() {
  mount();
  await screen.findByRole('button', { name: 'Crear contrato', exact: true });
}
const change = (input: HTMLElement, value: string) => fireEvent.change(input, { target: { value } });
const submit = (name: string) => fireEvent.click(screen.getByRole('button', { name, exact: true }));

it('creates a ticket tier from currency units, keeps free distinct from blank, and preserves quantity units', async () => {
  await ready();
  change(screen.getByLabelText('Nombre', { exact: true }), 'General');
  change(screen.getByLabelText('Cantidad', { exact: true }), '12');
  submit('Crear tier');
  await screen.findByText(/Ingresa un precio;/);
  expect(createTicketTier).not.toHaveBeenCalled();
  change(screen.getByLabelText(/^Precio \(/), '25,50');
  submit('Crear tier');
  await waitFor(() => expect(createTicketTier).toHaveBeenCalledWith('7', expect.objectContaining({ ticketTierPriceCents: 2550, ticketTierQuantityTotal: 12, ticketTierCurrency: 'USD' })));
  await screen.findByText('Tipo de ticket creado.');
  change(screen.getByLabelText('Nombre', { exact: true }), 'Gratis');
  change(screen.getByLabelText('Cantidad', { exact: true }), '1');
  change(screen.getByLabelText(/^Precio \(/), '0');
  submit('Crear tier');
  await waitFor(() => expect(createTicketTier).toHaveBeenLastCalledWith('7', expect.objectContaining({ ticketTierPriceCents: 0 })));
});

it('creates an event-currency budget in exact cents, including zero and the safe integer boundary', async () => {
  await ready();
  change(screen.getByLabelText('Nombre línea'), 'Producción');
  change(screen.getByLabelText('Plan (EUR)', { exact: true }), '90071992547409.91');
  submit('Agregar línea');
  await waitFor(() => expect(createBudgetLine).toHaveBeenCalledWith('7', expect.objectContaining({ eblPlannedCents: Number.MAX_SAFE_INTEGER })));
  await screen.findByText('Línea presupuestaria creada.');
  change(screen.getByLabelText('Nombre línea'), 'Sin prever');
  change(screen.getByLabelText('Plan (EUR)', { exact: true }), '0');
  submit('Agregar línea');
  await waitFor(() => expect(createBudgetLine).toHaveBeenLastCalledWith('7', expect.objectContaining({ eblPlannedCents: 0 })));
});

it('uses the same exact amount and selected currency for the contract and its accounting commitment', async () => {
  await ready();
  change(screen.getAllByLabelText('Concepto', { exact: true })[0]!, 'Sonido');
  change(screen.getAllByLabelText('Monto (EUR)', { exact: true })[0]!, '125.05');
  submit('Crear contrato');
  await waitFor(() => expect(createContract).toHaveBeenCalledWith(expect.objectContaining({ amountCents: 12505, currency: 'EUR' })));
  await waitFor(() => expect(createFinanceEntry).toHaveBeenCalledWith('7', expect.objectContaining({ efeAmountCents: 12505, efeCurrency: 'EUR', efeExternalRef: 'synthetic-contract' })));
});

it('posts finance currency units without rounding or changing its status', async () => {
  await ready();
  change(screen.getAllByLabelText('Concepto', { exact: true })[1]!, 'Iluminación');
  change(screen.getAllByLabelText('Monto (EUR)', { exact: true })[1]!, '17,09');
  submit('Registrar');
  await waitFor(() => expect(createFinanceEntry).toHaveBeenCalledWith('7', expect.objectContaining({ efeAmountCents: 1709, efeCurrency: 'EUR', efeStatus: 'posted' })));
  expect(createContract).not.toHaveBeenCalled();
});

it.each(['25.505', '-1', '90071992547409.92', '1e2'])('rejects %s in all four forms without any API mutation', async value => {
  await ready();
  change(screen.getByLabelText('Nombre', { exact: true }), 'General');
  change(screen.getByLabelText('Cantidad', { exact: true }), '12');
  change(screen.getByLabelText('Nombre línea'), 'Producción');
  screen.getAllByLabelText('Concepto', { exact: true }).forEach(input => change(input, 'Concepto sintético'));
  const inputs = [screen.getByLabelText(/^Precio \(/), screen.getByLabelText('Plan (EUR)'), ...screen.getAllByLabelText('Monto (EUR)')];
  inputs.forEach(input => { change(input, value); expect(input.getAttribute('aria-invalid')).toBe('true'); });
  for (const name of ['Crear tier', 'Agregar línea', 'Crear contrato', 'Registrar']) submit(name);
  await waitFor(() => expect(screen.getByRole('alert').textContent).toMatch(/precio/));
  expect(createTicketTier).not.toHaveBeenCalled();
  expect(createBudgetLine).not.toHaveBeenCalled();
  expect(createFinanceEntry).not.toHaveBeenCalled();
  expect(createContract).not.toHaveBeenCalled();
  inputs.forEach(input => expect((input as HTMLInputElement).value).toBe(value));
});

it('blocks blank budgets and zero-valued contract and finance entries', async () => {
  await ready();
  change(screen.getByLabelText('Nombre línea'), 'Producción');
  submit('Agregar línea');
  await screen.findByText(/Ingresa un presupuesto;/);
  screen.getAllByLabelText('Concepto', { exact: true }).forEach(input => change(input, 'Concepto sintético'));
  screen.getAllByLabelText('Monto (EUR)').forEach(input => change(input, '0'));
  submit('Crear contrato');
  await screen.findByText('El monto del contrato debe ser mayor que cero.');
  submit('Registrar');
  await screen.findAllByText('El monto debe ser mayor que cero.');
  expect(createBudgetLine).not.toHaveBeenCalled();
  expect(createFinanceEntry).not.toHaveBeenCalled();
  expect(createContract).not.toHaveBeenCalled();
});
