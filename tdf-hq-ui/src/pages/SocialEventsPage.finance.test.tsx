import { jest } from '@jest/globals';
import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { SocialEventDTO } from '../api/socialEvents';

type ListState = { data?: unknown[]; error?: Error | null; isLoading?: boolean };
let budget: ListState;
let entries: ListState;
const event: SocialEventDTO = { eventId: '7', eventOrganizerPartyId: '42', eventTitle: 'Synthetic finance', eventStart: '2030-01-01T12:00:00Z', eventCurrency: 'USD', eventArtists: [] };
let summary = { efsEventId: '7', efsCurrency: 'USD', efsNetCents: 250 };
const download = jest.fn();
const createObjectURL = jest.fn(() => 'blob:synthetic');

jest.unstable_mockModule('@tanstack/react-query', () => ({
  useQueryClient: () => ({}),
  useMutation: () => ({ mutate: jest.fn(), isPending: false }),
  useQuery: ({ queryKey }: { queryKey: string[] }) => ({
    data: queryKey[0] === 'social-events' ? [event] : queryKey[1] === 'event-types' ? { items: [] } : [],
    isSuccess: true, isLoading: false, error: null,
  }),
  useQueries: ({ queries }: { queries: { queryKey: string[] }[] }) => queries.map(({ queryKey }) => {
    if (queryKey[0] === 'social-budget-lines') return budget;
    if (queryKey[0] === 'social-finance-entries') return entries;
    return { data: queryKey[0] === 'social-finance-summary' ? summary : [], error: null, isLoading: false };
  }),
}));
jest.unstable_mockModule('../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: '42', roles: [], modules: [] } }) }));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({ useLocalePreferences: () => ({ locale: 'en-US' }) }));
jest.unstable_mockModule('../analytics/useTicketFunnel', () => ({ useTicketFunnel: () => jest.fn() }));
jest.unstable_mockModule('../api/socialEvents', () => ({ SocialEventsAPI: {} }));
jest.unstable_mockModule('../api/contracts', () => ({ ContractsAPI: {} }));
jest.unstable_mockModule('../api/catalogs', () => ({ Catalogs: {} }));
jest.unstable_mockModule('../components/StripeCheckoutModal', () => ({ StripeCheckoutModal: () => null }));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ UserSelector: () => null }));
jest.unstable_mockModule('../components/events/EventRsvpControls', () => ({ default: () => null }));
jest.unstable_mockModule('../components/events/EventManualPaymentsPanel', () => ({ default: () => null }));
jest.unstable_mockModule('../components/events/EventTaxDocumentsPanel', () => ({ default: () => null }));
jest.unstable_mockModule('../components/RefundManagementPanel', () => ({ RefundManagementPanel: () => null }));
jest.unstable_mockModule('@mui/x-date-pickers/DateCalendar', () => ({ DateCalendar: () => null }));
const { default: SocialEventsPage } = await import('./SocialEventsPage');

describe('actual event finance export button', () => {
  beforeEach(() => {
    summary = { efsEventId: '7', efsCurrency: 'USD', efsNetCents: 250 };
    budget = { data: [], error: null };
    entries = { data: [], error: null };
    download.mockClear(); createObjectURL.mockClear();
    Object.defineProperty(URL, 'createObjectURL', { configurable: true, value: createObjectURL });
    Object.defineProperty(URL, 'revokeObjectURL', { configurable: true, value: jest.fn() });
    jest.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(download);
  });
  afterEach(() => { cleanup(); jest.restoreAllMocks(); });
  it.each(['budget', 'entries'])('blocks a failed %s query instead of exporting an empty substitute', field => {
    const state = { error: new Error('precisión') };
    if (field === 'budget') budget = state; else entries = state;
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    const button = screen.getByRole('button', { name: 'Exportar CSV' });
    expect((button as HTMLButtonElement).disabled).toBe(true);
    fireEvent.click(button);
    expect(createObjectURL).not.toHaveBeenCalled(); expect(download).not.toHaveBeenCalled();
  });
  it.each(['budget', 'entries'])('blocks a pending %s query', field => {
    if (field === 'budget') budget = { isLoading: true }; else entries = { isLoading: true };
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    expect((screen.getByRole('button', { name: 'Exportar CSV' }) as HTMLButtonElement).disabled).toBe(true);
  });
  it.each(['budget', 'entries'])('blocks stale data after a failed %s refetch', field => {
    if (field === 'budget') budget.error = new Error('precisión'); else entries.error = new Error('precisión');
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    expect((screen.getByRole('button', { name: 'Exportar CSV' }) as HTMLButtonElement).disabled).toBe(true);
  });
  it('shows a precision error and never downloads unsafe cached finance data', () => {
    summary = { ...summary, efsNetCents: 9007199254740992 };
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    fireEvent.click(screen.getByRole('button', { name: 'Exportar CSV' }));
    expect(screen.getByText(/excede la precisión/)).toBeTruthy();
    expect(createObjectURL).not.toHaveBeenCalled(); expect(download).not.toHaveBeenCalled();
  });
  it('exports when both lists successfully contain zero rows', () => {
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    const button = screen.getByRole('button', { name: 'Exportar CSV' });
    expect((button as HTMLButtonElement).disabled).toBe(false);
    fireEvent.click(button);
    expect(createObjectURL).toHaveBeenCalledTimes(1); expect(download).toHaveBeenCalledTimes(1);
  });
});
