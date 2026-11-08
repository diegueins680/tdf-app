import { jest } from '@jest/globals';
import { render, screen, fireEvent, cleanup } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import type { SocialEventDTO, SocialEventFinanceEntryDTO } from '../api/socialEvents';

interface ListState { data?: unknown[]; error?: Error | null; isLoading?: boolean }
let budget: ListState;
let entries: ListState;
const event: SocialEventDTO = { eventId: '7', eventOrganizerPartyId: '42', eventTitle: 'Synthetic finance', eventStart: '2030-01-01T12:00:00Z', eventCurrency: 'USD', eventArtists: [] };
let displayedEvent = event;
let summary = { efsEventId: '7', efsCurrency: 'USD', efsNetCents: 250 };
const download = jest.fn();
const createObjectURL = jest.fn<(blob: Blob) => string>(() => 'blob:synthetic');

jest.unstable_mockModule('@tanstack/react-query', () => ({
  useQueryClient: () => ({}),
  useMutation: () => ({ mutate: jest.fn(), isPending: false }),
  useQuery: ({ queryKey }: { queryKey: string[] }) => ({
    data: queryKey[0] === 'social-events' ? [displayedEvent] : queryKey[1] === 'event-types' ? { items: [] } : [],
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

const readCsvBlob = (blob: Blob) => new Promise<string>((resolve, reject) => {
  const reader = new FileReader();
  reader.onload = () => {
    if (typeof reader.result === 'string') resolve(reader.result);
    else reject(new Error('Expected CSV text from readAsText'));
  };
  reader.onerror = () => reject(new Error(reader.error?.message ?? 'Could not read CSV Blob'));
  reader.readAsText(blob);
});

const parseCsv = (text: string) => {
  const rows: string[][] = [];
  let row: string[] = [];
  let cell = '';
  let quoted = false;
  for (let index = 0; index < text.length; index += 1) {
    const character = text[index];
    if (character === '"') {
      if (quoted && text[index + 1] === '"') {
        cell += '"';
        index += 1;
      } else quoted = !quoted;
    } else if (character === ',' && !quoted) {
      row.push(cell);
      cell = '';
    } else if (character === '\n' && !quoted) {
      rows.push([...row, cell]);
      row = [];
      cell = '';
    } else cell += character;
  }
  rows.push([...row, cell]);
  return rows;
};

describe('actual event finance export button', () => {
  beforeEach(() => {
    displayedEvent = event;
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
    const button = screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' });
    expect(button.disabled).toBe(true);
    fireEvent.click(button);
    expect(createObjectURL).not.toHaveBeenCalled(); expect(download).not.toHaveBeenCalled();
  });
  it.each(['budget', 'entries'])('blocks a pending %s query', field => {
    if (field === 'budget') budget = { isLoading: true }; else entries = { isLoading: true };
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' }).disabled).toBe(true);
  });
  it.each(['budget', 'entries'])('blocks stale data after a failed %s refetch', field => {
    if (field === 'budget') budget.error = new Error('precisión'); else entries.error = new Error('precisión');
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' }).disabled).toBe(true);
  });
  it('shows a precision error and never downloads unsafe cached finance data', () => {
    summary = { ...summary, efsNetCents: 9007199254740992 };
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    fireEvent.click(screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' }));
    expect(screen.getByText(/excede la precisión/)).toBeTruthy();
    expect(createObjectURL).not.toHaveBeenCalled(); expect(download).not.toHaveBeenCalled();
  });
  it('exports when both lists successfully contain zero rows', () => {
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    const button = screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' });
    expect(button.disabled).toBe(false);
    fireEvent.click(button);
    expect(createObjectURL).toHaveBeenCalledTimes(1); expect(download).toHaveBeenCalledTimes(1);
  });
  it.each(['title', 'concept', 'reference'])('encodes formula-like %s in the actual download', async field => {
    const title = '=1+1';
    displayedEvent = { ...event, eventTitle: title };
    summary = { ...summary, efsNetCents: -250 };
    const entry: SocialEventFinanceEntryDTO = {
      efeId: '11', efeEventId: '7', efeDirection: 'expense', efeSource: 'manual',
      efeCategory: 'production', efeConcept: '+SUM(1,1)\n"quoted"', efeAmountCents: 125,
      efeCurrency: 'EUR', efeStatus: 'void', efeExternalRef: '@SUM(2,2)',
      efeOccurredAt: '2030-01-01T12:00:00Z',
    };
    entries = { data: [entry], error: null };
    render(<MemoryRouter><SocialEventsPage /></MemoryRouter>);
    fireEvent.click(screen.getByRole<HTMLButtonElement>('button', { name: 'Exportar CSV' }));
    expect(createObjectURL).toHaveBeenCalledTimes(1);
    expect(download).toHaveBeenCalledTimes(1);
    const csv = await readCsvBlob(createObjectURL.mock.calls[0]![0]);
    const rows = parseCsv(csv);
    const exportedEntry = rows.find(row => row[0] === 'finance_entry');
    // Each negative control reaches its own unsafe cell before other assertions.
    if (field === 'title') expect(rows[1]?.[2]).toBe(`'${title}`);
    if (field === 'concept') expect(exportedEntry?.[20]).toBe(`'${entry.efeConcept}`);
    if (field === 'reference') expect(exportedEntry?.[22]).toBe(`'${entry.efeExternalRef}`);
    expect(rows.every(row => row.length === 24)).toBe(true);
    expect(rows.slice(1).every(row => row[2] === `'${title}`)).toBe(true);
    expect(rows.find(row => row[0] === 'summary' && row[6] === 'net_cents')?.[7]).toBe('-250');
    expect(rows.filter(row => row[0] === 'summary').every(row => row[5] === 'USD')).toBe(true);
    expect(exportedEntry?.[20]).toBe(`'${entry.efeConcept}`);
    expect(exportedEntry?.[22]).toBe(`'${entry.efeExternalRef}`);
    expect([exportedEntry?.[5], exportedEntry?.[21]]).toEqual(['EUR', '125']);
    expect(csv).toContain('"\'+SUM(1,1)\n""quoted"""');
  });
});
