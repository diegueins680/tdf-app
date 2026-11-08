import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState } from 'react';
import type { SocialArtistDTO, SocialEventDTO } from '../../api/socialEvents';

let currentParty = 42;
const listArtists = jest.fn<(options?: { name?: string; signal?: AbortSignal }) => Promise<SocialArtistDTO[]>>();
const getArtist = jest.fn<(id: string, signal?: AbortSignal) => Promise<SocialArtistDTO>>();
const getEvent = jest.fn<(id: string) => Promise<SocialEventDTO>>();
const updateEvent = jest.fn<(id: string, event: SocialEventDTO) => Promise<SocialEventDTO>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({ SocialEventsAPI: { listArtists, getArtist, getEvent, updateEvent } }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: currentParty } }) }));
const { default: EventArtistSelector } = await import('./EventArtistSelector');
const { default: EventLineupEditor } = await import('./EventLineupEditor');
const initialArtists = Array.from({ length: 50 }, (_, index) => ({ artistId: String(index + 1), artistName: `Artist ${index + 1}` }));
const beyond: SocialArtistDTO = { artistId: '51', artistName: 'Beyond initial fifty' };
const event: SocialEventDTO = { eventId: '21', eventTitle: 'Synthetic event', eventStart: '2099-01-01T12:00:00Z', eventArtists: [] };
let client: QueryClient;
function FilterControl({ initial }: { initial: string[] }) {
  const [ids, setIds] = useState(initial);
  return <><EventArtistSelector artistIds={[...ids]} onChange={setIds} /><output data-testid="selected-ids">{JSON.stringify(ids)}</output>
    <button onClick={() => setIds(['8'])}>External selection</button></>;
}
function setup(initial: string[], lineup = false) {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(<QueryClientProvider client={client}>{lineup
    ? <EventLineupEditor event={{ ...event, eventArtists: initial.map(id => ({ artistId: id, artistName: `Artist ${id}` })) }} ownerId={42} />
    : <FilterControl initial={initial} />}</QueryClientProvider>);
}
beforeEach(() => {
  currentParty = 42;
  jest.clearAllMocks();
  listArtists.mockResolvedValue(initialArtists);
  getArtist.mockImplementation(async id => ({ artistId: id, artistName: `Artist ${id}` }));
  getEvent.mockResolvedValue(event);
  updateEvent.mockImplementation(async (_id, value) => value);
});
afterEach(() => { cleanup(); client?.clear(); });

for (const lineup of [false, true]) for (const initial of [[], ['7']]) {
  it(`keeps real MUI search through async results and selects beyond the initial fifty (${lineup ? 'lineup' : 'filter'}, ${initial.length ? 'selected' : 'empty'})`, async () => {
    let resolveSearch!: (artists: SocialArtistDTO[]) => void;
    listArtists.mockImplementation(options => options?.name ? new Promise(resolve => { resolveSearch = resolve; }) : Promise.resolve(initialArtists));
    setup(initial, lineup);
    const input = screen.getByRole('combobox', { name: lineup ? 'Artistas que se presentan' : 'Artistas' });
    await waitFor(() => expect(listArtists).toHaveBeenCalled());
    if (initial.length) await screen.findByText('Artist 7');
    act(() => input.focus());
    fireEvent.change(input, { target: { value: 'Beyond' } });
    expect(input.value).toBe('Beyond');
    await waitFor(() => expect(listArtists).toHaveBeenCalledWith(expect.objectContaining({ name: 'Beyond' })));
    expect(input.value).toBe('Beyond');
    await act(async () => resolveSearch([beyond]));
    expect(input.value).toBe('Beyond');
    fireEvent.click(await screen.findByRole('option', { name: 'Beyond initial fifty · #51' }));
    if (lineup) {
      fireEvent.click(screen.getByRole('button', { name: 'Guardar artistas' }));
      await screen.findByText('Artistas guardados.');
      expect(updateEvent).toHaveBeenCalledWith('21', expect.objectContaining({ eventArtists: [...initial, '51'].map(id => ({ artistId: id, artistName: '' })) }));
    } else {
      expect(screen.getByTestId('selected-ids').textContent).toBe(JSON.stringify([...initial, '51']));
    }
    expect(input.value).toBe('');
    act(() => input.focus());
    fireEvent.click(screen.getByLabelText('Clear'));
    if (!lineup) expect(screen.getByTestId('selected-ids').textContent).toBe('[]');
    expect(input.value).toBe('');
  });
}

it('retains typed input when selected names resolve and supports controlled external values', async () => {
  let resolveSelected!: (artist: SocialArtistDTO) => void;
  listArtists.mockResolvedValue([]);
  getArtist.mockImplementation(id => id === '7' ? new Promise(resolve => { resolveSelected = resolve; }) : Promise.resolve({ artistId: id, artistName: `Artist ${id}` }));
  setup(['7']);
  const input = screen.getByRole('combobox', { name: 'Artistas' });
  await waitFor(() => expect(resolveSelected).toBeDefined());
  act(() => input.focus()); fireEvent.change(input, { target: { value: 'Beyond' } });
  expect(input.value).toBe('Beyond');
  await act(async () => resolveSelected({ artistId: '7', artistName: 'Authorized resolved seven' }));
  expect(input.value).toBe('Beyond');
  await screen.findByText('Authorized resolved seven');
  fireEvent.click(screen.getByRole('button', { name: 'External selection' }));
  await screen.findByText('Artist 8');
  expect(screen.queryByText('Authorized resolved seven')).toBeNull();
  expect(screen.getByTestId('selected-ids').textContent).toBe('["8"]');
});

it('retains the fifty-selection cap while searching later canonical IDs', async () => {
  listArtists.mockResolvedValue([beyond]);
  setup(initialArtists.map(artist => artist.artistId));
  const input = screen.getByRole('combobox', { name: 'Artistas' });
  act(() => input.focus()); fireEvent.change(input, { target: { value: 'Beyond' } });
  const option = await screen.findByRole('option', { name: 'Beyond initial fifty · #51' });
  expect(option.getAttribute('aria-disabled')).toBe('true');
  fireEvent.keyDown(input, { key: 'ArrowDown' });
  fireEvent.keyDown(input, { key: 'Enter' });
  expect(JSON.parse(screen.getByTestId('selected-ids').textContent)).toHaveLength(50);
});


it('does not expose a retired owner’s late artist details after the session query scope changes', async () => {
  let resolveOld!: (artist: SocialArtistDTO) => void;
  let oldSignal!: AbortSignal;
  listArtists.mockResolvedValue([]);
  getArtist.mockImplementation((_id, signal) => currentParty === 42
    ? new Promise(resolve => { resolveOld = resolve; oldSignal = signal!; })
    : Promise.resolve({ artistId: '7', artistName: 'Current authorized artist' }));
  const view = setup(['7']);
  await waitFor(() => expect(resolveOld).toBeDefined());
  currentParty = 43;
  view.rerender(<QueryClientProvider client={client}><FilterControl initial={['7']} /></QueryClientProvider>);
  await screen.findByText('Current authorized artist');
  expect(oldSignal.aborted).toBe(true);
  await act(async () => resolveOld({ artistId: '7', artistName: 'Retired private artist' }));
  expect(screen.queryByText('Retired private artist')).toBeNull();
  expect(screen.getByText('Current authorized artist')).toBeTruthy();
});
