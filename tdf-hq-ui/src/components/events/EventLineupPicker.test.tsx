import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState } from 'react';

import type { SocialArtistDTO } from '../../api/socialEvents';
import type { EventLineupArtistDraft } from '../../pages/collaborativeEventCreation';

const listArtists = jest.fn<(options?: { name?: string }) => Promise<SocialArtistDTO[]>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({ SocialEventsAPI: { listArtists } }));
jest.unstable_mockModule('../../session/SessionContext', () => ({ useSession: () => ({ session: { partyId: 42 } }) }));
const { default: EventLineupPicker } = await import('./EventLineupPicker');

let client: QueryClient;
function Harness({ initial = [] }: { initial?: EventLineupArtistDraft[] }) {
  const [lineup, setLineup] = useState(initial);
  return (
    <QueryClientProvider client={client}>
      <EventLineupPicker value={lineup} onChange={setLineup} />
      <output data-testid="lineup">{JSON.stringify(lineup)}</output>
    </QueryClientProvider>
  );
}
const lineup = () => JSON.parse(screen.getByTestId('lineup').textContent ?? '[]') as EventLineupArtistDraft[];
const type = (value: string) => {
  const input = screen.getByRole('combobox', { name: 'Artistas que se presentan' });
  act(() => input.focus());
  fireEvent.change(input, { target: { value } });
  return input;
};

beforeEach(() => {
  client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  listArtists.mockReset();
  listArtists.mockImplementation(async (options) => (options?.name?.toLowerCase().startsWith('ver')
    ? [{ artistId: '7', artistName: 'Vera' }]
    : []));
});
afterEach(() => { cleanup(); client.clear(); });

describe('EventLineupPicker', () => {
  it('selects an artist already on the platform by id', async () => {
    render(<Harness />);
    type('Ver');
    fireEvent.click(await screen.findByRole('option', { name: 'Vera' }));

    expect(lineup()).toEqual([{ artistId: '7', artistName: 'Vera' }]);
    // A partial match still allows adding the typed name as someone else.
    expect(screen.queryByRole('option', { name: /artista nuevo/ })).toBeNull();
  });

  it('offers to add a typed name that is not on the platform', async () => {
    render(<Harness />);
    type('  Los   Nuevos ');
    fireEvent.click(await screen.findByRole('option', { name: 'Agregar «Los Nuevos» como artista nuevo' }));

    expect(lineup()).toEqual([{ artistId: null, artistName: 'Los Nuevos' }]);
    expect(screen.getByText('Los Nuevos · nuevo')).toBeTruthy();
  });

  it('adds the typed name with Enter and does not repeat it', async () => {
    render(<Harness />);
    fireEvent.keyDown(type('Banda Invitada'), { key: 'Enter' });
    await waitFor(() => expect(lineup()).toEqual([{ artistId: null, artistName: 'Banda Invitada' }]));

    fireEvent.keyDown(type('banda invitada'), { key: 'Enter' });
    expect(lineup()).toHaveLength(1);
  });

  it('links the listed artist when Enter is pressed on their exact name', async () => {
    render(<Harness />);
    const input = type('vera');
    await screen.findByRole('option', { name: 'Vera' });
    expect(screen.queryByRole('option', { name: /artista nuevo/ })).toBeNull();
    fireEvent.keyDown(input, { key: 'Enter' });

    await waitFor(() => expect(lineup()).toEqual([{ artistId: '7', artistName: 'Vera' }]));
  });

  it('asks which artist is meant when several share the typed name', async () => {
    listArtists.mockResolvedValue([{ artistId: '7', artistName: 'Vera' }, { artistId: '9', artistName: 'VERA' }]);
    render(<Harness />);
    const input = type('vera');
    await waitFor(() => expect(screen.getAllByRole('option')).toHaveLength(2));
    fireEvent.keyDown(input, { key: 'Enter' });

    expect(await screen.findByText('Hay más de un artista llamado «vera». Elige el correcto en la lista.')).toBeTruthy();
    expect(lineup()).toEqual([]);
    expect((input as HTMLInputElement).value).toBe('vera');

    fireEvent.click(screen.getByRole('option', { name: 'VERA' }));
    expect(lineup()).toEqual([{ artistId: '9', artistName: 'VERA' }]);
    expect(screen.queryByText(/Hay más de un artista/)).toBeNull();
  });

  it('still accepts a new name when the artist search fails', async () => {
    listArtists.mockRejectedValue(new Error('offline'));
    render(<Harness />);
    await screen.findByRole('alert');
    fireEvent.keyDown(type('Banda Invitada'), { key: 'Enter' });

    await waitFor(() => expect(lineup()).toEqual([{ artistId: null, artistName: 'Banda Invitada' }]));
  });
});
