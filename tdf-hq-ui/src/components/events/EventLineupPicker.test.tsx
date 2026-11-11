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
const expectLineup = (expected: EventLineupArtistDraft[]) => {
  expect(screen.getByTestId('lineup').textContent).toBe(JSON.stringify(expected));
};
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

    expectLineup([{ artistId: '7', artistName: 'Vera' }]);
    // A partial match still allows adding the typed name as someone else.
    expect(screen.queryByRole('option', { name: /artista nuevo/ })).toBeNull();
  });

  it('offers to add a typed name that is not on the platform', async () => {
    render(<Harness />);
    type('  Los   Nuevos ');
    fireEvent.click(await screen.findByRole('option', { name: 'Agregar «Los Nuevos» como artista nuevo' }));

    expectLineup([{ artistId: null, artistName: 'Los Nuevos' }]);
    expect(screen.getByText('Los Nuevos · nuevo')).toBeTruthy();
  });

  it('adds the typed name with Enter and does not repeat it', async () => {
    render(<Harness />);
    fireEvent.keyDown(type('Banda Invitada'), { key: 'Enter' });
    await waitFor(() => expectLineup([{ artistId: null, artistName: 'Banda Invitada' }]));

    fireEvent.keyDown(type('banda invitada'), { key: 'Enter' });
    expectLineup([{ artistId: null, artistName: 'Banda Invitada' }]);
  });

  it('links the listed artist when Enter is pressed on their exact name', async () => {
    render(<Harness />);
    const exactNameInput = type('vera');
    await screen.findByRole('option', { name: 'Vera' });
    expect(screen.queryByRole('option', { name: /artista nuevo/ })).toBeNull();
    fireEvent.keyDown(exactNameInput, { key: 'Enter' });

    await waitFor(() => expectLineup([{ artistId: '7', artistName: 'Vera' }]));
  });

  it('still accepts a new name when the artist search fails', async () => {
    listArtists.mockRejectedValue(new Error('offline'));
    render(<Harness />);
    await screen.findByRole('alert');
    fireEvent.keyDown(type('Banda Invitada'), { key: 'Enter' });

    await waitFor(() => expectLineup([{ artistId: null, artistName: 'Banda Invitada' }]));
  });
});
