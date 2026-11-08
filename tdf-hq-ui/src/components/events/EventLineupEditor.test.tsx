import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { SocialEventDTO } from '../../api/socialEvents';
import { advanceAuthSessionEpoch } from '../../session/authEvents';

const read = jest.fn<() => Promise<SocialEventDTO>>();
const update = jest.fn<(id: string, event: SocialEventDTO) => Promise<SocialEventDTO>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({ SocialEventsAPI: { getEvent: read, updateEvent: update } }));
jest.unstable_mockModule('./EventArtistSelector', () => ({ default: ({ onChange }: { onChange: (ids: string[]) => void }) =>
  <button onClick={() => onChange(['7', '8'])}>Seleccionar artistas de prueba</button> }));
const { default: EventLineupEditor } = await import('./EventLineupEditor');
const event: SocialEventDTO = { eventId: '21', eventTitle: 'Evento', eventStart: '2099-01-01T12:00:00Z', eventArtists: [] };
function setup() {
  const client = new QueryClient({ defaultOptions: { mutations: { retry: false } } });
  return render(<QueryClientProvider client={client}><EventLineupEditor event={event} ownerId={42} /></QueryClientProvider>);
}
beforeEach(() => {
  read.mockReset().mockResolvedValue({ ...event, eventTitle: 'Título actualizado', eventImageUrl: 'https://example.test/new.png' });
  update.mockReset().mockResolvedValue(event);
});
afterEach(cleanup);
function save() {
  fireEvent.click(screen.getByRole('button', { name: 'Seleccionar artistas de prueba' }));
  fireEvent.click(screen.getByRole('button', { name: 'Guardar artistas' }));
}
it('saves canonical lineup IDs through the existing authorized event write and preserves unrelated metadata by omission', async () => {
  setup(); save();
  await screen.findByText('Artistas guardados.');
  expect(update).toHaveBeenCalledWith('21', expect.objectContaining({ eventTitle: 'Título actualizado', eventArtists: [{ artistId: '7', artistName: '' }, { artistId: '8', artistName: '' }] }));
  expect(update.mock.calls[0][1]).not.toHaveProperty('eventImageUrl');
  expect(update.mock.calls[0][1]).not.toHaveProperty('eventWorkflowStateId');
});
it.each(['session', 'unmount'])('stops a late event read after %s revocation', async boundary => {
  let resolve!: (value: SocialEventDTO) => void;
  read.mockImplementation(() => new Promise(done => { resolve = done; }));
  const view = setup(); save();
  await waitFor(() => expect(read).toHaveBeenCalled());
  if (boundary === 'session') advanceAuthSessionEpoch(); else view.unmount();
  await act(async () => resolve(event));
  expect(update).not.toHaveBeenCalled();
  expect(screen.queryByText('Artistas guardados.')).toBeNull();
});
it('retains the selected lineup and allows retry after a write failure', async () => {
  update.mockRejectedValueOnce(new Error('Conexión interrumpida'));
  setup(); save();
  await screen.findByText('Conexión interrumpida');
  fireEvent.click(screen.getByRole('button', { name: 'Guardar artistas' }));
  await screen.findByText('Artistas guardados.');
  expect(update.mock.calls[1][1].eventArtists.map(a => a.artistId)).toEqual(['7', '8']);
});
