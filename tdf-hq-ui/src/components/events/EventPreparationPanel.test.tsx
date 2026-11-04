import { jest } from '@jest/globals';
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';
import type { SocialEventDTO } from '../../api/socialEvents';
import { advanceAuthSessionEpoch } from '../../session/authEvents';

const upload = jest.fn<() => Promise<{ eiuPublicUrl: string }>>();
const readEvent = jest.fn<() => Promise<SocialEventDTO>>();
jest.unstable_mockModule('../../api/socialEvents', () => ({ SocialEventsAPI: { uploadEventImage: upload, getEvent: readEvent } }));
const { default: EventPreparationPanel } = await import('./EventPreparationPanel');
const url = 'https://api.example.test/assets/psique.png';
const event: SocialEventDTO = { eventId: '216', eventTitle: 'Psique', eventStart: '2026-11-28T23:00:00Z', eventArtists: [] };
const file = () => new File(['poster'], 'psique.png', { type: 'image/png' });
function setup() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return render(<QueryClientProvider client={client}><MemoryRouter><EventPreparationPanel event={event} ownerId={9} /></MemoryRouter></QueryClientProvider>);
}
function savePoster(selected = file()) {
  fireEvent.change(screen.getByLabelText('Afiche del evento'), { target: { files: [selected] } });
  fireEvent.click(screen.getByRole('button', { name: 'Guardar afiche' }));
}
beforeEach(() => {
  upload.mockReset().mockResolvedValue({ eiuPublicUrl: url });
  readEvent.mockReset().mockResolvedValue({ ...event, eventImageUrl: url });
});
afterEach(cleanup);

it('confirms the poster attachment and exposes the preparation destinations', async () => {
  setup(); savePoster();
  await screen.findByText('Afiche guardado.');
  expect(upload).toHaveBeenCalledWith('216', expect.any(File));
  expect(readEvent).toHaveBeenCalledWith('216');
  expect(screen.getByRole('link', { name: 'Cronograma y equipo' }).getAttribute('href')).toBe('/social/eventos/216/logistica');
  // No campaign frontend is served for organizers yet, so no dead link is offered.
  expect(screen.queryByRole('link', { name: 'Abrir campañas' })).toBeNull();
});
it('confirms an attachment served from a supported http asset base', async () => {
  const httpUrl = 'http://localhost:8080/assets/psique.png';
  upload.mockResolvedValueOnce({ eiuPublicUrl: httpUrl });
  readEvent.mockResolvedValueOnce({ ...event, eventImageUrl: httpUrl });
  setup(); savePoster();
  await screen.findByText('Afiche guardado.');
  expect(upload).toHaveBeenCalledTimes(1);
});
it('rejects an upload URL with embedded credentials', async () => {
  upload.mockResolvedValueOnce({ eiuPublicUrl: 'https://user:secret@api.example.test/assets/psique.png' });
  setup(); savePoster();
  await screen.findByText(/No se pudo confirmar la carga/);
  expect(readEvent).not.toHaveBeenCalled();
});
it('retries verification without uploading the same file twice', async () => {
  readEvent.mockRejectedValueOnce(new Error('Conexión interrumpida'));
  setup(); savePoster();
  await screen.findByText('Conexión interrumpida');
  fireEvent.click(screen.getByRole('button', { name: 'Reintentar afiche' }));
  await screen.findByText('Afiche guardado.');
  expect(upload).toHaveBeenCalledTimes(1);
  expect(readEvent).toHaveBeenCalledTimes(2);
});
it('rejects an unconfirmed upload response and retains the selected file', async () => {
  upload.mockResolvedValueOnce({ eiuPublicUrl: '' });
  setup(); savePoster();
  await screen.findByText(/No se pudo confirmar la carga/);
  expect(readEvent).not.toHaveBeenCalled();
  expect(screen.getByRole<HTMLButtonElement>('button', { name: 'Reintentar afiche' }).disabled).toBe(false);
});
it.each(['session', 'unmount'])('stops late upload continuation after %s changes', async (boundary) => {
  let resolve!: (value: { eiuPublicUrl: string }) => void;
  upload.mockImplementationOnce(() => new Promise((done) => { resolve = done; }));
  const view = setup(); savePoster();
  await waitFor(() => expect(upload).toHaveBeenCalledTimes(1));
  if (boundary === 'session') advanceAuthSessionEpoch(); else view.unmount();
  await act(async () => { resolve({ eiuPublicUrl: url }); });
  expect(readEvent).not.toHaveBeenCalled();
  expect(screen.queryByText('Afiche guardado.')).toBeNull();
});
it('rejects an unsupported file before calling the server', async () => {
  setup(); savePoster(new File(['svg'], 'poster.svg', { type: 'image/svg+xml' }));
  await screen.findByText('Selecciona una imagen JPG, PNG o WebP.');
  expect(upload).not.toHaveBeenCalled();
});
