import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

import type { SocialArtistDTO, SocialEventDTO } from '../api/socialEvents';

const EVENT_TYPE_ID = '41000000-0000-4000-8000-000000000001';
const createEvent = jest.fn<(payload: SocialEventDTO) => Promise<SocialEventDTO>>();
const updateEvent = jest.fn<(eventId: string, payload: SocialEventDTO) => Promise<SocialEventDTO>>();
const uploadEventImage = jest.fn<(eventId: string, file: File, name?: string) => Promise<unknown>>();
const listArtists = jest.fn<() => Promise<SocialArtistDTO[]>>(async () => []);

jest.unstable_mockModule('../api/socialEvents', () => ({
  SocialEventsAPI: {
    createEvent, updateEvent, uploadEventImage, listArtists,
    listVenues: async () => [],
    createLogisticsMember: jest.fn(),
  },
}));
jest.unstable_mockModule('../api/catalogs', () => ({
  Catalogs: {
    listPublicBatch: async () => ({
      catalogs: [{
        catalog: { code: 'event-types' },
        items: [{ id: EVENT_TYPE_ID, name: 'Concierto' }],
        defaults: [{ scopeKind: 'social-event', scopeId: 'global', entityId: EVENT_TYPE_ID }],
      }],
    }),
  },
}));
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: { partyId: 42 }, loading: false }),
}));
jest.unstable_mockModule('../contexts/LocalePreferencesContext', () => ({
  useLocalePreferences: () => ({ locale: 'es', timezone: 'America/Guayaquil', currency: 'USD' }),
}));
jest.unstable_mockModule('../components/party-selector/PartySelector', () => ({ UserSelector: () => null }));
jest.unstable_mockModule('../components/events/TicketPricePreview', () => ({ default: () => null }));
jest.unstable_mockModule('../components/PageShell', () => ({
  default: ({ title, children }: { title: string; children: React.ReactNode }) => <main><h1>{title}</h1>{children}</main>,
}));

const { default: CollaborativeEventCreatorPage } = await import('./CollaborativeEventCreatorPage');

function renderPage() {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false }, mutations: { retry: false } } });
  return render(
    <MemoryRouter>
      <QueryClientProvider client={client}>
        <CollaborativeEventCreatorPage />
      </QueryClientProvider>
    </MemoryRouter>,
  );
}
const art = () => new File(['png'], 'afiche.png', { type: 'image/png' });
const chooseArt = (file: File) =>
  fireEvent.change(screen.getByTestId('event-art-input'), { target: { files: [file] } });

beforeEach(() => {
  window.localStorage.clear();
  jest.clearAllMocks();
  createEvent.mockImplementation(async (payload) => ({ ...payload, eventId: '42' }));
  updateEvent.mockImplementation(async (_eventId, payload) => payload);
  uploadEventImage.mockResolvedValue({});
});
afterEach(() => { cleanup(); });

describe('CollaborativeEventCreatorPage', () => {
  it('opens on the event art picker, ahead of every other field', () => {
    renderPage();
    const picker = screen.getByRole('button', { name: 'Subir el arte del evento' });
    const title = screen.getByRole('textbox', { name: /Nombre del evento/ });

    expect(picker.compareDocumentPosition(title) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(screen.getByTestId('event-art-input').getAttribute('accept')).toBe('image/jpeg,image/png,image/webp,image/gif');
  });

  it('rejects a file that is not an image and keeps the picker', () => {
    renderPage();
    chooseArt(new File(['%PDF'], 'afiche.pdf', { type: 'application/pdf' }));

    expect(screen.getByText('El arte debe ser una imagen JPG, PNG, WebP o GIF.')).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Subir el arte del evento' })).toBeTruthy();
  });

  it('creates the event with a newly named artist and uploads the chosen art', async () => {
    renderPage();
    const file = art();
    chooseArt(file);
    expect(screen.getByText('afiche.png')).toBeTruthy();
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre del evento/ }), { target: { value: 'Noche Modular' } });
    const lineup = screen.getByRole('combobox', { name: 'Artistas que se presentan' });
    fireEvent.change(lineup, { target: { value: 'Banda Invitada' } });
    fireEvent.keyDown(lineup, { key: 'Enter' });
    expect(screen.getByText('Banda Invitada · nuevo')).toBeTruthy();
    await waitFor(() => expect(screen.getByRole('button', { name: 'Concierto' }).getAttribute('aria-pressed')).toBe('true'));

    fireEvent.click(screen.getByRole('button', { name: 'Crear evento' }));

    await screen.findByRole('heading', { name: 'Evento creado' });
    expect(createEvent.mock.calls[0]?.[0]).toMatchObject({ eventTitle: 'Noche Modular', eventArtists: [] });
    expect(updateEvent.mock.calls[0]?.[0]).toBe('42');
    expect(updateEvent.mock.calls[0]?.[1].eventArtists).toEqual([{ artistName: 'Banda Invitada' }]);
    expect(uploadEventImage).toHaveBeenCalledWith('42', file, 'afiche.png');
    expect(screen.getByText('Arte cargado')).toBeTruthy();
    expect(screen.getByText('1 artista')).toBeTruthy();
  });

  it('keeps the event and offers retries when the art and the named artist are refused', async () => {
    updateEvent.mockRejectedValueOnce(new Error('eventArtists[].artistId is required'));
    uploadEventImage.mockRejectedValueOnce(new Error('Sin conexión'));
    renderPage();
    chooseArt(art());
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre del evento/ }), { target: { value: 'Noche Modular' } });
    const lineup = screen.getByRole('combobox', { name: 'Artistas que se presentan' });
    fireEvent.change(lineup, { target: { value: 'Banda Invitada' } });
    fireEvent.keyDown(lineup, { key: 'Enter' });
    await waitFor(() => expect(screen.getByRole('button', { name: 'Concierto' }).getAttribute('aria-pressed')).toBe('true'));
    fireEvent.click(screen.getByRole('button', { name: 'Crear evento' }));

    await screen.findByRole('heading', { name: 'Evento creado' });
    expect(screen.getByText(/no se pudo subir el arte: Sin conexión/)).toBeTruthy();
    expect(screen.getByText(/falta agregar a Banda Invitada/)).toBeTruthy();

    const retries = screen.getAllByRole('button', { name: 'Reintentar' });
    retries.forEach((retry) => fireEvent.click(retry));
    await waitFor(() => expect(screen.queryAllByRole('button', { name: 'Reintentar' })).toHaveLength(0));
    expect(uploadEventImage).toHaveBeenCalledTimes(2);
    expect(updateEvent).toHaveBeenCalledTimes(2);
    expect(screen.getByText('Arte cargado')).toBeTruthy();
  });
});
