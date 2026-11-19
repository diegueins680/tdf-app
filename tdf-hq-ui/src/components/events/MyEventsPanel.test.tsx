import '../../i18n';
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

import type { SocialEventDTO } from '../../api/socialEvents';
import { expectNoSeriousAccessibilityViolations } from '../../test/accessibility';

const listEventsMock = jest.fn<(opts?: { startAfter?: string; limit?: number }) => Promise<SocialEventDTO[]>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { listEvents: (opts?: { startAfter?: string; limit?: number }) => listEventsMock(opts) },
}));

const { default: MyEventsPanel } = await import('./MyEventsPanel');

const inTwoWeeks = new Date(Date.now() + 14 * 86_400_000).toISOString();
const event = (id: string, title: string, organizer: string | null, overrides: Partial<SocialEventDTO> = {}): SocialEventDTO => ({
  eventId: id,
  eventOrganizerPartyId: organizer,
  eventTitle: title,
  eventStart: inTwoWeeks,
  eventTimezone: 'America/Guayaquil',
  eventWorkflowStateNameEs: 'Publicado',
  eventArtists: [],
  ...overrides,
});

function renderPanel(partyId: string | number = 42) {
  const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  return render(
    <MemoryRouter>
      <QueryClientProvider client={queryClient}>
        <MyEventsPanel partyId={partyId} />
      </QueryClientProvider>
    </MemoryRouter>,
  );
}

describe('MyEventsPanel', () => {
  beforeEach(() => { listEventsMock.mockReset(); });
  afterEach(() => { cleanup(); });

  it('offers creation and management before the list resolves', () => {
    listEventsMock.mockReturnValue(new Promise(() => undefined));
    renderPanel();

    expect(screen.getByRole('link', { name: 'Crear evento' }).getAttribute('href')).toBe('/social/eventos/nuevo');
    expect(screen.getByRole('link', { name: 'Gestionar eventos' }).getAttribute('href')).toBe('/social/eventos');
    expect(screen.getByLabelText('Cargando tus eventos')).toBeTruthy();
  });

  it('lists only upcoming events the viewer organizes, with management links', async () => {
    listEventsMock.mockResolvedValue([
      event('141', 'PATCH CULTURE vol.1', '42'),
      event('200', 'Evento ajeno', '7'),
      event('201', 'Evento importado', null),
    ]);
    const view = renderPanel(42);

    await waitFor(() => expect(screen.getByText('PATCH CULTURE vol.1')).toBeTruthy());
    expect(screen.queryByText('Evento ajeno')).toBeNull();
    expect(screen.queryByText('Evento importado')).toBeNull();
    expect(screen.getByText('Publicado')).toBeTruthy();
    expect(screen.getByRole('link', { name: 'Gestionar PATCH CULTURE vol.1' }).getAttribute('href')).toBe('/social/eventos/141');
    expect(screen.getByRole('link', { name: 'Logística de PATCH CULTURE vol.1' }).getAttribute('href'))
      .toBe('/social/eventos/141/logistica');
    const request = listEventsMock.mock.calls[0]?.[0];
    expect(request?.limit).toBe(500);
    expect(Number.isNaN(Date.parse(request?.startAfter ?? ''))).toBe(false);
    await expectNoSeriousAccessibilityViolations(view.container);
  });

  it('previews three events and points to the rest', async () => {
    listEventsMock.mockResolvedValue(
      Array.from({ length: 5 }, (_, index) => event(String(300 + index), `Fecha ${index + 1}`, '42')),
    );
    renderPanel('42');

    await waitFor(() => expect(screen.getByText('Fecha 1')).toBeTruthy());
    expect(screen.getByText('Fecha 3')).toBeTruthy();
    expect(screen.queryByText('Fecha 4')).toBeNull();
    expect(screen.getByText('Y 2 más en Eventos.')).toBeTruthy();
  });

  it('invites the viewer to create a first event when none are upcoming', async () => {
    listEventsMock.mockResolvedValue([event('200', 'Evento ajeno', '7')]);
    renderPanel();

    await waitFor(() => expect(screen.getByText('No organizas eventos próximos. Crea el primero en minutos.')).toBeTruthy());
  });

  it('keeps the actions and allows a retry when the list fails', async () => {
    listEventsMock.mockRejectedValueOnce(new Error('offline'));
    listEventsMock.mockResolvedValue([event('141', 'PATCH CULTURE vol.1', '42')]);
    renderPanel();

    await waitFor(() => expect(screen.getByRole('alert')).toBeTruthy());
    expect(screen.getByRole('link', { name: 'Crear evento' })).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
    await waitFor(() => expect(screen.getByText('PATCH CULTURE vol.1')).toBeTruthy());
  });
});
