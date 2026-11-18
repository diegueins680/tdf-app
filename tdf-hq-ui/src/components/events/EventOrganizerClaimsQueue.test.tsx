/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { ApiError } from '../../api/client';
import type { EventOrganizerClaimReviewDTO } from '../../api/socialEvents';

const listMock = jest.fn<() => Promise<EventOrganizerClaimReviewDTO[]>>();
const decideMock = jest.fn<(claimId: string, decision: string, notes: string) => Promise<EventOrganizerClaimReviewDTO>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    listEventOrganizerClaims: () => listMock(),
    decideEventOrganizerClaim: (claimId: string, decision: string, notes: string) => decideMock(claimId, decision, notes),
  },
}));

const { default: EventOrganizerClaimsQueue } = await import('./EventOrganizerClaimsQueue');

const open: EventOrganizerClaimReviewDTO = {
  eocrId: '4', eocrEventId: '121', eocrEventTitle: 'Festival sintético', eocrEventStart: '2026-12-01T01:00:00Z',
  eocrEventHasOrganizer: false, eocrClaimantPartyId: '42', eocrClaimantName: 'Ana Pérez',
  eocrEvidence: '<script>pruebas enviadas</script> https://example.com/afiche', eocrStatus: 'submitted',
  eocrSubmittedAt: '2026-11-13T10:00:00Z',
};
const decided: EventOrganizerClaimReviewDTO = {
  ...open, eocrId: '3', eocrEventTitle: 'Concierto decidido', eocrStatus: 'rejected', eocrReviewerPartyId: '7',
  eocrReviewerNotes: 'Las pruebas no nombran al solicitante.', eocrReviewedAt: '2026-11-12T10:00:00Z',
};
const notes = 'Afiche oficial contrastado con el local.';

const renderQueue = () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 }, mutations: { retry: false } } });
  return render(
    <MemoryRouter>
      <QueryClientProvider client={client}><EventOrganizerClaimsQueue /></QueryClientProvider>
    </MemoryRouter>,
  );
};
const card = async (title: string) => await screen.findByRole('article', { name: `Solicitud sobre ${title}` });

describe('EventOrganizerClaimsQueue', () => {
  beforeEach(() => {
    listMock.mockReset().mockResolvedValue([open, decided]);
    decideMock.mockReset().mockResolvedValue({ ...open, eocrStatus: 'approved' });
  });

  it('shows the claimant and the evidence as text and links to the event', async () => {
    const view = renderQueue();
    const pending = await card('Festival sintético');
    expect(within(pending).getByText('Solicitante: Ana Pérez (cuenta 42)')).toBeTruthy();
    expect(within(pending).getByText('<script>pruebas enviadas</script> https://example.com/afiche')).toBeTruthy();
    expect(view.container.querySelector('script')).toBeNull();
    expect(within(pending).getByRole('link', { name: 'Festival sintético' }).getAttribute('href')).toBe('/social/eventos/121');
    expect(within(pending).getByText('Pendiente')).toBeTruthy();
  });

  it('requires review notes before a decision and sends the approval once', async () => {
    renderQueue();
    const pending = await card('Festival sintético');
    const approve = within(pending).getByRole<HTMLButtonElement>('button', { name: 'Aprobar y asignar el evento' });
    const reject = within(pending).getByRole<HTMLButtonElement>('button', { name: 'Rechazar' });
    expect([approve.disabled, reject.disabled]).toEqual([true, true]);
    fireEvent.change(within(pending).getByRole('textbox', { name: 'Notas de revisión' }), { target: { value: notes } });
    fireEvent.click(approve);
    await waitFor(() => expect(decideMock).toHaveBeenCalledWith('4', 'approved', notes));
    expect(decideMock).toHaveBeenCalledTimes(1);
    await waitFor(() => expect(listMock.mock.calls.length).toBeGreaterThan(1));
  });

  it('offers no decision on a decided claim and shows who decided and why', async () => {
    renderQueue();
    const closed = await card('Concierto decidido');
    expect(within(closed).queryByRole('button')).toBeNull();
    expect(within(closed).queryByRole('textbox')).toBeNull();
    expect(within(closed).getByText('Rechazada')).toBeTruthy();
    expect(within(closed).getByText(/por la cuenta 7: Las pruebas no nombran al solicitante\./)).toBeTruthy();
  });

  it('only allows rejecting a claim whose event already has an organizer', async () => {
    listMock.mockResolvedValue([{ ...open, eocrEventHasOrganizer: true }]);
    renderQueue();
    const pending = await card('Festival sintético');
    fireEvent.change(within(pending).getByRole('textbox', { name: 'Notas de revisión' }), { target: { value: notes } });
    expect(within(pending).getByRole<HTMLButtonElement>('button', { name: 'Aprobar y asignar el evento' }).disabled).toBe(true);
    fireEvent.click(within(pending).getByRole('button', { name: 'Rechazar' }));
    await waitFor(() => expect(decideMock).toHaveBeenCalledWith('4', 'rejected', notes));
  });

  it('explains a refused self-review and a stale decision', async () => {
    decideMock.mockRejectedValueOnce(new ApiError('Claim review requires a distinct administrator', 403));
    renderQueue();
    const pending = await card('Festival sintético');
    fireEvent.change(within(pending).getByRole('textbox', { name: 'Notas de revisión' }), { target: { value: notes } });
    fireEvent.click(within(pending).getByRole('button', { name: 'Aprobar y asignar el evento' }));
    expect(await screen.findByText(/persona administradora distinta de quien la envió/)).toBeTruthy();
    decideMock.mockRejectedValueOnce(new ApiError('This organizer claim was already decided', 409));
    const refreshed = await card('Festival sintético');
    fireEvent.click(within(refreshed).getByRole('button', { name: 'Rechazar' }));
    expect(await screen.findByText(/ya fue decidida.*Se actualizó la cola\./)).toBeTruthy();
  });

  it('reports an empty queue and a refused queue', async () => {
    listMock.mockResolvedValueOnce([]);
    const empty = renderQueue();
    expect(await screen.findByText('No hay solicitudes sobre eventos sin organizador.')).toBeTruthy();
    empty.unmount();
    listMock.mockRejectedValueOnce(new ApiError('Organizer claims are reviewed by strict administrators', 403));
    renderQueue();
    expect(await screen.findByText('No tienes acceso a esta cola o no se pudo cargar.')).toBeTruthy();
  });
});
