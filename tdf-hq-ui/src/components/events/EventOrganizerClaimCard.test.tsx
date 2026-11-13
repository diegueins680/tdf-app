/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { ApiError } from '../../api/client';
import type { EventOrganizerClaimDTO, EventOrganizerClaimStateDTO } from '../../api/socialEvents';

const stateMock = jest.fn<(eventId: string) => Promise<EventOrganizerClaimStateDTO>>();
const submitMock = jest.fn<(eventId: string, evidence: string) => Promise<EventOrganizerClaimDTO>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: {
    getEventOrganizerClaim: (eventId: string) => stateMock(eventId),
    submitEventOrganizerClaim: (eventId: string, evidence: string) => submitMock(eventId, evidence),
  },
}));

const { default: EventOrganizerClaimCard } = await import('./EventOrganizerClaimCard');

const evidence = 'Organizo este evento desde 2024; el afiche oficial lo confirma.';
const claim = (status: EventOrganizerClaimDTO['eocStatus']): EventOrganizerClaimDTO => ({
  eocId: '4', eocEventId: '121', eocStatus: status, eocSubmittedAt: '2026-11-13T10:00:00Z',
});
const claimable = (current: EventOrganizerClaimDTO | null = null): EventOrganizerClaimStateDTO => ({
  eocsEventId: '121', eocsClaimable: true, eocsClaim: current,
});

const renderCard = () => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  return render(
    <QueryClientProvider client={client}>
      <EventOrganizerClaimCard eventId="121" accountScope="account:9" formatDate={(value) => value.slice(0, 10)} />
    </QueryClientProvider>,
  );
};
const evidenceField = () => screen.findByRole<HTMLTextAreaElement>('textbox', { name: 'Pruebas de que organizas este evento' });
const submitButton = () => screen.getByRole<HTMLButtonElement>('button', { name: 'Solicitar administración del evento' });

describe('EventOrganizerClaimCard', () => {
  beforeEach(() => {
    stateMock.mockReset().mockResolvedValue(claimable());
    submitMock.mockReset().mockResolvedValue(claim('submitted'));
  });

  it('requires enough evidence, submits it trimmed and then shows the claim under review', async () => {
    renderCard();
    const field = await evidenceField();
    expect(submitButton().disabled).toBe(true);
    fireEvent.change(field, { target: { value: 'Es mi evento' } });
    expect(submitButton().disabled).toBe(true);
    fireEvent.change(field, { target: { value: `  ${evidence}  ` } });
    fireEvent.click(submitButton());
    expect(await screen.findByText(/Solicitud registrada el 2026-11-13\. Está en revisión/)).toBeTruthy();
    expect(submitMock).toHaveBeenCalledTimes(1);
    expect(submitMock).toHaveBeenCalledWith('121', evidence);
    expect(screen.queryByRole('textbox')).toBeNull();
    expect(stateMock).toHaveBeenCalledWith('121');
  });

  it('shows a pending claim without offering a second submission', async () => {
    stateMock.mockResolvedValue(claimable(claim('submitted')));
    renderCard();
    expect(await screen.findByText(/Está en revisión/)).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Solicitar administración del evento' })).toBeNull();
  });

  it('lets a rejected claimant submit again', async () => {
    stateMock.mockResolvedValue(claimable(claim('rejected')));
    renderCard();
    expect(await screen.findByText('Tu solicitud anterior no fue aprobada. Puedes enviar una nueva con más pruebas.')).toBeTruthy();
    fireEvent.change(await evidenceField(), { target: { value: evidence } });
    fireEvent.click(submitButton());
    await waitFor(() => expect(submitMock).toHaveBeenCalledWith('121', evidence));
  });

  it('renders nothing once the event has an organizer', async () => {
    stateMock.mockResolvedValue({ eocsEventId: '121', eocsClaimable: false, eocsClaim: claim('superseded') });
    const view = renderCard();
    await waitFor(() => expect(view.container.querySelector('section')).toBeNull());
  });

  it('explains a refused submission, keeps the evidence and reloads the state', async () => {
    submitMock.mockRejectedValue(new ApiError('This event already has an organizer', 409));
    renderCard();
    const field = await evidenceField();
    fireEvent.change(field, { target: { value: evidence } });
    fireEvent.click(submitButton());
    expect(await screen.findByText(/el evento ya tiene quien lo organice o tienes demasiadas solicitudes pendientes/)).toBeTruthy();
    expect(field.value).toBe(evidence);
    await waitFor(() => expect(stateMock).toHaveBeenCalledTimes(2));
  });

  it('offers a retry when the claim state cannot be loaded', async () => {
    stateMock.mockRejectedValueOnce(new ApiError('unavailable', 503));
    renderCard();
    expect(await screen.findByText('No se pudo cargar el estado de tu solicitud.')).toBeTruthy();
    expect(screen.queryByRole('textbox')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Reintentar' }));
    expect(await evidenceField()).toBeTruthy();
  });
});
