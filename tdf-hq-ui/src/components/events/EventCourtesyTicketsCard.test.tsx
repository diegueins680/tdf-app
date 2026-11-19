/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { ApiError } from '../../api/client';
import type { EventCourtesyTicketDTO, SocialTicketOrderDTO, SocialTicketTierDTO } from '../../api/socialEvents';

const issueMock = jest.fn<(eventId: string, payload: EventCourtesyTicketDTO) => Promise<SocialTicketOrderDTO>>();

jest.unstable_mockModule('../../api/socialEvents', () => ({
  SocialEventsAPI: { issueCourtesyTickets: (eventId: string, payload: EventCourtesyTicketDTO) => issueMock(eventId, payload) },
}));

const { default: EventCourtesyTicketsCard } = await import('./EventCourtesyTicketsCard');

const tier = (id: string, name: string, sold = 0): SocialTicketTierDTO => ({
  ticketTierId: id, ticketTierEventId: '121', ticketTierCode: name.toUpperCase(), ticketTierName: name,
  ticketTierPriceCents: 500, ticketTierCurrency: 'USD', ticketTierQuantityTotal: 100, ticketTierQuantitySold: sold, ticketTierActive: true,
});
const order = (codes: string[]): SocialTicketOrderDTO => ({
  ticketOrderId: '900', ticketOrderEventId: '121', ticketOrderQuantity: codes.length, ticketOrderFaceValueCents: 0,
  ticketOrderBuyerPlatformFeeCents: 0, ticketOrderOrganizerPlatformFeeCents: 0, ticketOrderAmountCents: 0,
  ticketOrderCurrency: 'USD', ticketOrderStatusValue: 'paid',
  ticketOrderTickets: codes.map((code, index) => ({ ticketId: String(index + 1), ticketCode: code, ticketStatus: 'issued' })),
});

const renderCard = (tiers: SocialTicketTierDTO[], tiersLoading = false) => {
  const client = new QueryClient({ defaultOptions: { queries: { retry: false, gcTime: 0 } } });
  const invalidate = jest.spyOn(client, 'invalidateQueries');
  const view = render(
    <QueryClientProvider client={client}>
      <EventCourtesyTicketsCard eventId="121" tiers={tiers} tiersLoading={tiersLoading} />
    </QueryClientProvider>,
  );
  return { ...view, invalidate };
};

describe('EventCourtesyTicketsCard', () => {
  beforeEach(() => {
    issueMock.mockReset().mockResolvedValue(order(['TDF-AAA-111', 'TDF-BBB-222']));
  });

  it('asks for a ticket tier first when the event has none', () => {
    renderCard([]);
    expect(screen.getByText(/Primero crea un tipo de ticket/)).toBeTruthy();
    expect(screen.queryByRole('button', { name: 'Emitir cortesía' })).toBeNull();
  });

  it('issues courtesy tickets for the chosen tier, shows their codes and refreshes cached availability', async () => {
    const { invalidate } = renderCard([tier('tier-general', 'General', 10), tier('tier-vip', 'VIP')]);
    const submit = screen.getByRole<HTMLButtonElement>('button', { name: 'Emitir cortesía' });
    expect(submit.disabled).toBe(true);
    fireEvent.mouseDown(screen.getByRole('combobox', { name: 'Tipo de entrada' }));
    fireEvent.click(await screen.findByRole('option', { name: 'VIP · 100 disponibles' }));
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre de quien recibe/ }), { target: { value: ' Ana Pérez ' } });
    fireEvent.change(screen.getByRole('textbox', { name: 'Correo (opcional)' }), { target: { value: 'ana@example.com' } });
    fireEvent.change(screen.getByRole('spinbutton', { name: 'Cantidad' }), { target: { value: '2' } });
    fireEvent.click(submit);

    const codes = await screen.findByRole('list', { name: 'Códigos de las entradas emitidas' });
    expect(issueMock).toHaveBeenCalledWith('121', {
      ectTierId: 'tier-vip', ectQuantity: 2, ectHolderName: 'Ana Pérez', ectHolderEmail: 'ana@example.com',
    });
    expect(within(codes).getByText('TDF-AAA-111')).toBeTruthy();
    expect(within(codes).getByText('TDF-BBB-222')).toBeTruthy();
    expect(screen.getByText('2 cortesías emitidas para Ana Pérez')).toBeTruthy();
    expect(screen.getByText('También enviamos los códigos a ana@example.com.')).toBeTruthy();
    for (const key of ['social-event-ticket-tiers', 'social-ticket-tiers', 'social-ticket-orders', 'public-event-ticket-storefront']) {
      expect(invalidate).toHaveBeenCalledWith({ queryKey: [key, '121'] });
    }
    expect(screen.getByRole<HTMLInputElement>('textbox', { name: /Nombre de quien recibe/ }).value).toBe('');
  });

  it('defaults to the first tier and sends a null email when none is given', async () => {
    issueMock.mockResolvedValue(order(['TDF-CCC-333']));
    renderCard([tier('tier-general', 'General')]);
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre de quien recibe/ }), { target: { value: 'Luis' } });
    fireEvent.click(screen.getByRole('button', { name: 'Emitir cortesía' }));
    expect(await screen.findByText('Cortesía emitida para Luis')).toBeTruthy();
    expect(issueMock).toHaveBeenCalledWith('121', { ectTierId: 'tier-general', ectQuantity: 1, ectHolderName: 'Luis', ectHolderEmail: null });
    expect(screen.getByText(/No indicaste un correo/)).toBeTruthy();
  });

  it.each(['0', '11', '1.5', ''])('refuses quantity "%s" before calling the server', async (quantity) => {
    renderCard([tier('tier-general', 'General')]);
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre de quien recibe/ }), { target: { value: 'Luis' } });
    fireEvent.change(screen.getByRole('spinbutton', { name: 'Cantidad' }), { target: { value: quantity } });
    fireEvent.click(screen.getByRole('button', { name: 'Emitir cortesía' }));
    expect(await screen.findByText('La cantidad debe estar entre 1 y 10.')).toBeTruthy();
    expect(issueMock).not.toHaveBeenCalled();
  });

  it('refuses a malformed email and explains a sold-out tier', async () => {
    renderCard([tier('tier-general', 'General')]);
    fireEvent.change(screen.getByRole('textbox', { name: /Nombre de quien recibe/ }), { target: { value: 'Luis' } });
    const email = screen.getByRole('textbox', { name: 'Correo (opcional)' });
    fireEvent.change(email, { target: { value: 'luis-at-example' } });
    fireEvent.click(screen.getByRole('button', { name: 'Emitir cortesía' }));
    expect(await screen.findByText('Escribe un correo válido o deja el campo vacío.')).toBeTruthy();
    expect(issueMock).not.toHaveBeenCalled();

    issueMock.mockRejectedValue(new ApiError('Not enough tickets available', 409));
    fireEvent.change(email, { target: { value: '' } });
    fireEvent.click(screen.getByRole('button', { name: 'Emitir cortesía' }));
    expect(await screen.findByText(/No quedan suficientes entradas disponibles/)).toBeTruthy();
    await waitFor(() => expect(screen.getByRole<HTMLInputElement>('textbox', { name: /Nombre de quien recibe/ }).value).toBe('Luis'));
  });
});
