/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';

import type { PublicEventTicketBankTransfer } from '../../api/eventTickets';
import TicketBankTransferPanel from './TicketBankTransferPanel';

const transfer = (overrides: Partial<PublicEventTicketBankTransfer> = {}): PublicEventTicketBankTransfer => ({
  instructions: 'Banco Internacional\nAhorros 440781141',
  paymentReference: 'TDF-31',
  amountMinor: 2000,
  currency: 'USD',
  evidenceStatus: 'awaiting_evidence',
  customerReference: null,
  reviewNotes: null,
  ...overrides,
});

const mount = (value: PublicEventTicketBankTransfer, onSubmit = jest.fn<(reference: string) => void>()) => {
  render(
    <TicketBankTransferPanel
      transfer={value}
      english={false}
      amountLabel="$20.00"
      holdExpiresLabel="miércoles 7 de octubre"
      busy={false}
      onSubmitReference={onSubmit}
    />,
  );
  return onSubmit;
};

afterEach(cleanup);

it('shows the server instructions, exact amount and reference before evidence', () => {
  const onSubmit = mount(transfer());
  expect(screen.getByText('TDF-31')).toBeTruthy();
  expect(screen.getByText('$20.00')).toBeTruthy();
  expect(screen.getByText(/Ahorros 440781141/)).toBeTruthy();

  const button = screen.getByRole('button', { name: 'Ya transferí' });
  expect((button as HTMLButtonElement).disabled).toBe(true);
  fireEvent.change(screen.getByLabelText(/comprobante/i), { target: { value: '  COMP-77 ' } });
  fireEvent.click(button);
  expect(onSubmit).toHaveBeenCalledWith('COMP-77');
});

it('never presents a reported transfer as paid and hides the form while it is reviewed', () => {
  mount(transfer({ evidenceStatus: 'under_review', customerReference: 'COMP-77' }));
  expect(screen.getByText(/Estamos verificando el depósito/)).toBeTruthy();
  expect(screen.queryByRole('button', { name: 'Ya transferí' })).toBeNull();
});

it('lets the buyer resubmit after a rejection and shows the staff reason', () => {
  mount(transfer({ evidenceStatus: 'rejected', reviewNotes: 'No encontramos el depósito.' }));
  expect(screen.getByText(/No encontramos el depósito/)).toBeTruthy();
  expect(screen.getByRole('button', { name: 'Ya transferí' })).toBeTruthy();
});
