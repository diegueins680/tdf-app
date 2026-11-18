import '@testing-library/jest-dom';
import { render, screen, within } from '@testing-library/react';
import PaymentAlternativesNotice from './PaymentAlternativesNotice';

it('shows all other supported methods, excluding duplicates, the failed method and unknown options', () => {
  render(<PaymentAlternativesNotice methods={['datafast', 'paypal', 'paypal', 'bank_transfer', 'not_enabled', 'toString']} failedMethod="datafast" />);
  const notice = within(screen.getByRole('status'));
  expect(notice.getAllByRole('listitem')).toHaveLength(2);
  expect(notice.getByText('PayPal')).toBeInTheDocument();
  expect(notice.getByText('Transferencia bancaria')).toBeInTheDocument();
  expect(notice.queryByText(/Datafast|not_enabled|toString/)).toBeNull();
  expect(notice.getByText(/Elige otra opción/)).toBeInTheDocument();
});

it('lists alternatives while requiring verification of an unresolved attempt', () => {
  render(<PaymentAlternativesNotice methods={['payphone_wallet', 'paypal']} failedMethod="paypal" blocked english />);
  const notice = within(screen.getByRole('status'));
  expect(notice.getByText('PayPhone app')).toBeInTheDocument();
  expect(notice.getByText(/Do not pay again while it is pending/)).toBeInTheDocument();
  expect(notice.queryByRole('button')).toBeNull();
});

it('does not carry methods from a previous purchase into an unavailable checkout', () => {
  const view = render(<PaymentAlternativesNotice methods={['paypal', 'bank_transfer']} failedMethod="paypal" />);
  expect(screen.getByText('Transferencia bancaria')).toBeInTheDocument();
  view.rerender(<PaymentAlternativesNotice methods={[]} failedMethod="paypal" />);
  expect(screen.queryByRole('listitem')).toBeNull();
  expect(screen.getByText(/No hay otro medio habilitado.*referencia de tu orden/)).toBeInTheDocument();
});
