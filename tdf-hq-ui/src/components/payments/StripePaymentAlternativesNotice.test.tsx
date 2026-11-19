import { jest } from '@jest/globals';
import { render, screen, waitFor } from '@testing-library/react';
import '@testing-library/jest-dom';

const retrieve = jest.fn<(...args: unknown[]) => Promise<Record<string, unknown>>>();
const stripe = { retrievePaymentIntent: retrieve };
jest.unstable_mockModule('@stripe/react-stripe-js', () => ({ useStripe: () => stripe }));
const { default: StripePaymentAlternativesNotice } = await import('./StripePaymentAlternativesNotice');
beforeEach(() => retrieve.mockReset());

it('lists the other methods enabled on the same declined PaymentIntent', async () => {
  retrieve.mockResolvedValue({ paymentIntent: { client_secret: 'original-secret', status: 'requires_payment_method', payment_method_types: ['card', 'link', 'us_bank_account'] } });
  render(<StripePaymentAlternativesNotice clientSecret="original-secret" failedMethod="card" />);
  await screen.findByText('Link · Stripe');
  expect(screen.getAllByRole('listitem').map((item) => item.textContent)).toEqual(['Link · Stripe', 'Cuenta bancaria de EE. UU. · Stripe']);
  expect(retrieve).toHaveBeenCalledWith('original-secret');
  expect(screen.queryByText(/No vuelvas a pagar/)).toBeNull();
});

it.each(['processing', 'succeeded', 'requires_action'])('does not offer another payment for %s', async (status) => {
  retrieve.mockResolvedValue({ paymentIntent: { client_secret: 'original-secret', status, payment_method_types: ['card', 'link'] } });
  render(<StripePaymentAlternativesNotice clientSecret="original-secret" failedMethod="card" english />);
  await waitFor(() => expect(retrieve).toHaveBeenCalledTimes(1));
  expect(screen.queryByRole('listitem')).toBeNull();
  expect(screen.getByText(/Verify the original payment/)).toBeInTheDocument();
});

it('does not invent an empty list of available methods when the lookup fails', async () => {
  retrieve.mockRejectedValue(new Error('offline'));
  render(<StripePaymentAlternativesNotice clientSecret="original-secret" failedMethod="card" />);
  expect(await screen.findByText(/No pudimos consultar otros medios/)).toBeInTheDocument();
  expect(screen.queryByText(/No hay otro medio habilitado/)).toBeNull();
});

it('does not expose payment methods from a different intent', async () => {
  retrieve.mockResolvedValue({ paymentIntent: { client_secret: 'another-secret', status: 'requires_payment_method', payment_method_types: ['card', 'link'] } });
  render(<StripePaymentAlternativesNotice clientSecret="original-secret" failedMethod="card" />);
  await waitFor(() => expect(retrieve).toHaveBeenCalledTimes(1));
  expect(screen.queryByRole('listitem')).toBeNull();
});
