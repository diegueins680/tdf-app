/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';

const checkoutId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const attemptId = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const createMock = jest.fn();
const getMock = jest.fn();

jest.unstable_mockModule('../../api/providerPaymentSessions', () => ({
  HOSTED_PAYMENT_METHODS: [
    { label: 'placetopay_card', provider: 'placetopay', paymentMethod: 'card' },
    { label: 'placetopay_bank_redirect', provider: 'placetopay', paymentMethod: 'bank_redirect' },
    { label: 'placetopay_deuna_qr', provider: 'placetopay', paymentMethod: 'deuna_qr' },
    { label: 'payphone_wallet', provider: 'payphone', paymentMethod: 'payphone_wallet' },
  ],
  createProviderPaymentSession: createMock,
  getProviderPaymentSession: getMock,
  safePlaceToPayRedirect: (value?: string | null) => value?.startsWith('https://checkout-test.placetopay.ec/') ? value : null,
}));

const { default: HostedProviderCheckout } = await import('./HostedProviderCheckout');

const context = {
  checkoutId,
  lookupToken: 'secure-checkout-lookup-token',
  returnPath: '/eventos/7/orden/9',
};

describe('HostedProviderCheckout', () => {
  beforeEach(() => {
    createMock.mockReset();
    getMock.mockReset();
    window.sessionStorage.clear();
  });

  it('shows only server-offered methods and redirects only to the allowlisted host', async () => {
    const navigate = jest.fn();
    createMock.mockResolvedValue({
      checkoutId,
      attemptId,
      operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
      provider: 'placetopay',
      state: 'requires_customer_action',
      externalId: 'request-7',
      redirectUrl: 'https://checkout-test.placetopay.ec/session/7',
      outcomeCertainty: 'ambiguous',
      canRetryOrFallback: false,
    });

    render(
      <HostedProviderCheckout
        checkout={context}
        offeredMethods={['placetopay_card']}
        navigateToProvider={navigate}
      />,
    );

    expect(screen.queryByRole('button', { name: /PayPhone/ })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));

    await waitFor(() => expect(navigate).toHaveBeenCalledWith(
      'https://checkout-test.placetopay.ec/session/7',
    ));
    expect(createMock).toHaveBeenCalledTimes(1);
    expect(window.location.href).not.toContain(context.lookupToken);
  });

  it('locks alternative methods when provider creation has an unknown result', async () => {
    createMock.mockRejectedValue(new Error('network interrupted'));
    const locks: boolean[] = [];
    render(
      <HostedProviderCheckout
        checkout={context}
        offeredMethods={['placetopay_card', 'payphone_wallet']}
        initialBuyerPhone="991234567"
        onSafetyLockChange={(locked) => locks.push(locked)}
      />,
    );

    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));

    expect(await screen.findByText(/resultado del proveedor/)).toBeTruthy();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    expect(locks).toContain(true);
  });

  it('restores the exact pre-response lock after a reload and reuses its idempotency key', async () => {
    createMock.mockRejectedValue(new Error('connection closed after request transmission'));
    const first = render(
      <HostedProviderCheckout
        checkout={context}
        offeredMethods={['placetopay_card', 'payphone_wallet']}
        initialBuyerPhone="991234567"
      />,
    );

    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));
    expect(await screen.findByText(/resultado del proveedor/)).toBeTruthy();
    const firstIdempotencyKey = createMock.mock.calls[0]?.[2];
    first.unmount();

    render(
      <HostedProviderCheckout
        checkout={context}
        offeredMethods={['placetopay_card', 'payphone_wallet']}
        initialBuyerPhone="991234567"
      />,
    );

    expect(await screen.findByText(/resultado del proveedor/)).toBeTruthy();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));
    await waitFor(() => expect(createMock).toHaveBeenCalledTimes(2));
    expect(createMock.mock.calls[1]?.[2]).toBe(firstIdempotencyKey);
  });
});
