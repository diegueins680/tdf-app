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
const { ApiError } = await import('../../api/client');
const {
  saveProviderPaymentPending, saveProviderPaymentResume, loadProviderPaymentPending,
  loadOrCreatePaymentIdempotencyKey, clearPaymentIdempotencyKey,
} = await import('../../utils/providerPaymentResume');

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

  const seedPending = (payphone = false) => {
    const provider = payphone ? 'payphone' : 'placetopay';
    const paymentMethod = payphone ? 'payphone_wallet' : 'card';
    const key = loadOrCreatePaymentIdempotencyKey(checkoutId, provider, paymentMethod);
    saveProviderPaymentPending({
      version: 1, ...context, provider, paymentMethod, createdAt: Date.now(),
      ...(payphone ? { buyerPhone: '991234567', buyerCountryCode: '593' } : {}),
    });
    return key;
  };

  const succeeded = {
    checkoutId, attemptId, operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
    provider: 'placetopay', state: 'succeeded', externalId: 'synthetic-recovery',
    outcomeCertainty: 'succeeded', canRetryOrFallback: false,
  };

  it('recovers a paid checkout even when no method is offered and new payment is disabled', async () => {
    const key = seedPending();
    const confirmed = jest.fn();
    createMock.mockResolvedValue(succeeded);
    render(<HostedProviderCheckout checkout={context} offeredMethods={[]} disabled
      onPaymentConfirmed={confirmed} />);
    expect(screen.queryByRole('button', { name: /Tarjeta · PlaceToPay/ })).toBeNull();
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    expect(await screen.findByText('El servidor verificó el pago del proveedor.')).toBeTruthy();
    expect(createMock).toHaveBeenCalledWith(checkoutId, context.lookupToken, key,
      { provider: 'placetopay', paymentMethod: 'card' });
    expect(confirmed).toHaveBeenCalledTimes(1);
    expect(createMock).toHaveBeenCalledTimes(1);
  });

  it('keeps a recovered closed-checkout approval on hold without success or a redirect', async () => {
    seedPending();
    const confirmed = jest.fn();
    const navigate = jest.fn();
    const locks: boolean[] = [];
    const held = {
      ...succeeded, state: 'ambiguous', outcomeCertainty: 'ambiguous',
      redirectUrl: null, canRetryOrFallback: false,
    };
    createMock.mockResolvedValue(held);
    getMock.mockResolvedValue(held);
    render(<HostedProviderCheckout checkout={context}
      offeredMethods={['placetopay_card', 'payphone_wallet']}
      onPaymentConfirmed={confirmed} navigateToProvider={navigate}
      onSafetyLockChange={(locked) => locks.push(locked)} />);

    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    expect(await screen.findByText(/No reintentes ni uses otro proveedor/)).toBeTruthy();
    expect(confirmed).not.toHaveBeenCalled();
    expect(navigate).not.toHaveBeenCalled();
    expect(screen.queryByText('El servidor verificó el pago del proveedor.')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Continuar en la página segura del proveedor' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Elegir otro método de pago' })).toBeNull();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    expect(locks).toContain(true);
    expect(createMock).toHaveBeenCalledTimes(1);
  });

  it('keeps the original PayPhone contact frozen during exact recovery', async () => {
    const key = seedPending(true);
    createMock.mockResolvedValue({ ...succeeded, provider: 'payphone' });
    render(<HostedProviderCheckout checkout={context} offeredMethods={['payphone_wallet']}
      initialBuyerPhone="000000000" />);
    const phone = await screen.findByRole('textbox', { name: 'Número PayPhone' });
    expect(phone.hasAttribute('disabled')).toBe(true);
    fireEvent.change(phone, { target: { value: '998888888' } });
    fireEvent.click(screen.getByRole('button', { name: 'Recuperar pago original' }));
    await waitFor(() => expect(createMock).toHaveBeenCalledWith(checkoutId, context.lookupToken, key,
      { provider: 'payphone', paymentMethod: 'payphone_wallet',
        buyerPhone: '991234567', buyerCountryCode: '593' }));
  });

  it('never mints a new request key when the original recovery key is missing', async () => {
    seedPending();
    clearPaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    const locks: boolean[] = [];
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']}
      onSafetyLockChange={(locked) => locks.push(locked)} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    expect(await screen.findByText(/contacta a soporte para conciliar esta orden/)).toBeTruthy();
    expect(createMock).not.toHaveBeenCalled();
    expect(loadProviderPaymentPending(checkoutId)).not.toBeNull();
    expect(locks.at(-1)).toBe(true);
  });

  it('does not release an unknown prior charge after a recovery lookup returns 404', async () => {
    seedPending();
    createMock.mockRejectedValue(new ApiError('Not found', 404));
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    await waitFor(() => expect(createMock).toHaveBeenCalledTimes(1));
    expect(await screen.findByText(/resultado del proveedor/)).toBeTruthy();
    expect(loadProviderPaymentPending(checkoutId)).not.toBeNull();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
  });

  it('restores a known attempt without requiring its provider in current offerings', async () => {
    saveProviderPaymentResume({ version: 1, ...context, attemptId, provider: 'placetopay',
      paymentMethod: 'card', createdAt: Date.now() });
    getMock.mockResolvedValue(succeeded);
    render(<HostedProviderCheckout checkout={context} offeredMethods={[]} disabled />);
    expect(await screen.findByText('El servidor verificó el pago del proveedor.')).toBeTruthy();
    expect(getMock).toHaveBeenCalledWith(checkoutId, attemptId, context.lookupToken);
    expect(createMock).not.toHaveBeenCalled();
  });

  it('does not recover an unrelated checkout from another product surface', () => {
    saveProviderPaymentResume({ version: 1, ...context, attemptId, provider: 'placetopay',
      paymentMethod: 'card', createdAt: Date.now() });
    render(<HostedProviderCheckout offeredMethods={[]} pendingReturnPathPrefix="/mezcla-mastering/pedido/" />);
    expect(getMock).not.toHaveBeenCalled();
    expect(createMock).not.toHaveBeenCalled();
    expect(screen.queryByRole('button', { name: 'Recuperar pago original' })).toBeNull();
  });

  it('ignores a late status response after navigation to a different checkout', async () => {
    saveProviderPaymentResume({ version: 1, ...context, attemptId, provider: 'placetopay',
      paymentMethod: 'card', createdAt: Date.now() });
    let finish!: (value: typeof succeeded) => void;
    getMock.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
    const page = render(<HostedProviderCheckout checkout={context} offeredMethods={[]} />);
    await waitFor(() => expect(getMock).toHaveBeenCalledTimes(1));
    page.rerender(<HostedProviderCheckout checkout={{ ...context,
      checkoutId: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd' }} offeredMethods={[]} />);
    finish(succeeded);
    await waitFor(() => expect(screen.queryByRole('progressbar')).toBeNull());
    expect(screen.queryByText('El servidor verificó el pago del proveedor.')).toBeNull();
    expect(createMock).not.toHaveBeenCalled();
  });
});
