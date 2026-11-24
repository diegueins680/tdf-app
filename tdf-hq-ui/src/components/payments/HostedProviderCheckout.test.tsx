/** @jest-environment jsdom */
import { jest } from '@jest/globals';
import { useState } from 'react';
import { PaymentMethodButton } from './PaymentMethodChoices';
import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';

const checkoutId = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const attemptId = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';
const createMock = jest.fn();
const getMock = jest.fn();

jest.unstable_mockModule('../../api/providerPaymentSessions', () => ({
  HOSTED_PAYMENT_METHODS: [
    { label: 'placetopay_card', provider: 'placetopay', paymentMethod: 'card' },
    { label: 'placetopay_bank_redirect', provider: 'placetopay', paymentMethod: 'bank_redirect' },
    { label: 'placetopay_deuna_qr', provider: 'placetopay', paymentMethod: 'deuna_qr' },
    { label: 'payphone_card', provider: 'payphone', paymentMethod: 'card' },
    { label: 'payphone_wallet', provider: 'payphone', paymentMethod: 'payphone_wallet' },
  ],
  createProviderPaymentSession: createMock,
  getProviderPaymentSession: getMock,
  safeProviderRedirect: (provider?: string | null, value?: string | null) => {
    if (provider === 'placetopay') return value?.startsWith('https://checkout-test.placetopay.ec/') ? value : null;
    if (provider === 'payphone') return value?.startsWith('https://pay.payphonetodoesposible.com/') ? value : null;
    return null;
  },
}));

const { default: HostedProviderCheckout } = await import('./HostedProviderCheckout');
const { HOSTED_PAYMENT_METHODS: testHostedMethods } = await import('../../api/providerPaymentSessions');
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

  it('keeps local and hosted choices in one group and locks local alternatives after an ambiguous hosted request', async () => {
    const localStart = jest.fn();
    createMock.mockRejectedValue(new Error('Synthetic lost response'));
    function CombinedMethods() {
      const [locked, setLocked] = useState(false);
      return <HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']}
        onSafetyLockChange={setLocked} additionalMethods={<>
          <PaymentMethodButton disabled={locked} onClick={localStart}>Tarjeta · Datafast</PaymentMethodButton>
          <PaymentMethodButton disabled={locked} onClick={localStart}>PayPal</PaymentMethodButton>
          <PaymentMethodButton disabled={locked} onClick={localStart}>Transferencia bancaria</PaymentMethodButton>
        </>} />;
    }
    render(<CombinedMethods />);
    const group = screen.getByRole('group', { name: 'Métodos de pago disponibles' });
    expect(group.querySelectorAll('button')).toHaveLength(5);
    expect(screen.getAllByRole('region', { name: 'Métodos de pago' })).toHaveLength(1);
    expect(createMock).not.toHaveBeenCalled(); expect(localStart).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Billetera PayPhone', exact: true }));
    expect(screen.getByRole('textbox', { name: 'Número PayPhone' })).toBeTruthy();
    expect(createMock).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Tarjeta · PlaceToPay', exact: true }));
    await screen.findByText(/no sabemos si el pago se completó/);
    for (const label of ['Tarjeta · Datafast', 'PayPal', 'Transferencia bancaria']) {
      const button = screen.getByRole('button', { name: label, exact: true });
      expect(button.hasAttribute('disabled')).toBe(true);
      fireEvent.click(button);
    }
    expect(localStart).not.toHaveBeenCalled(); expect(createMock).toHaveBeenCalledTimes(1);
    expect(screen.getByRole('button', { name: 'Recuperar pago original' })).toBeTruthy();
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

    expect(await screen.findByText(/no sabemos si el pago se completó/)).toBeTruthy();
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
    expect(await screen.findByText(/no sabemos si el pago se completó/)).toBeTruthy();
    const firstIdempotencyKey = createMock.mock.calls[0]?.[2];
    first.unmount();

    render(
      <HostedProviderCheckout
        checkout={context}
        offeredMethods={['placetopay_card', 'payphone_wallet']}
        initialBuyerPhone="991234567"
      />,
    );

    expect(await screen.findByText(/no sabemos si el pago se completó/)).toBeTruthy();
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
    expect(await screen.findByText('Pago confirmado.')).toBeTruthy();
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
    expect(await screen.findByText(/No vuelvas a intentar ni uses otro método/)).toBeTruthy();
    expect(confirmed).not.toHaveBeenCalled();
    expect(navigate).not.toHaveBeenCalled();
    expect(screen.queryByText('Pago confirmado.')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Continuar con el pago' })).toBeNull();
    expect(screen.queryByRole('button', { name: 'Elegir otro método de pago' })).toBeNull();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    expect(locks).toContain(true);
    expect(createMock).toHaveBeenCalledTimes(1);
  });

  it('asks for the PayPhone number only after that wallet is chosen', async () => {
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']}
      initialBuyerPhone="991234567" />);
    const wallet = await screen.findByRole('button', { name: /PayPhone/ });
    expect(screen.queryByRole('textbox', { name: 'Número PayPhone' })).toBeNull();
    fireEvent.click(wallet);
    expect(await screen.findByRole('textbox', { name: 'Número PayPhone' })).toBeTruthy();
    expect(createMock).not.toHaveBeenCalled();
  });

  it('shows PayPhone errors on the invalid fields without creating or locking a payment', async () => {
    const locks: boolean[] = [];
    createMock.mockResolvedValue({ ...succeeded, provider: 'payphone' });
    render(<HostedProviderCheckout checkout={context} offeredMethods={['payphone_wallet']}
      onSafetyLockChange={(locked) => locks.push(locked)} />);
    const wallet = await screen.findByRole('button', { name: /PayPhone/ });
    fireEvent.click(wallet);
    const phone = screen.getByRole<HTMLInputElement>('textbox', { name: 'Número PayPhone' });
    const country = screen.getByRole<HTMLInputElement>('textbox', { name: 'Código país' });
    expect(document.activeElement).toBe(phone);
    fireEvent.change(country, { target: { value: '' } });
    fireEvent.change(phone, { target: { value: '123' } });
    fireEvent.click(wallet);
    expect(country.getAttribute('aria-invalid')).toBe('true');
    expect(phone.getAttribute('aria-invalid')).toBe('true');
    expect(document.activeElement).toBe(country);
    expect(createMock).not.toHaveBeenCalled();
    expect(locks).not.toContain(true);
    fireEvent.change(country, { target: { value: '593' } });
    expect(document.activeElement).toBe(country);
    fireEvent.click(wallet);
    expect(document.activeElement).toBe(phone);
    expect(phone.value).toBe('123');
    fireEvent.change(phone, { target: { value: '999000111' } });
    fireEvent.click(wallet);
    await waitFor(() => expect(createMock).toHaveBeenCalledTimes(1));
    expect(createMock.mock.calls[0]?.[3]).toMatchObject({
      buyerCountryCode: '593', buyerPhone: '999000111',
    });
  });

  it('clears an unused wallet validation error when another offered method is chosen', async () => {
    createMock.mockResolvedValue(succeeded);
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']} />);
    const wallet = await screen.findByRole('button', { name: /PayPhone/ });
    fireEvent.click(wallet);
    fireEvent.click(wallet);
    const phone = screen.getByRole('textbox', { name: 'Número PayPhone' });
    expect(phone.getAttribute('aria-invalid')).toBe('true');
    expect(createMock).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));
    await waitFor(() => expect(createMock).toHaveBeenCalledTimes(1));
    expect(phone.getAttribute('aria-invalid')).toBe('false');
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

  it('sends a PayPhone card buyer to the hosted page without asking for a phone number', async () => {
    const navigate = jest.fn();
    const hosted = 'https://pay.payphonetodoesposible.com/Anonymous/Index?paymentId=Pay-1';
    createMock.mockResolvedValue({
      checkoutId,
      attemptId,
      operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
      provider: 'payphone',
      state: 'requires_customer_action',
      externalId: 'Pay-1',
      redirectUrl: hosted,
      outcomeCertainty: 'ambiguous',
      canRetryOrFallback: false,
    });
    render(<HostedProviderCheckout checkout={context}
      offeredMethods={['payphone_card']} navigateToProvider={navigate} />);
    expect(screen.queryByRole('textbox')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Tarjeta · PayPhone' }));
    await waitFor(() => expect(navigate).toHaveBeenCalledWith(hosted));
    expect(createMock).toHaveBeenCalledWith(checkoutId, context.lookupToken, expect.any(String),
      { provider: 'payphone', paymentMethod: 'card' });
    expect(screen.queryByRole('textbox')).toBeNull();
  });

  it('keeps wallet fields hidden and fallback locked for a PayPhone card among other offered methods', async () => {
    const navigate = jest.fn();
    const hosted = 'https://pay.payphonetodoesposible.com/Anonymous/Index?paymentId=Pay-1';
    createMock.mockResolvedValue({
      checkoutId, attemptId, operationId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
      provider: 'payphone', state: 'requires_customer_action', externalId: 'Pay-1',
      redirectUrl: hosted, outcomeCertainty: 'ambiguous', canRetryOrFallback: false,
    });
    render(<HostedProviderCheckout checkout={context}
      offeredMethods={testHostedMethods.map((method) => method.label)} navigateToProvider={navigate} />);
    const wallet = screen.getByRole('button', { name: 'Billetera PayPhone' });
    expect(screen.queryByRole('textbox')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Tarjeta · PayPhone' }));
    await waitFor(() => expect(navigate).toHaveBeenCalledWith(hosted));
    expect(createMock).toHaveBeenCalledWith(checkoutId, context.lookupToken, expect.any(String),
      { provider: 'payphone', paymentMethod: 'card' });
    expect(screen.queryByRole('textbox')).toBeNull();
    expect(wallet.hasAttribute('disabled')).toBe(true);
  });

  it('never mints a new request key when the original recovery key is missing', async () => {
    seedPending();
    clearPaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    const locks: boolean[] = [];
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']}
      onSafetyLockChange={(locked) => locks.push(locked)} />);
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    expect(await screen.findByText(/contacta a soporte para revisar esta orden/)).toBeTruthy();
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
    expect(await screen.findByText(/no sabemos si el pago se completó/)).toBeTruthy();
    expect(loadProviderPaymentPending(checkoutId)).not.toBeNull();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
  });

  it('restores a known attempt without requiring its provider in current offerings', async () => {
    saveProviderPaymentResume({ version: 1, ...context, attemptId, provider: 'placetopay',
      paymentMethod: 'card', createdAt: Date.now() });
    getMock.mockResolvedValue(succeeded);
    render(<HostedProviderCheckout checkout={context} offeredMethods={[]} disabled />);
    expect(await screen.findByText('Pago confirmado.')).toBeTruthy();
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

  it('recovers the transmitted mixing payment despite an unrelated saved resume', async () => {
    const prepareCheckout = jest.fn();
    const mixingId = 'dddddddd-dddd-4ddd-8ddd-dddddddddddd';
    saveProviderPaymentResume({ version: 1, ...context, attemptId, provider: 'placetopay',
      paymentMethod: 'card', createdAt: Date.now() });
    const key = loadOrCreatePaymentIdempotencyKey(mixingId, 'placetopay', 'card');
    saveProviderPaymentPending({ version: 1, checkoutId: mixingId,
      lookupToken: 'mixing-lookup-token-secure', returnPath: '/mezcla-mastering/pedido/existing',
      provider: 'placetopay', paymentMethod: 'card', createdAt: Date.now() });
    createMock.mockResolvedValue({ ...succeeded, checkoutId: mixingId });
    render(<HostedProviderCheckout offeredMethods={['placetopay_card', 'payphone_wallet']}
      prepareCheckout={prepareCheckout} pendingReturnPathPrefix="/mezcla-mastering/pedido/" />);
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    expect(await screen.findByText('Pago confirmado.')).toBeTruthy();
    expect(createMock).toHaveBeenCalledTimes(1);
    expect(createMock).toHaveBeenCalledWith(mixingId, 'mixing-lookup-token-secure', key,
      { provider: 'placetopay', paymentMethod: 'card' });
    expect(prepareCheckout).not.toHaveBeenCalled();
    expect(getMock).not.toHaveBeenCalled();
  });

  it('allows a new mixing order after verifying the retained earlier order succeeded', async () => {
    saveProviderPaymentResume({ version: 1, ...context, returnPath: '/mezcla-mastering/pedido/old',
      attemptId, provider: 'placetopay', paymentMethod: 'card', createdAt: Date.now() });
    const newId = 'dddddddd-dddd-4ddd-8ddd-dddddddddddd';
    const prepareCheckout = jest.fn().mockResolvedValue({ ...context, checkoutId: newId,
      returnPath: '/mezcla-mastering/pedido/new' });
    getMock.mockResolvedValue(succeeded);
    createMock.mockResolvedValue({ ...succeeded, checkoutId: newId });
    render(<HostedProviderCheckout offeredMethods={['placetopay_card']}
      prepareCheckout={prepareCheckout} pendingReturnPathPrefix="/mezcla-mastering/pedido/" />);
    const button = await screen.findByRole('button', { name: /Tarjeta · PlaceToPay/ });
    await waitFor(() => expect(button.hasAttribute('disabled')).toBe(false));
    fireEvent.click(button);
    await waitFor(() => expect(prepareCheckout).toHaveBeenCalledTimes(1));
    await waitFor(() => expect(createMock.mock.calls[0]?.[0]).toBe(newId));
  });

  it('keeps an unresolved mixing resume locked and does not prepare another order', async () => {
    saveProviderPaymentResume({ version: 1, ...context, returnPath: '/mezcla-mastering/pedido/old',
      attemptId, provider: 'placetopay', paymentMethod: 'card', createdAt: Date.now() });
    const prepareCheckout = jest.fn();
    getMock.mockResolvedValue({ ...succeeded, state: 'ambiguous', outcomeCertainty: 'ambiguous' });
    render(<HostedProviderCheckout offeredMethods={['placetopay_card', 'payphone_wallet']}
      prepareCheckout={prepareCheckout} pendingReturnPathPrefix="/mezcla-mastering/pedido/" />);
    expect(await screen.findByText(/No vuelvas a intentar ni uses otro método/)).toBeTruthy();
    expect(screen.getByRole('button', { name: /PayPhone/ }).hasAttribute('disabled')).toBe(true);
    expect(prepareCheckout).not.toHaveBeenCalled();
  });

  it('rechecks the original attempt after a temporary status error', async () => {
    jest.useFakeTimers();
    try {
      const awaiting = { checkoutId, attemptId, provider: 'placetopay',
        state: 'requires_customer_action', canRetryOrFallback: false,
        redirectUrl: 'https://checkout-test.placetopay.ec/session/test' };
      createMock.mockResolvedValue(awaiting);
      getMock.mockRejectedValueOnce(new Error('temporary loss'))
        .mockResolvedValueOnce({ ...awaiting, state: 'succeeded', redirectUrl: null });
      render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']}
        navigateToProvider={jest.fn()} />);
      await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Tarjeta · PlaceToPay' })); });
      await act(async () => { await jest.advanceTimersByTimeAsync(4_000); });
      expect(screen.getByText(/No pudimos actualizar el estado/)).toBeTruthy();
      expect(screen.queryByRole('button', { name: 'Continuar con el pago' })).toBeNull();
      expect(screen.getByRole('button', { name: 'Billetera PayPhone' }).hasAttribute('disabled')).toBe(true);
      await act(async () => { await jest.advanceTimersByTimeAsync(4_000); });
      expect(screen.getByText('Pago confirmado.')).toBeTruthy();
      expect(screen.queryByText(/No pudimos actualizar el estado/)).toBeNull();
      expect(getMock.mock.calls).toEqual(Array(2).fill([checkoutId, attemptId, context.lookupToken]));
      expect(createMock).toHaveBeenCalledTimes(1);
    } finally {
      jest.useRealTimers();
    }
  });

  it('shares one status request between repeated clicks and the polling timer', async () => {
    jest.useFakeTimers();
    try {
      const processing = { checkoutId, attemptId, provider: 'placetopay', state: 'processing',
        canRetryOrFallback: false };
      createMock.mockResolvedValue(processing);
      let finish!: (next: typeof processing) => void;
      getMock.mockImplementation(() => new Promise(resolve => { finish = resolve; }));
      const confirmed = jest.fn();
      render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card']}
        onPaymentConfirmed={confirmed} />);
      await act(async () => { fireEvent.click(screen.getByRole('button', { name: 'Tarjeta · PlaceToPay' })); });
      const refresh = screen.getByRole('button', { name: 'Actualizar estado del pago' });
      fireEvent.click(refresh);
      fireEvent.click(refresh);
      await act(async () => { await jest.advanceTimersByTimeAsync(8_000); });
      expect(getMock).toHaveBeenCalledTimes(1);
      await act(async () => { finish({ ...processing, state: 'succeeded' }); });
      await act(async () => { await jest.advanceTimersByTimeAsync(8_000); });
      expect(confirmed).toHaveBeenCalledTimes(1);
      expect(getMock).toHaveBeenCalledTimes(1);
    } finally {
      jest.useRealTimers();
    }
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
    expect(screen.queryByText('Pago confirmado.')).toBeNull();
    expect(createMock).not.toHaveBeenCalled();
  });
});


describe('payment failure alternatives', () => {
  beforeEach(() => {
    createMock.mockReset();
    getMock.mockReset();
    window.sessionStorage.clear();
  });

  it('shows hosted and legacy alternatives after a verified decline without starting another payment', async () => {
    createMock.mockResolvedValue({ checkoutId, attemptId, provider: 'placetopay',
      state: 'failed', outcomeCertainty: 'confirmed_no_charge', canRetryOrFallback: true });
    render(<HostedProviderCheckout checkout={context}
      offeredMethods={['placetopay_card', 'payphone_wallet', 'paypal', 'bank_transfer']} />);
    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));
    await screen.findByText(/Otros medios de pago para esta compra/);
    const alternatives = screen.getAllByRole('listitem').map((item) => item.textContent);
    expect(alternatives).toEqual(['App PayPhone', 'PayPal', 'Transferencia bancaria']);
    expect(createMock).toHaveBeenCalledTimes(1);
    expect(screen.getByRole('button', { name: 'Elegir otro método de pago' })).toBeTruthy();
  });

  it('shows alternatives after an external failure even without a hosted executor', () => {
    render(<HostedProviderCheckout checkout={context} offeredMethods={['datafast', 'paypal']}
      paymentFailure={{ method: 'datafast', needsVerification: true }} />);
    expect(screen.getAllByRole('listitem').map((item) => item.textContent)).toEqual(['PayPal']);
    expect(screen.getByText(/Antes de elegir otra opción, verifica/)).toBeTruthy();
    expect(createMock).not.toHaveBeenCalled();
  });

  it('keeps all alternatives locked after a timeout while still listing them', async () => {
    createMock.mockRejectedValue(new Error('timeout after transmission'));
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card', 'payphone_wallet']} />);
    fireEvent.click(screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ }));
    await screen.findByText(/Otros medios de pago para esta compra/);
    expect(screen.getAllByRole('listitem').map((item) => item.textContent)).toEqual(['App PayPhone']);
    const alternative = screen.getByRole('button', { name: /PayPhone/ });
    expect(alternative.hasAttribute('disabled')).toBe(true);
    fireEvent.click(alternative);
    expect(createMock).toHaveBeenCalledTimes(1);
  });

  it('preserves unknown availability and prevents new attempts with cached methods', () => {
    render(<HostedProviderCheckout checkout={context} offeredMethods={['placetopay_card']}
      availabilityUnknown paymentFailure={{ needsVerification: false }} />);
    expect(screen.getByText(/No pudimos consultar otros medios de pago/)).toBeTruthy();
    expect(screen.queryByText(/No hay otro medio habilitado/)).toBeNull();
    const method = screen.getByRole('button', { name: /Tarjeta · PlaceToPay/ });
    expect(method.hasAttribute('disabled')).toBe(true);
    fireEvent.click(method);
    expect(createMock).not.toHaveBeenCalled();
  });

  it('still recovers the original transmitted attempt while current methods are unknown', async () => {
    const key = loadOrCreatePaymentIdempotencyKey(checkoutId, 'placetopay', 'card');
    saveProviderPaymentPending({ version: 1, ...context, provider: 'placetopay', paymentMethod: 'card', createdAt: Date.now() });
    createMock.mockResolvedValue({ checkoutId, attemptId, provider: 'placetopay',
      state: 'processing', canRetryOrFallback: false, outcomeCertainty: 'ambiguous' });
    render(<HostedProviderCheckout checkout={context} offeredMethods={[]} availabilityUnknown />);
    fireEvent.click(await screen.findByRole('button', { name: 'Recuperar pago original' }));
    await waitFor(() => expect(createMock).toHaveBeenCalledTimes(1));
    expect(createMock).toHaveBeenCalledWith(checkoutId, context.lookupToken, key,
      { provider: 'placetopay', paymentMethod: 'card' });
  });
});
