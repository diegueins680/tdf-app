import { beforeEach, expect, it, jest } from '@jest/globals';

import { ApiError } from '../../api/client';

const createAgreementPayment = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const createProviderPaymentSession = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const getProviderPaymentSession = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const saveProviderPaymentResume = jest.fn<(...args: unknown[]) => boolean>();
const saveProviderPaymentPending = jest.fn<(...args: unknown[]) => boolean>();
const loadProviderPaymentResume = jest.fn<(...args: unknown[]) => unknown>();
const loadProviderPaymentPending = jest.fn<(...args: unknown[]) => unknown>();
const loadExistingPaymentIdempotencyKey = jest.fn<(...args: unknown[]) => string | null>();
const clearProviderPaymentPending = jest.fn();
const clearProviderPaymentResume = jest.fn();
const clearPaymentIdempotencyKey = jest.fn();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { createAgreementPayment } }));
jest.unstable_mockModule('../../api/providerPaymentSessions', () => ({ createProviderPaymentSession, getProviderPaymentSession }));
jest.unstable_mockModule('../../utils/providerPaymentResume', () => ({
  clearPaymentIdempotencyKey,
  clearProviderPaymentPending,
  clearProviderPaymentResume,
  loadExistingPaymentIdempotencyKey,
  loadOrCreatePaymentIdempotencyKey: () => 'payment-session-0123456789abcdef',
  loadProviderPaymentPending,
  loadProviderPaymentResume,
  saveProviderPaymentPending,
  saveProviderPaymentResume,
}));
const { agreementPayableByBuyer, agreementPaymentReturnPath, safePayPhoneRedirect, startAgreementPayment, AGREEMENT_PAYMENT_RETURN_PATH } =
  await import('./agreementPayment');

const checkoutId = '0b8c6a1e-6f1d-4c33-9d0a-2f6a8a7f4c11';
const attemptId = '5e0b7f3c-2a44-4c51-8f6e-9b1d2c3a4e55';

const returnPath = `${AGREEMENT_PAYMENT_RETURN_PATH}&acuerdo=agreement-1`;
const payPage = 'https://pay.payphonetodoesposible.com/PayWithCard/abc';
const session = (state: string, extra: Record<string, unknown> = {}) =>
  ({ checkoutId, attemptId, state, redirectUrl: payPage, canRetryOrFallback: false, ...extra });
const none = { redirected: false, notEnabled: false, inProgress: false, recoveryUnavailable: false };

beforeEach(() => {
  for (const mock of [createAgreementPayment, createProviderPaymentSession, getProviderPaymentSession,
    saveProviderPaymentResume, saveProviderPaymentPending, loadProviderPaymentResume, loadProviderPaymentPending,
    loadExistingPaymentIdempotencyKey, clearProviderPaymentPending, clearProviderPaymentResume, clearPaymentIdempotencyKey]) {
    mock.mockReset();
  }
  loadProviderPaymentResume.mockReturnValue(null);
  loadProviderPaymentPending.mockReturnValue(null);
  saveProviderPaymentPending.mockReturnValue(true);
  saveProviderPaymentResume.mockReturnValue(true);
});

it('opens only HTTPS PayPhone pages without credentials or ports', () => {
  expect(safePayPhoneRedirect('https://pay.payphonetodoesposible.com/PayWithCard/abc')).toBe('https://pay.payphonetodoesposible.com/PayWithCard/abc');
  expect(safePayPhoneRedirect('https://payphone.app/pay/abc')).toBe('https://payphone.app/pay/abc');
  expect(safePayPhoneRedirect('http://pay.payphonetodoesposible.com/PayWithCard/abc')).toBeNull();
  expect(safePayPhoneRedirect('https://user@pay.payphonetodoesposible.com/x')).toBeNull();
  expect(safePayPhoneRedirect('https://pay.payphonetodoesposible.com:8443/x')).toBeNull();
  expect(safePayPhoneRedirect('https://payphonetodoesposible.com.evil.test/x')).toBeNull();
  expect(safePayPhoneRedirect('javascript:alert(1)')).toBeNull();
  expect(safePayPhoneRedirect(null)).toBeNull();
});

it('lets only the buyer pay a platform charge that the provider can receive through PayPhone', () => {
  const agreement: Parameters<typeof agreementPayableByBuyer>[0] = {
    participantRole: 'buyer', settlementMode: 'platform', status: 'pending_deposit', payoutProviders: ['payphone'],
  };
  expect(agreementPayableByBuyer(agreement)).toBe(true);
  expect(agreementPayableByBuyer({ ...agreement, status: 'balance_due' })).toBe(true);
  expect(agreementPayableByBuyer({ ...agreement, participantRole: 'provider' })).toBe(false);
  expect(agreementPayableByBuyer({ ...agreement, settlementMode: 'off_platform' })).toBe(false);
  expect(agreementPayableByBuyer({ ...agreement, status: 'confirmed' })).toBe(false);
  expect(agreementPayableByBuyer({ ...agreement, payoutProviders: [] })).toBe(false);
});

it('records the checkout before the session request, then sends the buyer to PayPhone', async () => {
  expect(agreementPaymentReturnPath('agreement-1')).toBe(returnPath);
  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789', paymentId: 'p', provider: 'payphone', agreement: {} });
  createProviderPaymentSession.mockImplementation(async () => {
    // The recovery record exists before the request can reach the server.
    expect(saveProviderPaymentPending).toHaveBeenCalledWith(expect.objectContaining({
      checkoutId, lookupToken: 'lookup-token-0123456789', provider: 'payphone', paymentMethod: 'card', returnPath,
    }));
    return session('requires_customer_action');
  });
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toEqual({ ...none, redirected: true });
  expect(createAgreementPayment).toHaveBeenCalledWith('agreement-1', 'payphone');
  expect(createProviderPaymentSession).toHaveBeenCalledWith(checkoutId, 'lookup-token-0123456789', 'payment-session-0123456789abcdef',
    { provider: 'payphone', paymentMethod: 'card' });
  expect(saveProviderPaymentResume).toHaveBeenCalledWith(expect.objectContaining({
    checkoutId, attemptId, provider: 'payphone', paymentMethod: 'card', returnPath,
  }));
  expect(clearProviderPaymentPending).toHaveBeenCalledWith(checkoutId);
  expect(navigate).toHaveBeenCalledWith(payPage);
});

it('replays a session request whose answer was lost instead of creating another charge', async () => {
  loadProviderPaymentPending.mockReturnValue({ checkoutId, lookupToken: 'lookup-token-0123456789', returnPath });
  loadExistingPaymentIdempotencyKey.mockReturnValue('payment-session-original-key');
  createProviderPaymentSession.mockResolvedValue(session('requires_customer_action'));
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toEqual({ ...none, redirected: true });
  expect(loadProviderPaymentPending).toHaveBeenCalledWith(undefined, returnPath);
  expect(createAgreementPayment).not.toHaveBeenCalled();
  expect(createProviderPaymentSession).toHaveBeenCalledWith(checkoutId, 'lookup-token-0123456789', 'payment-session-original-key',
    { provider: 'payphone', paymentMethod: 'card' });

  // Without the original key the request is not repeated with another one.
  createProviderPaymentSession.mockClear();
  loadExistingPaymentIdempotencyKey.mockReturnValue(null);
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, inProgress: true });
  expect(createProviderPaymentSession).not.toHaveBeenCalled();
});

it('starts over once when the stored checkout was replaced or ended', async () => {
  loadProviderPaymentPending.mockReturnValueOnce({ checkoutId: 'old-checkout', lookupToken: 'old-token', returnPath });
  loadExistingPaymentIdempotencyKey.mockReturnValue('payment-session-original-key');
  createProviderPaymentSession
    .mockRejectedValueOnce(new ApiError('This agreement charge is no longer payable', 409))
    .mockResolvedValueOnce(session('requires_customer_action'));
  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789' });
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, redirected: true });
  expect(clearProviderPaymentPending).toHaveBeenCalledWith('old-checkout');
  expect(clearPaymentIdempotencyKey).toHaveBeenCalledWith('old-checkout', 'payphone', 'card');
  expect(createAgreementPayment).toHaveBeenCalledTimes(1);
});

it('reopens an attempt already started and never pays twice while it may still charge', async () => {
  loadProviderPaymentResume.mockReturnValue({ checkoutId, attemptId, lookupToken: 'lookup-token-0123456789', returnPath });
  getProviderPaymentSession.mockResolvedValue(session('requires_customer_action'));
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toEqual({ ...none, redirected: true });
  expect(getProviderPaymentSession).toHaveBeenCalledWith(checkoutId, attemptId, 'lookup-token-0123456789');
  expect(navigate).toHaveBeenCalledWith(payPage);

  getProviderPaymentSession.mockResolvedValue(session('processing', { redirectUrl: null }));
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, inProgress: true });
  expect(createAgreementPayment).not.toHaveBeenCalled();
  expect(createProviderPaymentSession).not.toHaveBeenCalled();

  // An attempt known not to have charged is forgotten and a new charge starts.
  getProviderPaymentSession.mockResolvedValue(session('failed', { canRetryOrFallback: true }));
  createAgreementPayment.mockResolvedValue({ checkoutId: 'new-checkout', lookupToken: 'new-token' });
  createProviderPaymentSession.mockResolvedValue(session('requires_customer_action'));
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, redirected: true });
  expect(clearProviderPaymentResume).toHaveBeenCalledWith(checkoutId);
  expect(createAgreementPayment).toHaveBeenCalledWith('agreement-1', 'payphone');
});

it('does not start a payment this tab could not recover', async () => {
  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789' });
  saveProviderPaymentPending.mockReturnValue(false);
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, recoveryUnavailable: true });
  expect(createProviderPaymentSession).not.toHaveBeenCalled();
  expect(clearPaymentIdempotencyKey).toHaveBeenCalledWith(checkoutId, 'payphone', 'card');
});

it('reports a disabled gate without storing anything and never opens a foreign page', async () => {
  createAgreementPayment.mockRejectedValue(new ApiError('Feature marketplace.agreement_payments is not enabled', 503));
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toEqual({ ...none, notEnabled: true });
  expect(saveProviderPaymentPending).not.toHaveBeenCalled();
  expect(saveProviderPaymentResume).not.toHaveBeenCalled();

  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789' });
  createProviderPaymentSession.mockResolvedValue(session('requires_customer_action', { redirectUrl: 'https://evil.test/pay', canRetryOrFallback: true }));
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toEqual({ ...none, inProgress: true });
  expect(navigate).not.toHaveBeenCalled();

  createAgreementPayment.mockRejectedValue(new ApiError('a payment for this agreement is already in progress', 409));
  await expect(startAgreementPayment('agreement-1', jest.fn())).rejects.toThrow('already in progress');
});
