import { beforeEach, expect, it, jest } from '@jest/globals';

import { ApiError } from '../../api/client';

const createAgreementPayment = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const createProviderPaymentSession = jest.fn<(...args: unknown[]) => Promise<unknown>>();
const saveProviderPaymentResume = jest.fn<(...args: unknown[]) => boolean>();
jest.unstable_mockModule('../../api/directory', () => ({ Directory: { createAgreementPayment } }));
jest.unstable_mockModule('../../api/providerPaymentSessions', () => ({ createProviderPaymentSession }));
jest.unstable_mockModule('../../utils/providerPaymentResume', () => ({
  loadOrCreatePaymentIdempotencyKey: () => 'payment-session-0123456789abcdef',
  saveProviderPaymentResume,
}));
const { agreementPayableByBuyer, safePayPhoneRedirect, startAgreementPayment, AGREEMENT_PAYMENT_RETURN_PATH } =
  await import('./agreementPayment');

const checkoutId = '0b8c6a1e-6f1d-4c33-9d0a-2f6a8a7f4c11';
const attemptId = '5e0b7f3c-2a44-4c51-8f6e-9b1d2c3a4e55';

beforeEach(() => {
  createAgreementPayment.mockReset();
  createProviderPaymentSession.mockReset();
  saveProviderPaymentResume.mockReset();
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

it('starts the charge, keeps the recovery record and sends the buyer to PayPhone', async () => {
  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789', paymentId: 'p', provider: 'payphone', agreement: {} });
  createProviderPaymentSession.mockResolvedValue({ checkoutId, attemptId, redirectUrl: 'https://pay.payphonetodoesposible.com/PayWithCard/abc' });
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toBe('redirected');
  expect(createAgreementPayment).toHaveBeenCalledWith('agreement-1', 'payphone');
  expect(createProviderPaymentSession).toHaveBeenCalledWith(checkoutId, 'lookup-token-0123456789', 'payment-session-0123456789abcdef',
    { provider: 'payphone', paymentMethod: 'card' });
  expect(saveProviderPaymentResume).toHaveBeenCalledWith(expect.objectContaining({
    checkoutId, attemptId, provider: 'payphone', paymentMethod: 'card', returnPath: AGREEMENT_PAYMENT_RETURN_PATH,
  }));
  expect(navigate).toHaveBeenCalledWith('https://pay.payphonetodoesposible.com/PayWithCard/abc');
});

it('reports a disabled gate without storing anything and never opens a foreign page', async () => {
  createAgreementPayment.mockRejectedValue(new ApiError('Feature marketplace.agreement_payments is not enabled', 503));
  await expect(startAgreementPayment('agreement-1', jest.fn())).resolves.toBe('not_enabled');
  expect(saveProviderPaymentResume).not.toHaveBeenCalled();

  createAgreementPayment.mockResolvedValue({ checkoutId, lookupToken: 'lookup-token-0123456789' });
  createProviderPaymentSession.mockResolvedValue({ checkoutId, attemptId, redirectUrl: 'https://evil.test/pay' });
  const navigate = jest.fn<(url: string) => void>();
  await expect(startAgreementPayment('agreement-1', navigate)).resolves.toBe('unsafe_redirect');
  expect(navigate).not.toHaveBeenCalled();

  createAgreementPayment.mockRejectedValue(new ApiError('a payment for this agreement is already in progress', 409));
  await expect(startAgreementPayment('agreement-1', jest.fn())).rejects.toThrow('already in progress');
});
