import { describe, expect, it } from '@jest/globals';

import type { AgreementPayment } from '../../api/directory';
import { agreementActions, agreementChargeDue, bpsToPercent, isAgreementOpen, visibleAgreementPayments } from './agreements';

describe('agreement actions (ADR 0135)', () => {
  it('lets the provider start, only the buyer complete, and either party cancel or dispute a live agreement', () => {
    expect(agreementActions('confirmed', 'provider')).toEqual(['in_progress', 'cancelled', 'disputed']);
    expect(agreementActions('confirmed', 'buyer')).toEqual(['completed', 'cancelled', 'disputed']);
    expect(agreementActions('in_progress', 'provider')).toEqual(['cancelled', 'disputed']);
    expect(agreementActions('in_progress', 'buyer')).toEqual(['completed', 'cancelled', 'disputed']);
  });

  it('offers nothing on settled or settlement-owned states and leaves disputes to an admin', () => {
    for (const status of ['completed', 'cancelled', 'expired', 'disputed'] as const) {
      expect(agreementActions(status, 'buyer')).toEqual([]);
      expect(agreementActions(status, 'provider')).toEqual([]);
    }
    expect(agreementActions('disputed', 'admin')).toEqual(['completed', 'cancelled']);
    expect(agreementActions('confirmed', 'admin')).toEqual([]);
  });

  it('classifies open agreements and formats basis points', () => {
    expect(isAgreementOpen('confirmed')).toBe(true);
    expect(isAgreementOpen('disputed')).toBe(true);
    expect(isAgreementOpen('completed')).toBe(false);
    expect(isAgreementOpen('cancelled')).toBe(false);
    expect(bpsToPercent(1000)).toBe('10');
    expect(bpsToPercent(1250)).toBe('12.5');
    expect(bpsToPercent(3000)).toBe('30');
    expect(bpsToPercent(0)).toBe('0');
  });

  it('lets either party walk away before the deposit is paid and nothing else (ADR 0136)', () => {
    expect(agreementActions('pending_deposit', 'buyer')).toEqual(['cancelled']);
    expect(agreementActions('pending_deposit', 'provider')).toEqual(['cancelled']);
    expect(agreementActions('pending_deposit', 'admin')).toEqual([]);
    expect(agreementActions('balance_due', 'buyer')).toEqual(['disputed']);
    expect(agreementActions('balance_due', 'provider')).toEqual(['disputed']);
    expect(agreementActions('balance_due', 'admin')).toEqual([]);
  });

  it('names the charge a platform agreement is waiting for', () => {
    expect(agreementChargeDue({ status: 'pending_deposit', settlementMode: 'platform' })).toBe('deposit');
    expect(agreementChargeDue({ status: 'balance_due', settlementMode: 'platform' })).toBe('balance');
    expect(agreementChargeDue({ status: 'confirmed', settlementMode: 'platform' })).toBeNull();
    expect(agreementChargeDue({ status: 'confirmed', settlementMode: 'off_platform' })).toBeNull();
  });

  it('shows paid and pending charges and hides expired ones', () => {
    const payment = (id: string, status: AgreementPayment['status']): AgreementPayment => ({
      id, kind: 'deposit', provider: 'payphone', status, amountMinor: 3000, currency: 'USD', platformFeeMinor: 1000,
      processingFeeMinor: 173, processingFeePayer: 'provider', providerAmountMinor: 1827,
      dueAt: '2026-11-20T00:00:00Z', createdAt: '2026-11-12T00:00:00Z',
    });
    expect(visibleAgreementPayments({ payments: [payment('a', 'expired'), payment('b', 'paid'), payment('c', 'pending')] }).map((item) => item.id)).toEqual(['b', 'c']);
    expect(visibleAgreementPayments({})).toEqual([]);
  });
});
