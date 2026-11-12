import { describe, expect, it } from '@jest/globals';

import { agreementActions, bpsToPercent, isAgreementOpen } from './agreements';

describe('agreement actions (ADR 0135)', () => {
  it('lets the provider start, only the buyer complete, and either party cancel or dispute a live agreement', () => {
    expect(agreementActions('confirmed', 'provider')).toEqual(['in_progress', 'cancelled', 'disputed']);
    expect(agreementActions('confirmed', 'buyer')).toEqual(['completed', 'cancelled', 'disputed']);
    expect(agreementActions('in_progress', 'provider')).toEqual(['cancelled', 'disputed']);
    expect(agreementActions('in_progress', 'buyer')).toEqual(['completed', 'cancelled', 'disputed']);
  });

  it('offers nothing on settled or settlement-owned states and leaves disputes to an admin', () => {
    for (const status of ['pending_deposit', 'balance_due', 'completed', 'cancelled', 'expired', 'disputed'] as const) {
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
});
