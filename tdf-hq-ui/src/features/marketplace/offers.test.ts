import { describe, expect, it } from '@jest/globals';

import type { ClassifiedApplication } from '../../api/directory';
import {
  canAccept,
  canCounter,
  canReject,
  canWithdraw,
  formatMinor,
  isOfferOpen,
  moneyInputToMinor,
  referenceAmount,
  sortOffers,
  waitingOn,
} from './offers';

const offer = (overrides: Partial<ClassifiedApplication>): ClassifiedApplication => ({
  id: 'a', classifiedId: 'c', applicantProfileId: 'p', applicantProfile: { id: 'p', name: 'Banda', slug: 'banda' },
  message: 'hola', status: 'submitted', currentRound: 1, lastActorRole: 'applicant', participantRole: 'author',
  submittedAt: '2026-11-01T00:00:00Z', version: 1, ...overrides,
});

describe('marketplace offer helpers (ADR 0132)', () => {
  it('mirrors the backend turn rules for accepting and countering', () => {
    expect(canAccept('submitted', 'author')).toBe(true);
    expect(canAccept('submitted', 'applicant')).toBe(false);
    expect(canAccept('countered_by_author', 'applicant')).toBe(true);
    expect(canAccept('countered_by_author', 'author')).toBe(false);
    expect(canAccept('countered_by_applicant', 'author')).toBe(true);
    expect(canAccept('accepted', 'author')).toBe(false);
    expect(canCounter('submitted', 'author', 'suggested')).toBe(true);
    expect(canCounter('submitted', 'author', 'fixed')).toBe(false);
    expect(canCounter('countered_by_author', 'applicant', 'open')).toBe(true);
  });

  it('lets only the author reject open offers and only the offerer withdraw', () => {
    expect(canReject('shortlisted', 'author')).toBe(true);
    expect(canReject('accepted', 'author')).toBe(false);
    expect(canReject('submitted', 'applicant')).toBe(false);
    expect(canWithdraw('countered_by_author', 'applicant')).toBe(true);
    expect(canWithdraw('accepted', 'applicant')).toBe(true);
    expect(canWithdraw('rejected', 'applicant')).toBe(false);
    expect(canWithdraw('submitted', 'author')).toBe(false);
  });

  it('knows whose move it is', () => {
    expect(waitingOn('submitted')).toBe('author');
    expect(waitingOn('countered_by_author')).toBe('applicant');
    expect(waitingOn('countered_by_applicant')).toBe('author');
    expect(waitingOn('accepted')).toBeNull();
    expect(isOfferOpen('expired')).toBe(false);
  });

  it('formats minor units with the taxonomy currency', () => {
    const currencies = [{ id: 'usd', code: 'USD', name: 'Dólar', symbol: '$', minorUnits: 2 }, { id: 'jpy', code: 'JPY', name: 'Yen', symbol: '¥', minorUnits: 0 }];
    expect(formatMinor(125000, 'usd', currencies, 'en')).toBe('$ 1,250.00');
    expect(formatMinor(5000, 'jpy', currencies, 'en')).toBe('¥ 5,000');
    expect(formatMinor(null, 'usd', currencies)).toBeNull();
    expect(moneyInputToMinor('1.250,5'.replace('.', ''), 2)).toBe(125050);
    expect(moneyInputToMinor('', 2)).toBeUndefined();
    expect(moneyInputToMinor('-3', 2)).toBeUndefined();
  });

  it('derives the reference price only for suggested or fixed classifieds', () => {
    expect(referenceAmount({ priceMode: 'open', budget: { maxMinor: 100, currencyId: 'usd' } })).toBeNull();
    expect(referenceAmount({ priceMode: 'fixed', budget: { maxMinor: 40000, currencyId: 'usd' } })).toEqual({ amountMinor: 40000, currencyId: 'usd' });
    expect(referenceAmount(undefined)).toBeNull();
  });

  it('sorts offers by price, reputation and recency with nulls last', () => {
    const offers = [
      offer({ id: 'a', currentAmountMinor: 130000, reputation: { ratingAvg: 4.9, ratingCount: 10, completedCount: 12 }, updatedAt: '2026-11-02T00:00:00Z' }),
      offer({ id: 'b', currentAmountMinor: 110000, reputation: { ratingAvg: null, ratingCount: 0, completedCount: 0 }, updatedAt: '2026-11-03T00:00:00Z' }),
      offer({ id: 'c', currentAmountMinor: null, proposedAmountMinor: null, updatedAt: '2026-11-04T00:00:00Z' }),
    ];
    expect(sortOffers(offers, 'price_asc').map((item) => item.id)).toEqual(['b', 'a', 'c']);
    expect(sortOffers(offers, 'price_desc').map((item) => item.id)).toEqual(['a', 'b', 'c']);
    expect(sortOffers(offers, 'reputation').map((item) => item.id)).toEqual(['a', 'b', 'c']);
    expect(sortOffers(offers, 'recent').map((item) => item.id)).toEqual(['c', 'b', 'a']);
  });
});
