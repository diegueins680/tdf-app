import { convertCurrency, DEFAULT_EXCHANGE_RATE_API_BASE } from './CurrencyContext';

describe('exchange-rate service', () => {
  it('uses the current Frankfurter API without a cross-origin redirect', () => {
    expect(DEFAULT_EXCHANGE_RATE_API_BASE).toBe('https://api.frankfurter.dev/v1');
  });
});

describe('convertCurrency', () => {
  const rates = { USD: 1, EUR: 0.8, JPY: 150 };

  it('converts through the configured rate base', () => {
    expect(convertCurrency(100, 'USD', 'EUR', rates)).toBeCloseTo(80);
    expect(convertCurrency(80, 'EUR', 'JPY', rates)).toBeCloseTo(15000);
  });

  it('keeps same-currency values and rejects missing rates', () => {
    expect(convertCurrency(42, 'GBP', 'GBP', rates)).toBe(42);
    expect(convertCurrency(42, 'GBP', 'EUR', rates)).toBeNull();
  });
});
