import { convertCurrency } from './CurrencyContext';

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
