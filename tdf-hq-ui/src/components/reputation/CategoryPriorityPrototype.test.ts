import { rankOrderCentroid } from './CategoryPriorityPrototype';

describe('rankOrderCentroid', () => {
  it('returns no weights for no active categories', () => {
    expect(rankOrderCentroid(0)).toEqual([]);
  });

  it('keeps priority monotonic and totals exactly 100', () => {
    for (const count of [1, 2, 3, 8, 10]) {
      const weights = rankOrderCentroid(count);
      expect(weights).toHaveLength(count);
      expect(weights.reduce((total, value) => total + value, 0)).toBe(100);
      expect(weights.every((weight, index) => index === 0 || weights[index - 1] >= weight)).toBe(true);
    }
  });
});
