import { orderCategoriesByPreference, rankOrderCentroid } from './CategoryPriorityPrototype';

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

  it('rehydrates only the caller saved order and keeps new categories available', () => {
    const categories = [
      { id: 'a', slug: 'quality', name: 'Quality', description: '', defaultPosition: 2, institutionalWeight: 20, version: 1 },
      { id: 'b', slug: 'communication', name: 'Communication', description: '', defaultPosition: 1, institutionalWeight: 20, version: 1 },
      { id: 'c', slug: 'punctuality', name: 'Punctuality', description: '', defaultPosition: 3, institutionalWeight: 20, version: 1 },
    ];

    const ordered = orderCategoriesByPreference(categories, {
      contextKind: 'general', status: 'active', revision: 2, formulaVersion: 'public-bayes-roc-v1',
      categories: [{ categoryId: 'a', slug: 'quality', position: 1, weight: 60, notApplicable: false }],
    });

    expect(ordered.map(({ id }) => id)).toEqual(['a', 'b', 'c']);
  });
});
