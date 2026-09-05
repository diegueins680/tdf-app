import { jest } from '@jest/globals';
import { createElement } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReputationCategory, ReputationPreference } from '../../api/reputation';

const categoriesMock = jest.fn<() => Promise<ReputationCategory[]>>();
const getPreferencesMock = jest.fn<() => Promise<ReputationPreference>>();
const savePreferencesMock = jest.fn();

jest.unstable_mockModule('../../api/reputation', () => ({
  Reputation: {
    categories: categoriesMock,
    getMyPreferences: getPreferencesMock,
    saveMyPreferences: savePreferencesMock,
  },
}));

jest.unstable_mockModule('../../session/SessionContext', () => ({
  useSession: () => ({
    session: { featureFlags: ['CONTEXTUAL_REPUTATION_ENABLED'] },
  }),
}));

const {
  default: CategoryPriorityPrototype,
  orderCategoriesByPreference,
  rankOrderCentroid,
  reorderCategories,
} = await import('./CategoryPriorityPrototype');

const categories: ReputationCategory[] = [
  { id: 'a', slug: 'quality', name: 'Quality', description: '', defaultPosition: 2, institutionalWeight: 20, version: 1 },
  { id: 'b', slug: 'communication', name: 'Communication', description: '', defaultPosition: 1, institutionalWeight: 20, version: 1 },
  { id: 'c', slug: 'punctuality', name: 'Punctuality', description: '', defaultPosition: 3, institutionalWeight: 20, version: 1 },
];

const emptyPreference: ReputationPreference = {
  contextKind: 'general',
  status: 'draft',
  revision: 0,
  formulaVersion: 'public-bayes-roc-v1',
  categories: [],
};

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
    const ordered = orderCategoriesByPreference(categories, {
      contextKind: 'general', status: 'active', revision: 2, formulaVersion: 'public-bayes-roc-v1',
      categories: [{ categoryId: 'a', slug: 'quality', position: 1, weight: 60, notApplicable: false }],
    });

    expect(ordered.map(({ id }) => id)).toEqual(['a', 'b', 'c']);
  });

  it('reorders categories deterministically without mutating the current order', () => {
    const categoriesInDefaultOrder = [...categories].sort((left, right) => left.defaultPosition - right.defaultPosition);

    const reordered = reorderCategories(categoriesInDefaultOrder, 2, 0);

    expect(reordered.map(({ id }) => id)).toEqual(['c', 'b', 'a']);
    expect(categoriesInDefaultOrder.map(({ id }) => id)).toEqual(['b', 'a', 'c']);
    expect(reorderCategories(categoriesInDefaultOrder, -1, 1)).toBe(categoriesInDefaultOrder);
  });
});

describe('CategoryPriorityPrototype preference loading', () => {
  beforeEach(() => {
    categoriesMock.mockReset().mockResolvedValue(categories);
    getPreferencesMock.mockReset()
      .mockRejectedValueOnce(new Error('preference unavailable'))
      .mockResolvedValueOnce(emptyPreference);
    savePreferencesMock.mockReset();
  });

  it('does not expose the editor or revision-zero save path until a failed preference query is retried', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const view = render(createElement(
      QueryClientProvider,
      { client: queryClient },
      createElement(CategoryPriorityPrototype, { locale: 'en' }),
    ));

    try {
      expect(await screen.findByText('Your saved preference could not be loaded. Try again before editing.')).toBeTruthy();
      expect(screen.queryByRole('button', { name: 'Save draft' })).toBeNull();

      fireEvent.click(screen.getByRole('button', { name: 'Try again' }));

      expect(await screen.findByRole('button', { name: 'Save draft' })).toBeTruthy();
      expect(getPreferencesMock).toHaveBeenCalledTimes(2);
      expect(savePreferencesMock).not.toHaveBeenCalled();
    } finally {
      view.unmount();
      queryClient.clear();
    }
  });
});
