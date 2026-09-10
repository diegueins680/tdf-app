import { jest } from '@jest/globals';
import { createElement } from 'react';
import { render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { PublicReputation } from '../../api/reputation';
import { expectNoSeriousAccessibilityViolations } from '../../test/accessibility';
import i18n from '../../i18n';

const getPublicMock = jest.fn<() => Promise<PublicReputation>>();

jest.unstable_mockModule('../../api/reputation', () => ({
  Reputation: { getPublic: getPublicMock },
}));

const { default: PublicReputationSummary } = await import('./PublicReputationSummary');

const publishedReputation: PublicReputation = {
  partyId: 42,
  formulaVersion: 'public-bayes-roc-v1',
  status: 'published',
  score: 82,
  verifiedInteractions: 12,
  confidence: 'moderate',
  categories: [{
    slug: 'communication', score: 84, lowerBound: 70, upperBound: 92,
    verifiedCount: 12, confidence: 'moderate',
  }],
};

describe('PublicReputationSummary', () => {
  beforeEach(async () => {
    await i18n.changeLanguage('es');
    getPublicMock.mockReset().mockResolvedValue(publishedReputation);
  });

  it('renders public aggregates without reviewer-level data and has no serious accessibility violations', async () => {
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const view = render(createElement(
      QueryClientProvider,
      { client: queryClient },
      createElement(PublicReputationSummary, { partyId: 42 }),
    ));

    try {
      await screen.findByRole('heading', { name: 'Reputación verificada' });
      expect(screen.getByText('82')).toBeTruthy();
      expect(screen.getByLabelText('communication: 84 de 100')).toBeTruthy();
      expect(view.container.textContent).not.toContain('reviewer');
      await expectNoSeriousAccessibilityViolations(view.container);
    } finally {
      view.unmount();
      queryClient.clear();
    }
  });

  it('renders public aggregate labels in English', async () => {
    await i18n.changeLanguage('en');
    const queryClient = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    const view = render(createElement(
      QueryClientProvider,
      { client: queryClient },
      createElement(PublicReputationSummary, { partyId: 42 }),
    ));

    try {
      await screen.findByRole('heading', { name: 'Verified reputation' });
      expect(screen.getByText('12 verified interactions')).toBeTruthy();
      expect(screen.getByLabelText('communication: 84 out of 100')).toBeTruthy();
    } finally {
      view.unmount();
      queryClient.clear();
      await i18n.changeLanguage('es');
    }
  });
});
