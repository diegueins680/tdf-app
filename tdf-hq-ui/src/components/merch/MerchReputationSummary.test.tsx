import { render, screen } from '@testing-library/react';
import { MerchReputationSummary } from './MerchReputationSummary';

describe('MerchReputationSummary', () => {
  it('labels store reputation as commercial and does not fabricate a new-store score', () => {
    render(<MerchReputationSummary summary={{
      subjectKind: 'store',
      state: 'new_store',
      rating: null,
      verifiedReviewCount: 0,
      confidence: 'new',
      objectiveSignals: { identityVerified: true },
    }} />);

    expect(screen.getByText('Reputación comercial')).toBeTruthy();
    expect(screen.getByText('Tienda nueva')).toBeTruthy();
    expect(screen.queryByText(/★ \/ 5/)).toBeNull();
    expect(screen.getByText('Identidad verificada')).toBeTruthy();
  });

  it('keeps the product label separate from store reputation', () => {
    render(<MerchReputationSummary summary={{
      subjectKind: 'product',
      state: 'published',
      rating: 4.2,
      verifiedPurchaseReviewCount: 8,
      confidence: 'limited',
    }} />);

    expect(screen.getByLabelText('Valoración del producto')).toBeTruthy();
    expect(screen.queryByText('Reputación comercial')).toBeNull();
    expect(screen.getByText('8 evaluaciones verificadas')).toBeTruthy();
  });
});
