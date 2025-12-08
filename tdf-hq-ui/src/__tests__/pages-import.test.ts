import MarketplacePage from '../pages/MarketplacePage';
import BookingsPage from '../pages/BookingsPage';
import TrialLessonsPage from '../pages/TrialLessonsPage';

describe('page modules import', () => {
  it('MarketplacePage imports', () => {
    expect(typeof MarketplacePage).toBe('function');
  });

  it('BookingsPage imports', () => {
    expect(typeof BookingsPage).toBe('function');
  });

  it('TrialLessonsPage imports', () => {
    expect(typeof TrialLessonsPage).toBe('function');
  });
});
