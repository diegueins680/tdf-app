import { internalReportMutationsAllowed } from './internalFeedbackLogic';

describe('internal report mutation controls', () => {
  it('allows changes while the owning audit is active', () => {
    expect(internalReportMutationsAllowed(true)).toBe(true);
  });

  it('makes reports read-only after the owning audit is finalized', () => {
    expect(internalReportMutationsAllowed(false)).toBe(false);
  });
});
