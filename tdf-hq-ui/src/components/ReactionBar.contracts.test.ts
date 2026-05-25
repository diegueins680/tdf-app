import { REACTION_BAR_CONTRACTS } from './ReactionBar';

const EXPECTED_REACTION_BAR_CONTRACTS = {
  activeCountFontWeight: 7 * 100,
  inactiveCountFontWeight: 4 * 100,
  loadingSpinnerSizePx: 4 * 4,
} as const;

describe('ReactionBar contracts', () => {
  it('preserves the named count font-weight contracts', () => {
    expect(REACTION_BAR_CONTRACTS).toEqual(EXPECTED_REACTION_BAR_CONTRACTS);
  });

  it('keeps active reaction counts heavier than inactive counts', () => {
    expect(REACTION_BAR_CONTRACTS.activeCountFontWeight).toBeGreaterThan(
      REACTION_BAR_CONTRACTS.inactiveCountFontWeight,
    );

    for (const value of Object.values(REACTION_BAR_CONTRACTS)) {
      expect(Number.isInteger(value)).toBe(true);
      expect(value).toBeGreaterThan(0);
    }

    expect(REACTION_BAR_CONTRACTS.loadingSpinnerSizePx).toBeLessThan(
      REACTION_BAR_CONTRACTS.activeCountFontWeight,
    );
  });
});
