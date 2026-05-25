import { NOTIFICATION_BELL_CONTRACTS } from './NotificationBell.contracts';

const EXPECTED_PANEL_LOADING_SPINNER_RADIUS_PX = 11;

const EXPECTED_NOTIFICATION_BELL_CONTRACTS = {
  countRefetchIntervalMs: 3 * 10 * 1000,
  badgeMaxDisplayCount: 100 - 1,
  popoverPaperWidthPx: 3 * 100 + 6 * 10,
  popoverPaperMaxHeightPx: 5 * 100 - 2 * 10,
  headingFontWeight: 7 * 100,
  notificationListMaxHeightPx: 4 * 100 - 2 * 10,
  triggerLoadingSpinnerSizePx: 2 * 10,
  markAllActionSpinnerSizePx: 4 * 4,
  panelLoadingSpinnerSizePx: 2 * EXPECTED_PANEL_LOADING_SPINNER_RADIUS_PX,
} as const;

describe('NotificationBell contracts', () => {
  it('preserves the named timing, display, and layout contracts', () => {
    expect(NOTIFICATION_BELL_CONTRACTS).toEqual(EXPECTED_NOTIFICATION_BELL_CONTRACTS);
  });

  it('keeps positive integer dimensions and a bounded scroll region', () => {
    for (const value of Object.values(NOTIFICATION_BELL_CONTRACTS)) {
      expect(Number.isInteger(value)).toBe(true);
      expect(value).toBeGreaterThan(0);
    }

    expect(NOTIFICATION_BELL_CONTRACTS.notificationListMaxHeightPx).toBeLessThan(
      NOTIFICATION_BELL_CONTRACTS.popoverPaperMaxHeightPx,
    );

    expect(NOTIFICATION_BELL_CONTRACTS.markAllActionSpinnerSizePx).toBeLessThan(
      NOTIFICATION_BELL_CONTRACTS.triggerLoadingSpinnerSizePx,
    );
    expect(NOTIFICATION_BELL_CONTRACTS.triggerLoadingSpinnerSizePx).toBeLessThanOrEqual(
      NOTIFICATION_BELL_CONTRACTS.panelLoadingSpinnerSizePx,
    );
  });
});
