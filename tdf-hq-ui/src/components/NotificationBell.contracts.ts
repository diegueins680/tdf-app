type NotificationBellContract = Readonly<{
  countRefetchIntervalMs: number;
  badgeMaxDisplayCount: number;
  popoverPaperWidthPx: number;
  popoverPaperMaxHeightPx: number;
  headingFontWeight: number;
  notificationListMaxHeightPx: number;
  triggerLoadingSpinnerSizePx: number;
  markAllActionSpinnerSizePx: number;
  panelLoadingSpinnerSizePx: number;
}>;

const PANEL_LOADING_SPINNER_RADIUS_PX = 11;

// Invariant: all dimensions and limits are positive integers; the scrollable list
// remains shorter than its containing popover paper; action spinners remain
// smaller than the larger loading indicators.
export const NOTIFICATION_BELL_CONTRACTS = {
  countRefetchIntervalMs: 3 * 10 * 1000,
  badgeMaxDisplayCount: 100 - 1,
  popoverPaperWidthPx: 3 * 100 + 6 * 10,
  popoverPaperMaxHeightPx: 5 * 100 - 2 * 10,
  headingFontWeight: 7 * 100,
  notificationListMaxHeightPx: 4 * 100 - 2 * 10,
  triggerLoadingSpinnerSizePx: 2 * 10,
  markAllActionSpinnerSizePx: 4 * 4,
  panelLoadingSpinnerSizePx: 2 * PANEL_LOADING_SPINNER_RADIUS_PX,
} as const satisfies NotificationBellContract;
