export type RefundStatusColor = 'default' | 'warning' | 'success' | 'error';

const REFUND_STATUS_COLORS: Record<string, RefundStatusColor> = {
  approved: 'success',
  pending: 'warning',
  processed: 'success',
  rejected: 'error',
};

export function getRefundStatusColor(status: string): RefundStatusColor {
  return REFUND_STATUS_COLORS[status] ?? 'default';
}
