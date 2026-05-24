export type RefundStatusColor = 'default' | 'warning' | 'success' | 'error';

const REFUND_STATUS_COLORS: Readonly<Record<string, RefundStatusColor>> = {
  approved: 'success',
  pending: 'warning',
  processed: 'success',
  rejected: 'error',
};

export function getRefundStatusColor(status: string): RefundStatusColor {
  /*
   * precondition: status is backend refund status text.
   * invariant: unknown statuses stay neutral.
   * postcondition: returns a valid Chip color.
   */
  return REFUND_STATUS_COLORS[status] ?? 'default';
}
