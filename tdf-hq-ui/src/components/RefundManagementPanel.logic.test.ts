import { getRefundStatusColor } from './RefundManagementPanel.logic';

describe('getRefundStatusColor', () => {
  it('maps refund statuses without relying on switch fallthrough', () => {
    expect(getRefundStatusColor('pending')).toBe('warning');
    expect(getRefundStatusColor('approved')).toBe('success');
    expect(getRefundStatusColor('processed')).toBe('success');
    expect(getRefundStatusColor('rejected')).toBe('error');
    expect(getRefundStatusColor('unexpected')).toBe('default');
  });
});
