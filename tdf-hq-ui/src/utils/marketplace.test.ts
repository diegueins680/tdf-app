import { formatLastSavedTimestamp, getOrderStatusMeta } from './marketplace';

describe('getOrderStatusMeta', () => {
  it('marks paid/paypal statuses as paid', () => {
    const paid = getOrderStatusMeta('paid');
    expect(paid.label).toBe('Pagado');
    expect(paid.color).toBe('success');

    const paypal = getOrderStatusMeta('paypal_pending');
    expect(paypal.label).toBe('Pagado');
  });

  it('falls back to pending/process/cancel mapping', () => {
    expect(getOrderStatusMeta('pending').color).toBe('warning');
    expect(getOrderStatusMeta('processing').color).toBe('info');
    expect(getOrderStatusMeta('cancelled').color).toBe('default');
    expect(getOrderStatusMeta('delivered').color).toBe('success');
  });
});

describe('formatLastSavedTimestamp', () => {
  it('returns null when no timestamp', () => {
    expect(formatLastSavedTimestamp()).toBeNull();
  });

  it('formats minutes and hours', () => {
    const now = Date.now();
    expect(formatLastSavedTimestamp(now - 30_000)).toContain('<1 min');
    expect(formatLastSavedTimestamp(now - 10 * 60 * 1000)).toContain('10 min');
    expect(formatLastSavedTimestamp(now - 2 * 60 * 60 * 1000)).toContain('2 h');
  });
});
