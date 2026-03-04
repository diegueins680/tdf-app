import { formatLastSavedTimestamp, getOrderStatusMeta } from './marketplace';

describe('getOrderStatusMeta', () => {
  it('marks paid statuses as paid', () => {
    const paid = getOrderStatusMeta('paid');
    expect(paid.label).toBe('Pagado');
    expect(paid.color).toBe('success');
    expect(paid.desc.toLowerCase()).toContain('confirmado');
  });

  it('keeps pending statuses pending (including paypal_pending)', () => {
    expect(getOrderStatusMeta('pending').color).toBe('warning');
    expect(getOrderStatusMeta('paypal_pending').color).toBe('warning');
    expect(getOrderStatusMeta('paypal_pending').label).toBe('Pendiente');
  });

  it('handles datafast statuses', () => {
    expect(getOrderStatusMeta('datafast_init').color).toBe('info');
    expect(getOrderStatusMeta('datafast_init').label).toContain('iniciado');
    expect(getOrderStatusMeta('datafast_pending').color).toBe('warning');
    expect(getOrderStatusMeta('datafast_failed').label.toLowerCase()).toContain('rechazado');
  });

  it('classifies paypal failures and refunds correctly', () => {
    expect(getOrderStatusMeta('paypal_failed').label.toLowerCase()).toContain('rechazado');
    expect(getOrderStatusMeta('paypal_declined').label.toLowerCase()).toContain('rechazado');
    expect(getOrderStatusMeta('paypal_refunded').label).toBe('Reembolsado');
  });

  it('classifies datafast refunds as refunded instead of in-review', () => {
    const refunded = getOrderStatusMeta('datafast_refunded');
    expect(refunded.label).toBe('Reembolsado');
    expect(refunded.color).toBe('default');
  });

  it('does not classify unpaid as paid by substring match', () => {
    const meta = getOrderStatusMeta('unpaid');
    expect(meta.label).toContain('rechazado');
    expect(meta.color).toBe('default');
  });

  it('falls back to process/cancel/delivered mapping', () => {
    expect(getOrderStatusMeta('processing').color).toBe('info');
    expect(getOrderStatusMeta('cancelled').color).toBe('default');
    expect(getOrderStatusMeta('delivered').color).toBe('success');
  });

  it('returns a readable fallback for unknown statuses', () => {
    const meta = getOrderStatusMeta('custom_status');
    expect(meta.label).toBe('custom_status');
    expect(meta.color).toBe('default');
  });

  it('trims unknown status labels before using fallback text', () => {
    const meta = getOrderStatusMeta('  custom_status  ');
    expect(meta.label).toBe('custom_status');
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

  it('handles future timestamps without negative values', () => {
    const now = Date.now();
    expect(formatLastSavedTimestamp(now + 60_000)).toContain('<1 min');
  });

  it('ignores invalid/non-positive timestamp values', () => {
    expect(formatLastSavedTimestamp(0)).toBeNull();
    expect(formatLastSavedTimestamp(Number.NaN)).toBeNull();
    expect(formatLastSavedTimestamp(Number.POSITIVE_INFINITY)).toBeNull();
  });
});
