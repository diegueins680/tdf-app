import {
  applyMarketplaceOrderPreset,
  formatLastSavedTimestamp,
  getMarketplacePaymentProviderLabel,
  getOrderStatusMeta,
  isPaidOrderStatus,
  summarizeMarketplaceOrderList,
} from './marketplace';

describe('getOrderStatusMeta', () => {
  it('marks paid statuses as paid', () => {
    const paid = getOrderStatusMeta('paid');
    expect(paid.label).toBe('Pagado');
    expect(paid.color).toBe('success');
    expect(paid.desc.toLowerCase()).toContain('confirmado');
  });

  it('recognizes alternate paid variants through the shared parser', () => {
    expect(isPaidOrderStatus('approved')).toBe(true);
    expect(isPaidOrderStatus('completed')).toBe(true);
    expect(isPaidOrderStatus('successful')).toBe(true);
    expect(isPaidOrderStatus('datafast_paid')).toBe(true);
    expect(isPaidOrderStatus('paypal_approved')).toBe(true);
    expect(isPaidOrderStatus('datafast_pending')).toBe(false);
    expect(isPaidOrderStatus('delivered')).toBe(false);
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

  it('treats not-paid variants as rejected instead of paid', () => {
    expect(getOrderStatusMeta('not_paid').label.toLowerCase()).toContain('rechazado');
    expect(getOrderStatusMeta('un-paid').label.toLowerCase()).toContain('rechazado');
    expect(getOrderStatusMeta('paypal_not_paid').label.toLowerCase()).toContain('rechazado');
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

describe('applyMarketplaceOrderPreset', () => {
  it('returns a clean date-only quick filter for the last 7 days preset', () => {
    expect(applyMarketplaceOrderPreset('last7', new Date(2026, 2, 16, 12, 0, 0))).toEqual({
      statusFilter: 'all',
      providerFilter: 'all',
      fromDate: '2026-03-09',
      toDate: '',
      search: '',
      paidOnly: false,
    });
  });

  it('clears unrelated filters when applying payment presets', () => {
    expect(applyMarketplaceOrderPreset('paid')).toEqual({
      statusFilter: 'paid',
      providerFilter: 'all',
      fromDate: '',
      toDate: '',
      search: '',
      paidOnly: false,
    });
    expect(applyMarketplaceOrderPreset('paypal')).toEqual({
      statusFilter: 'all',
      providerFilter: 'paypal',
      fromDate: '',
      toDate: '',
      search: '',
      paidOnly: false,
    });
    expect(applyMarketplaceOrderPreset('card')).toEqual({
      statusFilter: 'datafast_pending',
      providerFilter: 'datafast',
      fromDate: '',
      toDate: '',
      search: '',
      paidOnly: false,
    });
  });
});

describe('getMarketplacePaymentProviderLabel', () => {
  it('formats known provider names for human-readable filter chips', () => {
    expect(getMarketplacePaymentProviderLabel('paypal')).toBe('PayPal');
    expect(getMarketplacePaymentProviderLabel('datafast')).toBe('Tarjeta (Datafast)');
    expect(getMarketplacePaymentProviderLabel('contact')).toBe('Manual/otros');
    expect(getMarketplacePaymentProviderLabel(' wire_transfer ')).toBe('wire_transfer');
  });
});

describe('summarizeMarketplaceOrderList', () => {
  it('summarizes the full list cleanly when no filters are active', () => {
    expect(
      summarizeMarketplaceOrderList({
        totalOrders: 12,
        visibleOrders: 12,
        activeFilterCount: 0,
      }),
    ).toBe('Mostrando 12 órdenes.');
  });

  it('explains the narrowed scope when filters are active', () => {
    expect(
      summarizeMarketplaceOrderList({
        totalOrders: 12,
        visibleOrders: 3,
        activeFilterCount: 2,
      }),
    ).toBe('Mostrando 3 de 12 órdenes. 2 filtros activos.');
  });

  it('clamps invalid counts so the summary never overstates visible orders', () => {
    expect(
      summarizeMarketplaceOrderList({
        totalOrders: 2,
        visibleOrders: 5,
        activeFilterCount: Number.NaN,
      }),
    ).toBe('Mostrando 2 órdenes.');
  });
});
