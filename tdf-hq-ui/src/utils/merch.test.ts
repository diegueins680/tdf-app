import { buildMerchOrdersCsv, formatMerchMoney, merchStatusLabel, readMerchMoneyInput, type MerchOrderExportRow } from './merch';

const order = (overrides: Partial<MerchOrderExportRow> = {}): MerchOrderExportRow => ({
  orderNumber: 'TDF-MERCH-100',
  createdAt: '2026-09-09T14:00:00Z',
  commercialStatus: 'confirmed',
  paymentStatus: 'paid',
  fulfillmentStatus: 'preparing',
  refundStatus: 'none',
  disputeStatus: 'none',
  settlementStatus: 'ready',
  currency: 'USD',
  productSubtotalMinor: 2500,
  taxMinor: 0,
  shippingMinor: 500,
  totalMinor: 3000,
  ...overrides,
});

describe('buildMerchOrdersCsv', () => {
  it('exports operational fields without customer personal data', () => {
    const csv = buildMerchOrdersCsv([order()], 'es');

    expect(csv).toContain('"Pedido"');
    expect(csv).toContain('"TDF-MERCH-100"');
    expect(csv).not.toMatch(/nombre|correo|tel[eé]fono|direcci[oó]n/i);
    expect(csv).not.toContain('Comisión TDF');
  });

  it('includes commission and seller net only when finance data was authorized', () => {
    const csv = buildMerchOrdersCsv([order({ tdfCommissionMinor: 250, sellerNetMinor: 2750 })], 'en');

    expect(csv).toContain('"TDF commission (minor)"');
    expect(csv).toContain('"Seller net (minor)"');
    expect(csv).toContain('"250","2750"');
  });

  it('neutralizes spreadsheet formulas in user-influenced identifiers', () => {
    const csv = buildMerchOrdersCsv([order({ orderNumber: '=HYPERLINK("https://evil.test")' })], 'en');

    expect(csv).toContain('"\'=HYPERLINK(""https://evil.test"")"');
    expect(csv).not.toContain('"=HYPERLINK');
  });
  it('preserves typed negative minor amounts while protecting negative-looking string identifiers', () => {
    const csv = buildMerchOrdersCsv([order({
      orderNumber: '-1+1', totalMinor: -3000, tdfCommissionMinor: -250, sellerNetMinor: -2750,
    })], 'en');

    expect(csv).toContain('"\'-1+1"');
    expect(csv).toContain('"-3000","-250","-2750"');
    expect(csv).not.toContain('"\'-3000"');
  });
});


describe('merch currency amounts', () => {
  it('preserves the last cent and the explicitly requested locale when displaying large amounts', () => {
    expect(formatMerchMoney(Number.MAX_SAFE_INTEGER, 'USD', 'en-US')).toBe('$90,071,992,547,409.91');
    expect(formatMerchMoney(-1, 'USD', 'en-US')).toBe('-$0.01');
    expect(formatMerchMoney(0, 'USD', 'en-US')).toBe('$0.00');
    expect(formatMerchMoney(2550, 'EUR', 'de-DE')).toBe('25,50\u00a0€');
  });
  it('distinguishes optional full-balance blank, required blank, free delivery and positive prices', () => {
    expect(readMerchMoneyInput('', 'en')).toEqual({ minor: null, error: null });
    expect(readMerchMoneyInput('', 'en', { required: true }).error).toBe('Enter an amount.');
    expect(readMerchMoneyInput('0', 'es', { required: true })).toEqual({ minor: 0, error: null });
    expect(readMerchMoneyInput('0', 'en', { positive: true }).error).toMatch(/greater than zero/);
    expect(readMerchMoneyInput('25,50', 'es')).toEqual({ minor: 2550, error: null });
  });
  it.each(['25.505', '-1', '1e2', '90071992547409.92'])('rejects %s without rounding or interpreting it as blank', value => {
    expect(readMerchMoneyInput(value, 'es').error).toMatch(/dos decimales/);
    expect(readMerchMoneyInput(value, 'en').error).toMatch(/two decimals/);
  });
});


describe('merch status vocabulary', () => {
  it('keeps request approval distinct from payment and completed refunds in both languages', () => {
    for (const language of ['es', 'en'] as const) {
      const labels = ['requested', 'approved', 'paid', 'partially_refunded', 'refunded', 'processing', 'held'].map(status => merchStatusLabel(status, language));
      expect(new Set(labels).size).toBe(labels.length);
    }
  });
  it('preserves unknown status information and machine-readable export values', () => {
    expect(merchStatusLabel('future_provider_state', 'es')).toBe('future provider state');
    expect(buildMerchOrdersCsv([order({ paymentStatus: 'partially_refunded', refundStatus: 'none' })], 'es')).toContain('"partially_refunded"');
  });
});
