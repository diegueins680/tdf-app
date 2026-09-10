import { buildMerchOrdersCsv, type MerchOrderExportRow } from './merch';

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
});
