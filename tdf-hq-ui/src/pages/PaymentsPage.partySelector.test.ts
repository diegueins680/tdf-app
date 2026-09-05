import { readFileSync } from 'node:fs';

describe('payment Party relationships', () => {
  const paymentsSource = readFileSync(new URL('./PaymentsPage.tsx', import.meta.url), 'utf8');
  const invoiceSource = readFileSync(
    new URL('../components/SessionInvoiceGeneratorCard.tsx', import.meta.url),
    'utf8',
  );

  it('uses server-backed selectors without downloading the Party directory', () => {
    expect(paymentsSource).toContain('<PartySelector');
    expect(invoiceSource).toContain('<PartySelector');
    expect(paymentsSource).not.toMatch(/Parties\.list\s*\(/);
    expect(invoiceSource).not.toMatch(/Parties\.list\s*\(/);
  });

  it('persists canonical IDs and renders the payment-provided display name', () => {
    expect(paymentsSource).toContain('pcPartyId: parsedPartyId');
    expect(invoiceSource).toContain('customerId: selectedCustomer?.partyId ?? undefined');
    expect(paymentsSource).toContain('pay.payPartyDisplayName');
    expect(paymentsSource).not.toContain('ID {pay.payPartyId}');
  });

  it('allows both people and organizations as billing contacts', () => {
    expect(paymentsSource.match(/context: 'billing_contact', kind: 'any', accountOnly: false/g)).toHaveLength(2);
  });
});
