import { nextInvoiceIssueAttempt, type GenerateSessionInvoiceInput } from './invoices';

const payload: GenerateSessionInvoiceInput = {
  customerId: 9,
  currency: 'USD',
  lineItems: [{ description: 'Mezcla', quantity: 1, unitCents: 1000, taxBps: 1500 }],
  issueSri: true,
};

describe('nextInvoiceIssueAttempt (PAY-INVOICE-ISSUE-001)', () => {
  const keys = () => {
    let n = 0;
    return () => `key-${(n += 1)}`;
  };

  it('keeps the key while the request is unchanged, so a resubmission replays', () => {
    const newKey = keys();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey);
    const retry = nextInvoiceIssueAttempt(first, 'session-1', { ...payload }, newKey);
    expect(retry.key).toBe(first.key);
  });

  it('ignores the certificate password and an already attached key', () => {
    const newKey = keys();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey);
    const retry = nextInvoiceIssueAttempt(
      first,
      'session-1',
      { ...payload, certificatePassword: 'secret', idempotencyKey: first.key },
      newKey,
    );
    expect(retry.key).toBe(first.key);
  });

  it('uses a new key for an edited request or another session', () => {
    const newKey = keys();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey);
    const edited = nextInvoiceIssueAttempt(
      first,
      'session-1',
      { ...payload, lineItems: [{ description: 'Mezcla', quantity: 2, unitCents: 1000, taxBps: 1500 }] },
      newKey,
    );
    expect(edited.key).not.toBe(first.key);
    const otherSession = nextInvoiceIssueAttempt(edited, 'session-2', payload, newKey);
    expect(otherSession.key).not.toBe(edited.key);
  });
});
