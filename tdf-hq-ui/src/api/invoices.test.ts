import {
  forgetInvoiceIssueAttempt,
  invoiceIssueSettled,
  nextInvoiceIssueAttempt,
  type GenerateSessionInvoiceInput,
} from './invoices';

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
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey, null);
    const retry = nextInvoiceIssueAttempt(first, 'session-1', { ...payload }, newKey, null);
    expect(retry.key).toBe(first.key);
  });

  it('ignores the certificate password and an already attached key', () => {
    const newKey = keys();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey, null);
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
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, newKey, null);
    const edited = nextInvoiceIssueAttempt(
      first,
      'session-1',
      { ...payload, lineItems: [{ description: 'Mezcla', quantity: 2, unitCents: 1000, taxBps: 1500 }] },
      newKey,
    );
    expect(edited.key).not.toBe(first.key);
    const otherSession = nextInvoiceIssueAttempt(edited, 'session-2', payload, newKey, null);
    expect(otherSession.key).not.toBe(edited.key);
  });
});

describe('invoiceIssueSettled (PAY-INVOICE-ISSUE-001)', () => {
  const base = { ok: true, sessionId: 's', invoice: {} as never };

  it('settles when SRI was not requested or reported the document as issued', () => {
    expect(invoiceIssueSettled({ ...base, sri: null })).toBe(true);
    expect(invoiceIssueSettled({ ...base })).toBe(true);
    expect(invoiceIssueSettled({ ...base, sri: { ok: true, status: 'issued' } })).toBe(true);
  });

  it('keeps the attempt open after an SRI error or an unconfirmed status', () => {
    expect(invoiceIssueSettled({ ...base, sri: { ok: false, error: 'No se pudo leer la confirmación' } })).toBe(false);
    expect(invoiceIssueSettled({ ...base, sri: { ok: true, status: 'received' } })).toBe(false);
    expect(invoiceIssueSettled({ ...base, sri: { ok: false, status: 'issued' } })).toBe(false);
  });
});

describe('unresolved attempts survive a reload (PAY-INVOICE-ISSUE-001)', () => {
  const memoryStorage = () => {
    const values = new Map<string, string>();
    return {
      getItem: (key: string) => values.get(key) ?? null,
      setItem: (key: string, value: string) => {
        values.set(key, value);
      },
    };
  };

  it('restores the stored key for the same request after the in-memory attempt is lost', () => {
    const storage = memoryStorage();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, () => 'key-before-reload', storage);
    const afterReload = nextInvoiceIssueAttempt(null, 'session-1', { ...payload }, () => 'key-after-reload', storage);
    expect(afterReload.key).toBe(first.key);
  });

  it('forgets a settled attempt, so the next request gets a new key', () => {
    const storage = memoryStorage();
    const first = nextInvoiceIssueAttempt(null, 'session-1', payload, () => 'first-key', storage);
    forgetInvoiceIssueAttempt(first, storage);
    const next = nextInvoiceIssueAttempt(null, 'session-1', payload, () => 'second-key', storage);
    expect(next.key).toBe('second-key');
  });

  it('keeps working when storage is unavailable or unreadable', () => {
    const broken = {
      getItem: () => {
        throw new Error('blocked');
      },
      setItem: () => {
        throw new Error('blocked');
      },
    };
    expect(nextInvoiceIssueAttempt(null, 'session-1', payload, () => 'k', broken).key).toBe('k');
    expect(nextInvoiceIssueAttempt(null, 'session-1', payload, () => 'k2', null).key).toBe('k2');
  });
});
