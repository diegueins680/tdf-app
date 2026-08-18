export const makeDomoQuoteIdempotencyKey = (): string => {
  if (typeof crypto !== 'undefined' && typeof crypto.randomUUID === 'function') {
    return `domo-quote-${crypto.randomUUID()}`;
  }
  throw new Error('Secure browser randomness is required to create a Domo quote.');
};

const storageKey = (quoteId: string) => `tdf:domo-quote:${quoteId}`;

export const saveDomoQuoteLookupToken = (quoteId: string, lookupToken: string) => {
  try {
    window.localStorage.setItem(storageKey(quoteId), lookupToken);
  } catch {
    // The creation response remains usable if browser storage is unavailable.
  }
};

export const loadDomoQuoteLookupToken = (quoteId: string): string | null => {
  try {
    return window.localStorage.getItem(storageKey(quoteId));
  } catch {
    return null;
  }
};
