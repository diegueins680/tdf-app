import { get, post } from './client';
import type { components } from './generated/types';
import type { DatafastCheckoutDTO, PaypalCreateDTO } from './types';

export type PublicDomoStorefront = components['schemas']['PublicDomoStorefront'];
export type PublicDomoQuoteCreateRequest = components['schemas']['PublicDomoQuoteCreateRequest'];
export type PublicDomoQuote = components['schemas']['PublicDomoQuote'];

const quoteBase = (quoteId: string): string => {
  const normalized = quoteId.trim();
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(normalized)) {
    throw new Error('quoteId must be a UUID.');
  }
  return `/public/domo/quotes/${normalized}`;
};

const lookupHeaders = (lookupToken: string): RequestInit => ({
  headers: { 'X-Order-Lookup-Token': lookupToken },
});

export const DomoQuotes = {
  getStorefront: () => get<PublicDomoStorefront>('/public/domo'),
  createQuote: (payload: PublicDomoQuoteCreateRequest, idempotencyKey: string) =>
    post<PublicDomoQuote>('/public/domo/quotes', payload, {
      headers: { 'Idempotency-Key': idempotencyKey },
    }),
  getQuote: (quoteId: string, lookupToken: string) =>
    get<PublicDomoQuote>(quoteBase(quoteId), lookupHeaders(lookupToken)),
  acceptQuote: (quoteId: string, lookupToken: string) =>
    post<PublicDomoQuote>(
      `${quoteBase(quoteId)}/accept`,
      { termsAccepted: true },
      lookupHeaders(lookupToken),
    ),
  createDatafastCheckout: (quoteId: string, lookupToken: string) =>
    post<DatafastCheckoutDTO>(
      `${quoteBase(quoteId)}/datafast/checkout`,
      {},
      lookupHeaders(lookupToken),
    ),
  confirmDatafastStatus: (quoteId: string, resourcePath: string, lookupToken: string) => {
    const search = new URLSearchParams({ resourcePath });
    return get<PublicDomoQuote>(
      `${quoteBase(quoteId)}/datafast/status?${search.toString()}`,
      lookupHeaders(lookupToken),
    );
  },
  createPaypalOrder: (quoteId: string, lookupToken: string) =>
    post<PaypalCreateDTO>(
      `${quoteBase(quoteId)}/paypal/create`,
      {},
      lookupHeaders(lookupToken),
    ),
  capturePaypalOrder: (
    quoteId: string,
    paypalOrderId: string,
    lookupToken: string,
  ) => post<PublicDomoQuote>(
    `${quoteBase(quoteId)}/paypal/capture`,
    { paypalOrderId },
    lookupHeaders(lookupToken),
  ),
};
