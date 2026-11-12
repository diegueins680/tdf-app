import type { ClassifiedApplication, DirectoryTaxonomyItem, PriceMode } from '../../api/directory';

// ADR 0134: pure helpers for the demand/offer marketplace UI. Kept free of
// React so they can be unit tested and shared with the mobile client later.

export const OPEN_OFFER_STATUSES = new Set<ClassifiedApplication['status']>([
  'submitted', 'viewed', 'shortlisted', 'countered_by_author', 'countered_by_applicant',
]);

export const ACCEPTED_OFFER_STATUSES = new Set<ClassifiedApplication['status']>([
  'accepted', 'conversation_open', 'converted',
]);

export type OfferActor = 'author' | 'applicant';

export function isOfferOpen(status: ClassifiedApplication['status']): boolean {
  return OPEN_OFFER_STATUSES.has(status);
}

/** Mirrors TDF.Directory.Policy.acceptAllowedFor. */
export function canAccept(status: ClassifiedApplication['status'], actor: OfferActor): boolean {
  if (actor === 'author') return ['submitted', 'viewed', 'shortlisted', 'countered_by_applicant'].includes(status);
  return status === 'countered_by_author';
}

/** Mirrors TDF.Directory.Policy.counterAllowedFor (fixed-price classifieds never admit counters). */
export function canCounter(status: ClassifiedApplication['status'], actor: OfferActor, priceMode?: PriceMode): boolean {
  if (priceMode === 'fixed') return false;
  return canAccept(status, actor);
}

export function canReject(status: ClassifiedApplication['status'], actor: OfferActor): boolean {
  return actor === 'author' && isOfferOpen(status);
}

export function canWithdraw(status: ClassifiedApplication['status'], actor: OfferActor): boolean {
  return actor === 'applicant' && (isOfferOpen(status) || status === 'accepted' || status === 'conversation_open');
}

/** Whose move it is, for the "waiting on you / waiting on them" cue. */
export function waitingOn(status: ClassifiedApplication['status']): OfferActor | null {
  if (status === 'countered_by_author') return 'applicant';
  if (['submitted', 'viewed', 'shortlisted', 'countered_by_applicant'].includes(status)) return 'author';
  return null;
}

/** Formats a minor-unit amount with the taxonomy currency (symbol + minor units). */
export function formatMinor(
  amountMinor: number | null | undefined,
  currencyId: string | null | undefined,
  currencies: DirectoryTaxonomyItem[] | undefined,
  locale = 'es',
): string | null {
  if (amountMinor === null || amountMinor === undefined) return null;
  const currency = currencies?.find((item) => item.id === currencyId);
  const minorUnits = currency?.minorUnits ?? 2;
  const value = amountMinor / 10 ** minorUnits;
  const formatted = new Intl.NumberFormat(locale, { minimumFractionDigits: minorUnits, maximumFractionDigits: minorUnits }).format(value);
  const symbol = currency?.symbol ?? currency?.code ?? '';
  return symbol ? `${symbol} ${formatted}`.trim() : formatted;
}

/** Converts a decimal string typed by the user into minor units; undefined when empty or invalid. */
export function moneyInputToMinor(value: string, minorUnits = 2): number | undefined {
  const trimmed = value.trim();
  if (!trimmed) return undefined;
  const number = Number(trimmed.replace(',', '.'));
  if (!Number.isFinite(number) || number < 0) return undefined;
  return Math.round(number * 10 ** minorUnits);
}

/** The reference price of a classified for the offer form (budget max in suggested/fixed modes). */
export function referenceAmount(classified: { priceMode?: PriceMode; budget?: { maxMinor?: number | null; currencyId?: string | null } | null } | undefined): { amountMinor: number; currencyId: string | null } | null {
  if (!classified || classified.priceMode === 'open' || !classified.budget?.maxMinor) return null;
  return { amountMinor: classified.budget.maxMinor, currencyId: classified.budget.currencyId ?? null };
}

/** Sort comparator used by the author's comparison board. */
export function sortOffers(offers: ClassifiedApplication[], sort: 'recent' | 'price_asc' | 'price_desc' | 'reputation'): ClassifiedApplication[] {
  const copy = [...offers];
  const amount = (offer: ClassifiedApplication) => offer.currentAmountMinor ?? offer.proposedAmountMinor ?? null;
  copy.sort((a, b) => {
    if (sort === 'price_asc' || sort === 'price_desc') {
      const left = amount(a); const right = amount(b);
      if (left === null && right === null) return 0;
      if (left === null) return 1;
      if (right === null) return -1;
      return sort === 'price_asc' ? left - right : right - left;
    }
    if (sort === 'reputation') {
      const left = a.reputation?.ratingAvg ?? -1; const right = b.reputation?.ratingAvg ?? -1;
      if (left !== right) return right - left;
      return (b.reputation?.completedCount ?? 0) - (a.reputation?.completedCount ?? 0);
    }
    return new Date(b.updatedAt ?? b.submittedAt).getTime() - new Date(a.updatedAt ?? a.submittedAt).getTime();
  });
  return copy;
}

export const OFFER_STATUS_LABEL_KEYS: Record<ClassifiedApplication['status'], string> = {
  submitted: 'marketplace.status.submitted',
  viewed: 'marketplace.status.viewed',
  shortlisted: 'marketplace.status.shortlisted',
  countered_by_author: 'marketplace.status.counteredByAuthor',
  countered_by_applicant: 'marketplace.status.counteredByApplicant',
  accepted: 'marketplace.status.accepted',
  rejected: 'marketplace.status.rejected',
  withdrawn: 'marketplace.status.withdrawn',
  expired: 'marketplace.status.expired',
  conversation_open: 'marketplace.status.conversationOpen',
  converted: 'marketplace.status.converted',
};
