import { get, post, put } from './client';

export type MerchReputationState = 'new_store' | 'unrated' | 'published';
export type MerchConfidence = 'new' | 'limited' | 'moderate' | 'strong';

export interface MerchDimensionAggregate {
  code: string;
  average: number | null;
  count: number;
  distribution?: Record<string, number>;
}

export interface MerchReputationSummary {
  subjectKind?: 'store' | 'product';
  id?: string;
  storeId?: string;
  productId?: string;
  storeName?: string;
  productName?: string;
  name?: string;
  slug?: string;
  commercialReputation?: boolean;
  reputationLabel?: string;
  state: MerchReputationState;
  rating: number | null;
  verifiedReviewCount?: number;
  verifiedPurchaseReviewCount?: number;
  historicalReviewCount?: number;
  confidence?: MerchConfidence;
  formulaVersion?: string | null;
  dimensions?: MerchDimensionAggregate[];
  objectiveSignals?: {
    identityVerified?: boolean;
    platformMemberSince?: string;
  };
  badges?: { code: string; earnedAt: string; validUntil: string }[];
}

export interface MerchReviewEligibility {
  orderId: string;
  storeId: string;
  orderState?: string;
  fulfillmentState?: string;
  storeReview: {
    eligible: boolean;
    state: 'available' | 'edit_available' | 'period_expired';
    reviewId: string | null;
    currentRevision: number;
    deadline: string | null;
  };
  productLines: {
    lineId: string;
    productId: string;
    productName: string;
    fulfillmentState: string;
    eligible: boolean;
    state: 'available' | 'edit_available' | 'period_expired';
    reviewId: string | null;
    currentRevision: number;
  }[];
}

export interface MerchReviewSubmit {
  overallRating: number;
  issueOccurred: boolean;
  comment?: string;
  dimensions: Record<string, number>;
  images?: { mediaAssetId: string; altText: string }[];
  expectedRevision: number;
}

export interface MerchReviewPublic {
  id: string;
  kind: 'store' | 'product';
  rating: number;
  comment: string | null;
  status: 'published' | 'limited';
  verifiedPurchase: true;
  badge: 'Compra verificada';
  author: { name: string; avatarUrl?: string | null };
  dimensions: Record<string, number>;
  images: { assetId: string; url: string; altText: string; position: number }[];
  sellerResponse?: { id: string; body: string; updatedAt: string; status: string } | null;
  createdAt: string;
  updatedAt: string;
}

export interface MerchReputationPriorities {
  subjectKind: 'store' | 'product';
  revision: number;
  affectsPublicScore: false;
  orderedDimensions: {
    code: string;
    nameEs: string;
    nameEn: string;
    definitionEs: string;
    definitionEn: string;
  }[];
}

export interface MerchNotificationPreferences {
  reviewInvitation: boolean;
  reviewReminder: boolean;
  sellerResponseNotification: boolean;
  moderationChange: boolean;
  evidenceRequest: boolean;
  appealResult: boolean;
  badgeChange: boolean;
}

const idempotency = (key?: string) => ({
  headers: { 'Idempotency-Key': key ?? crypto.randomUUID() },
});

export const MerchReputation = {
  artistStores: (artistPartyId: number) =>
    get<MerchReputationSummary[]>('/merch/artists/' + artistPartyId + '/stores'),
  store: (storeId: string) =>
    get<MerchReputationSummary>('/merch/stores/' + encodeURIComponent(storeId) + '/reputation'),
  product: (productId: string) =>
    get<MerchReputationSummary>('/merch/products/' + encodeURIComponent(productId) + '/reputation'),
  reviews: (kind: 'store' | 'product', subjectId: string, cursor?: string) => {
    const query = cursor ? '?cursor=' + encodeURIComponent(cursor) : '';
    const plural = kind === 'store' ? 'stores' : 'products';
    return get<{ items: MerchReviewPublic[]; nextCursor?: string | null }>(
      '/merch/' + plural + '/' + encodeURIComponent(subjectId) + '/reviews' + query,
    );
  },
  formula: (locale: 'es' | 'en' = 'es') =>
    get<Record<string, unknown>>('/merch/reputation/formula?locale=' + locale),
  eligibility: (orderId: string) =>
    get<MerchReviewEligibility>('/merch/orders/' + encodeURIComponent(orderId) + '/reviews/eligibility'),
  submitStore: (orderId: string, body: MerchReviewSubmit, key?: string) =>
    put<Record<string, unknown>>(
      '/merch/orders/' + encodeURIComponent(orderId) + '/store-review',
      body,
      idempotency(key),
    ),
  submitProduct: (orderId: string, lineId: string, body: MerchReviewSubmit, key?: string) =>
    put<Record<string, unknown>>(
      '/merch/orders/' + encodeURIComponent(orderId) + '/lines/' + encodeURIComponent(lineId) + '/product-review',
      body,
      idempotency(key),
    ),
  respond: (reviewId: string, responseBody: string, responseExpectedRevision: number, key?: string) =>
    put<Record<string, unknown>>(
      '/merch/reviews/' + encodeURIComponent(reviewId) + '/response',
      { responseBody, responseExpectedRevision },
      idempotency(key),
    ),
  report: (
    body: {
      reportTargetType: 'review' | 'seller_response';
      reportTargetId: string;
      reportReason: string;
      reportDetails?: string;
      authorizedEvidence?: unknown[];
    },
    key?: string,
  ) => post<Record<string, unknown>>('/merch/reputation/reports', body, idempotency(key)),
  appeal: (decisionId: string, appealGrounds: string, key?: string) =>
    post<Record<string, unknown>>(
      '/merch/reputation/decisions/' + encodeURIComponent(decisionId) + '/appeal',
      { appealGrounds },
      idempotency(key),
    ),
  sellerStore: (storeId: string) =>
    get<Record<string, unknown>>('/merch/seller/stores/' + encodeURIComponent(storeId) + '/reputation'),
  moderationCases: (state?: string) =>
    get<Record<string, unknown>[]>(
      '/merch/admin/reputation/cases' + (state ? '?state=' + encodeURIComponent(state) : ''),
    ),
  transitionCase: (
    caseId: string,
    moderationAction: 'triage' | 'request_evidence' | 'provisionally_hide' | 'resume_review',
    workflowRationale: string,
    key?: string,
  ) => post<Record<string, unknown>>(
    '/merch/admin/reputation/cases/' + encodeURIComponent(caseId) + '/workflow',
    { moderationAction, workflowRationale, workflowEvidence: { reviewedInAdminPanel: true } },
    idempotency(key),
  ),
  decide: (caseId: string, body: Record<string, unknown>, key?: string) =>
    post<Record<string, unknown>>(
      '/merch/admin/reputation/cases/' + encodeURIComponent(caseId) + '/decision',
      body,
      idempotency(key),
    ),
  resolveAppeal: (
    appealId: string,
    appealOutcome: 'upheld' | 'reversed',
    appealRationale: string,
    key?: string,
  ) => post<Record<string, unknown>>(
    '/merch/admin/reputation/appeals/' + encodeURIComponent(appealId) + '/decision',
    { appealOutcome, appealRationale, appealEvidence: { reviewedInAdminPanel: true } },
    idempotency(key),
  ),
  priorities: (subjectKind: 'store' | 'product') =>
    get<MerchReputationPriorities>('/merch/reputation/preferences/' + subjectKind),
  savePriorities: (
    subjectKind: 'store' | 'product',
    orderedDimensionCodes: string[],
    priorityExpectedRevision: number,
    key?: string,
  ) => put<MerchReputationPriorities>(
    '/merch/reputation/preferences/' + subjectKind,
    { orderedDimensionCodes, priorityExpectedRevision },
    idempotency(key),
  ),
  suggestCategory: (
    suggestionSubjectKind: 'store' | 'product',
    suggestionLabel: string,
    suggestionDefinition: string,
    key?: string,
  ) => post<Record<string, unknown>>(
    '/merch/reputation/category-suggestions',
    { suggestionSubjectKind, suggestionLabel, suggestionDefinition },
    idempotency(key),
  ),
  notificationPreferences: () =>
    get<MerchNotificationPreferences>('/merch/reputation/notification-preferences'),
  saveNotificationPreferences: (body: MerchNotificationPreferences) =>
    put<MerchNotificationPreferences>('/merch/reputation/notification-preferences', body),
  categorySuggestions: (status?: string) => get<Record<string, unknown>[]>(
    '/merch/admin/reputation/category-suggestions' + (status ? '?status=' + encodeURIComponent(status) : ''),
  ),
  decideCategorySuggestion: (
    suggestionId: string,
    body: Record<string, unknown>,
    key?: string,
  ) => post<Record<string, unknown>>(
    '/merch/admin/reputation/category-suggestions/' + encodeURIComponent(suggestionId) + '/decision',
    body,
    idempotency(key),
  ),
};
