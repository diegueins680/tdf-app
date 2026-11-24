import { get, post } from './client';
import type { components } from './generated/types';

export type CommerceReconciliationReport = components['schemas']['CommerceReconciliationReport'];
export type CommerceReconciliationEntry = components['schemas']['CommerceReconciliationEntry'];
export interface CommerceReconciliationFilters {
  environment?: CommerceReconciliationReport['crrEnvironment'];
  status?: NonNullable<CommerceReconciliationReport['crrStatus']>;
  checkoutId?: string;
  limit?: number;
  offset?: number;
}

export type CommerceProviderQueries = components['schemas']['CommerceProviderQueries'];
export type CommerceProviderQuery = components['schemas']['CommerceProviderQuery'];
export interface CommerceProviderQueryFilters {
  environment?: CommerceProviderQueries['cpqsEnvironment'];
  status?: CommerceProviderQuery['cpqStatus'];
  limit?: number;
  offset?: number;
}

export type CommerceProviderEventStatus =
  | 'pending'
  | 'processing'
  | 'processed'
  | 'retry'
  | 'dead_letter'
  | 'ignored';

export interface CommerceProviderEvent {
  cpeId: string;
  cpeProvider: string;
  cpeEnvironment: 'sandbox' | 'production';
  cpeProviderEventId: string;
  cpeEventType: string;
  cpeProviderResourceId?: string | null;
  cpeStatus: CommerceProviderEventStatus;
  cpeAttemptCount: number;
  cpeCheckoutId?: string | null;
  cpePaymentAttemptId?: string | null;
  cpeRefundId?: string | null;
  cpeReceivedAt: string;
  cpeProviderCreatedAt?: string | null;
  cpeProcessingStartedAt?: string | null;
  cpeLastAttemptAt?: string | null;
  cpeNextAttemptAt?: string | null;
  cpeProcessedAt?: string | null;
  cpeErrorSummary?: string | null;
}

export interface CommerceProviderCapability {
  cpcPaymentMethod: string;
  cpcCapability: string;
  cpcVerificationStatus: string;
  cpcVerifiedAt?: string | null;
}

export interface CommerceProviderAccount {
  cpaProvider: string;
  cpaEnvironment: 'sandbox' | 'production';
  cpaStatus: string;
  cpaContractStatus: string;
  cpaCredentialStatus: string;
  cpaSettlementCurrency: string;
  cpaEnabled: boolean;
  cpaFeatureEnabled: boolean;
  cpaVerifiedAt?: string | null;
  cpaDisabledReason?: string | null;
  cpaCapabilities: CommerceProviderCapability[];
}

export interface CommercePaymentIntentSummary {
  cpiEnvironment?: 'sandbox' | 'production';
  cpiStatus: string;
  cpiCurrency: string;
  cpiCount: number;
  cpiAmountMinor: number | string;
  cpiAuthorizedMinor: number | string;
  cpiCapturedMinor: number | string;
  cpiRefundedMinor: number | string;
}

export interface CommerceAmountComponentSummary {
  cacEnvironment?: string;
  cacComponentType: string;
  cacSource: string;
  cacCurrency: string;
  cacCount: number;
  cacAmountMinor: number | string;
}

export interface CommerceCommissionSummary {
  ccmProvider: string;
  ccmEnvironment: string;
  ccmCurrency: string;
  ccmCount: number;
  ccmBasisAmountMinor: number | string;
  ccmCommissionMinor: number | string;
  ccmProviderFeeMinor: number | string;
  ccmTaxMinor: number | string;
  ccmSellerNetMinor: number | string;
}

export interface CommerceRefundSummary {
  crfProvider: string;
  crfEnvironment: string;
  crfStatus: string;
  crfCurrency: string;
  crfCount: number;
  crfAmountMinor: number | string;
}

export interface CommerceDisputeSummary {
  cdsProvider: string;
  cdsEnvironment: string;
  cdsKind: string;
  cdsStatus: string;
  cdsCurrency: string;
  cdsCount: number;
  cdsAmountMinor: number | string;
}

export interface CommerceReconciliationSummary {
  crsProvider: string;
  crsEnvironment: string;
  crsStatus: string;
  crsCurrency?: string | null;
  crsCount: number;
  crsExpectedMinor: number | string;
  crsActualMinor: number | string;
}

export interface CommerceSettlementSummary {
  cssProvider: string;
  cssEnvironment: string;
  cssStatus: string;
  cssCurrency: string;
  cssCount: number;
  cssGrossMinor: number | string;
  cssFeeMinor: number | string;
  cssWithholdingMinor: number | string;
  cssRefundMinor: number | string;
  cssChargebackMinor: number | string;
  cssNetMinor: number | string;
}

export interface CommerceSellerBalanceSummary {
  csbProvider: string;
  csbEnvironment: string;
  csbAvailability: string;
  csbCurrency: string;
  csbEntryCount: number;
  csbNetAmountMinor: number | string;
}

export interface CommercePayoutSummary {
  cpsProvider: string;
  cpsEnvironment: string;
  cpsStatus: string;
  cpsCurrency: string;
  cpsCount: number;
  cpsAmountMinor: number | string;
}

export interface CommercePaymentOverview {
  cpoGeneratedAt: string;
  cpoProviderAccounts: CommerceProviderAccount[];
  cpoPaymentIntents: CommercePaymentIntentSummary[];
  cpoAmountComponents: CommerceAmountComponentSummary[];
  cpoCommissions: CommerceCommissionSummary[];
  cpoRefunds: CommerceRefundSummary[];
  cpoDisputes: CommerceDisputeSummary[];
  cpoReconciliationExceptions: CommerceReconciliationSummary[];
  cpoSettlements: CommerceSettlementSummary[];
  cpoSellerBalances: CommerceSellerBalanceSummary[];
  cpoPayouts: CommercePayoutSummary[];
}

// Keep the exact JSON integer before a browser rounds a large report amount.
const overviewAmount = (key: string, value: unknown, context?: { source?: string }) =>
  key.endsWith('Minor') && typeof value === 'number' && !Number.isSafeInteger(value)
    ? context?.source ?? value : value;

export const CommerceOperations = {
  listReconciliationExceptions: (params: CommerceReconciliationFilters = {}) => {
    const query = new URLSearchParams();
    query.set('environment', params.environment ?? 'sandbox');
    if (params.status !== undefined) query.set('status', params.status);
    if (params.checkoutId !== undefined) query.set('checkoutId', params.checkoutId);
    if (params.limit !== undefined) query.set('limit', String(params.limit));
    if (params.offset !== undefined) query.set('offset', String(params.offset));
    return get<CommerceReconciliationReport>(`/admin/commerce/reconciliation-exceptions?${query.toString()}`);
  },
  listProviderQueries: (params: CommerceProviderQueryFilters = {}) => {
    const query = new URLSearchParams();
    query.set('environment', params.environment ?? 'sandbox');
    if (params.status) query.set('status', params.status);
    if (params.limit !== undefined) query.set('limit', String(params.limit));
    if (params.offset !== undefined) query.set('offset', String(params.offset));
    return get<CommerceProviderQueries>(`/admin/commerce/provider-queries?${query.toString()}`);
  },
  getPaymentOverview: () => get<CommercePaymentOverview>('/admin/commerce/overview', {}, overviewAmount),

  listProviderEvents: (params?: {
    status?: CommerceProviderEventStatus;
    limit?: number;
    offset?: number;
  }) => {
    const query = new URLSearchParams();
    if (params?.status) query.set('status', params.status);
    if (params?.limit) query.set('limit', String(params.limit));
    if (params?.offset) query.set('offset', String(params.offset));
    const suffix = query.toString();
    return get<CommerceProviderEvent[]>(
      `/admin/commerce/provider-events${suffix ? `?${suffix}` : ''}`,
    );
  },

  replayProviderEvent: (eventId: string, reason: string) =>
    post<CommerceProviderEvent>(
      `/admin/commerce/provider-events/${encodeURIComponent(eventId)}/replay`,
      { cperReason: reason },
    ),
};
