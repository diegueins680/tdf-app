import { get, post } from './client';

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
  cpiStatus: string;
  cpiCurrency: string;
  cpiCount: number;
  cpiAmountMinor: number;
  cpiAuthorizedMinor: number;
  cpiCapturedMinor: number;
  cpiRefundedMinor: number;
}

export interface CommerceAmountComponentSummary {
  cacComponentType: string;
  cacSource: string;
  cacCurrency: string;
  cacCount: number;
  cacAmountMinor: number;
}

export interface CommerceCommissionSummary {
  ccmProvider: string;
  ccmEnvironment: string;
  ccmCurrency: string;
  ccmCount: number;
  ccmBasisAmountMinor: number;
  ccmCommissionMinor: number;
  ccmProviderFeeMinor: number;
  ccmTaxMinor: number;
  ccmSellerNetMinor: number;
}

export interface CommerceRefundSummary {
  crfProvider: string;
  crfEnvironment: string;
  crfStatus: string;
  crfCurrency: string;
  crfCount: number;
  crfAmountMinor: number;
}

export interface CommerceDisputeSummary {
  cdsProvider: string;
  cdsEnvironment: string;
  cdsKind: string;
  cdsStatus: string;
  cdsCurrency: string;
  cdsCount: number;
  cdsAmountMinor: number;
}

export interface CommerceReconciliationSummary {
  crsProvider: string;
  crsEnvironment: string;
  crsStatus: string;
  crsCurrency?: string | null;
  crsCount: number;
  crsExpectedMinor: number;
  crsActualMinor: number;
}

export interface CommerceSettlementSummary {
  cssProvider: string;
  cssEnvironment: string;
  cssStatus: string;
  cssCurrency: string;
  cssCount: number;
  cssGrossMinor: number;
  cssFeeMinor: number;
  cssWithholdingMinor: number;
  cssRefundMinor: number;
  cssChargebackMinor: number;
  cssNetMinor: number;
}

export interface CommerceSellerBalanceSummary {
  csbProvider: string;
  csbEnvironment: string;
  csbAvailability: string;
  csbCurrency: string;
  csbEntryCount: number;
  csbNetAmountMinor: number;
}

export interface CommercePayoutSummary {
  cpsProvider: string;
  cpsEnvironment: string;
  cpsStatus: string;
  cpsCurrency: string;
  cpsCount: number;
  cpsAmountMinor: number;
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

export const CommerceOperations = {
  getPaymentOverview: () => get<CommercePaymentOverview>('/admin/commerce/overview'),

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
