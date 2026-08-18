import { get, post, put } from './client';

export type EventResearchConfidence = 'high' | 'medium' | 'low';
export type EventResearchReviewState = 'draft' | 'review' | 'discarded';
export type EventResearchRunStatus = 'running' | 'completed' | 'failed';

export interface EventResearchEvidence {
  erEvidenceUrl: string;
  erEvidenceKind: string;
  erEvidenceConsultedAt: string;
  erEvidenceNotes?: string | null;
}

export interface EventResearchPilot {
  erPilotApproved: boolean;
  erPilotApprovedAt?: string | null;
  erPilotApprovedByPartyId?: string | null;
  erPilotApprovalReference?: string | null;
  erPilotMaxActiveCandidates: number;
  erPilotActiveCandidates: number;
  erPilotUpdatedAt: string;
}

export interface EventResearchRun {
  erRunId: string;
  erRunKey: string;
  erRunStatus: EventResearchRunStatus;
  erRunReconciliation: boolean;
  erRunCheckpoint?: string | null;
  erRunCounters: Record<string, unknown>;
  erRunErrorSummary?: string | null;
  erRunStartedAt: string;
  erRunUpdatedAt: string;
  erRunFinishedAt?: string | null;
  erRunCreatedByPartyId: string;
}

export interface EventResearchCandidateWrite {
  erCandidateProvider: string;
  erCandidateExternalId: string;
  erCandidateRunId: string;
  erCandidateSourceId?: string | null;
  erCandidateReviewState: EventResearchReviewState;
  erCandidateTitle: string;
  erCandidateStartTime?: string | null;
  erCandidateEndTime?: string | null;
  erCandidateTimezone: string;
  erCandidateVenueName?: string | null;
  erCandidateCity?: string | null;
  erCandidateProvince?: string | null;
  erCandidateCountryCode: string;
  erCandidateSourceUrl: string;
  erCandidateInfoUrl?: string | null;
  erCandidatePurchaseUrl?: string | null;
  erCandidatePayload: Record<string, unknown>;
  erCandidateEvidence: EventResearchEvidence[];
  erCandidateConfidence: EventResearchConfidence;
  erCandidateManagedFields: string[];
  erCandidateVerifiedAt: string;
}

export interface EventResearchCandidate extends EventResearchCandidateWrite {
  erCandidateId: string;
  erCandidateEventId?: string | null;
  erCandidateContentHash: string;
  erCandidateIsPilot: boolean;
  erCandidateCreatedAt: string;
  erCandidateUpdatedAt: string;
}

export interface EventResearchChange {
  erChangeId: string;
  erChangeRunId: string;
  erChangeCandidateId?: string | null;
  erChangeEventId?: string | null;
  erChangeAction: string;
  erChangeBeforeValue?: Record<string, unknown> | null;
  erChangeAfterValue?: Record<string, unknown> | null;
  erChangeSourceUrl: string;
  erChangeConfidence: EventResearchConfidence;
  erChangeConsultedAt: string;
  erChangeExternalId: string;
  erChangeResult: string;
  erChangeCreatedAt: string;
}

export interface EventResearchMaterialization {
  erMaterializationRunId: string;
  erMaterializationCandidateId: string;
  erMaterializationEventId: string;
  erMaterializationVenueId: string;
  erMaterializationArtistIds: string[];
  erMaterializationChangeId: string;
  erMaterializationCreated: boolean;
  erMaterializationPublished: boolean;
}

const queryString = (entries: Record<string, string | number | undefined>) => {
  const params = new URLSearchParams();
  Object.entries(entries).forEach(([key, value]) => {
    if (value !== undefined) params.set(key, String(value));
  });
  const encoded = params.toString();
  return encoded ? `?${encoded}` : '';
};

export const EventResearchAPI = {
  getPilot: () => get<EventResearchPilot>('/social-events/event-research/pilot'),
  approvePilot: (approvalReference: string) =>
    post<EventResearchPilot>('/social-events/event-research/pilot/approve', {
      erPilotApprovalReference: approvalReference,
    }),
  listRuns: (limit = 100) =>
    get<EventResearchRun[]>(`/social-events/event-research/runs${queryString({ limit })}`),
  createRun: (payload: { erRunKey: string; erRunReconciliation: boolean; erRunCheckpoint?: string | null }) =>
    post<EventResearchRun>('/social-events/event-research/runs', payload),
  updateRun: (runId: string, payload: {
    erRunStatus: EventResearchRunStatus;
    erRunCheckpoint?: string | null;
    erRunCounters: Record<string, unknown>;
    erRunErrorSummary?: string | null;
  }) => put<EventResearchRun>(`/social-events/event-research/runs/${encodeURIComponent(runId)}`, payload),
  listCandidates: (filters: { provider?: string; reviewState?: EventResearchReviewState; limit?: number } = {}) =>
    get<EventResearchCandidate[]>(`/social-events/event-research/candidates${queryString({
      provider: filters.provider,
      review_state: filters.reviewState,
      limit: filters.limit,
    })}`),
  upsertCandidate: (payload: EventResearchCandidateWrite) =>
    put<EventResearchCandidate>('/social-events/event-research/candidates', payload),
  materializeCandidate: (candidateId: string, runId: string, publish: boolean) =>
    post<EventResearchMaterialization>(
      `/social-events/event-research/candidates/${encodeURIComponent(candidateId)}/materialize`,
      { erMaterializationRunId: runId, erMaterializationPublish: publish },
    ),
  listChanges: (filters: { runId?: string; limit?: number } = {}) =>
    get<EventResearchChange[]>(`/social-events/event-research/changes${queryString({
      run_id: filters.runId,
      limit: filters.limit,
    })}`),
};
