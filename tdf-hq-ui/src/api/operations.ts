import { get, patch, post } from './client';
import type { components } from './generated/types';

export type OperationsWorkItem = components['schemas']['OperationsWorkItem'];
export type OperationsWorkItemPage = components['schemas']['OperationsWorkItemPage'];
export type OperationsWorkItemDetail = components['schemas']['OperationsWorkItemDetail'];
export type OperationsMetrics = components['schemas']['OperationsMetrics'];
export type OperationsStatus = components['schemas']['OperationsStatus'];
export type OperationsPriority = components['schemas']['OperationsPriority'];
export type OperationsSlaState = components['schemas']['OperationsSlaState'];
export type OperationsNote = components['schemas']['OperationsNote'];
export type OperationsSavedView = components['schemas']['OperationsSavedView'];
export type OperationsSavedViewCreate = components['schemas']['OperationsSavedViewCreate'];
export type OperationsStreamBatch = components['schemas']['OperationsStreamBatch'];

export interface OperationsFilters {
  cursor?: string;
  limit?: number;
  q?: string;
  seen?: boolean;
  entityType?: string;
  status?: OperationsStatus;
  priority?: OperationsPriority;
  slaState?: OperationsSlaState;
  assigneePartyId?: number;
  customerPartyId?: number;
  service?: string;
  from?: string;
  to?: string;
  minAmountMinor?: number;
  maxAmountMinor?: number;
  paymentState?: string;
  organizationId?: string;
  branchId?: string;
  sourceChannel?: string;
}

const queryString = (values: Record<string, unknown>): string => {
  const params = new URLSearchParams();
  Object.entries(values).forEach(([key, value]) => {
    if (value === undefined || value === null || value === '') return;
    if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
      params.set(key, String(value));
    }
  });
  const encoded = params.toString();
  return encoded ? `?${encoded}` : '';
};

const commandMetadata = () => ({
  requestId: typeof crypto !== 'undefined' && 'randomUUID' in crypto
    ? crypto.randomUUID()
    : `web-${Date.now()}-${Math.random().toString(16).slice(2)}`,
  sourceClient: 'tdf-hq-ui',
});

export const Operations = {
  metrics: (organizationId?: string, branchId?: string) =>
    get<OperationsMetrics>(`/operations/metrics${queryString({ organizationId, branchId })}`),
  list: (filters: OperationsFilters = {}) =>
    get<OperationsWorkItemPage>(`/operations/work-items${queryString({ ...filters })}`),
  detail: (workItemId: string) =>
    get<OperationsWorkItemDetail>(`/operations/work-items/${encodeURIComponent(workItemId)}`),
  markSeen: (item: OperationsWorkItem) =>
    patch<OperationsWorkItem>(`/operations/work-items/${encodeURIComponent(item.id)}/seen`, {
      expectedVersion: item.version,
      reason: null,
      ...commandMetadata(),
    }),
  transition: (item: OperationsWorkItem, targetStatus: OperationsStatus, reason: string,
    waitingExternalDependency?: boolean, resumeAt?: string | null) =>
    patch<OperationsWorkItem>(`/operations/work-items/${encodeURIComponent(item.id)}/transition`, {
      expectedVersion: item.version,
      targetStatus,
      reason,
      waitingExternalDependency: waitingExternalDependency ?? null,
      resumeAt: resumeAt ?? null,
      ...commandMetadata(),
    }),
  assign: (item: OperationsWorkItem, assigneePartyId: number | null, responsibleTeam: string | null, reason: string) =>
    patch<OperationsWorkItem>(`/operations/work-items/${encodeURIComponent(item.id)}/assignment`, {
      expectedVersion: item.version,
      assigneePartyId,
      responsibleTeam,
      reason,
      ...commandMetadata(),
    }),
  prioritize: (item: OperationsWorkItem, priority: OperationsPriority, reason: string) =>
    patch<OperationsWorkItem>(`/operations/work-items/${encodeURIComponent(item.id)}/priority`, {
      expectedVersion: item.version,
      priority,
      reason,
      ...commandMetadata(),
    }),
  addNote: (workItemId: string, body: string, mentionedPartyIds: number[] = []) =>
    post<OperationsNote>(`/operations/work-items/${encodeURIComponent(workItemId)}/notes`, {
      body,
      mentionedPartyIds,
      ...commandMetadata(),
    }),
  events: (afterId?: number, organizationId?: string) =>
    get<OperationsStreamBatch>(`/operations/events${queryString({ afterId, limit: 250, organizationId })}`),
  savedViews: (organizationId?: string) =>
    get<OperationsSavedView[]>(`/operations/saved-views${queryString({ organizationId })}`),
  saveView: (input: Omit<OperationsSavedViewCreate, 'requestId' | 'sourceClient'>) =>
    post<OperationsSavedView>('/operations/saved-views', { ...input, ...commandMetadata() }),
};
