import { del, get, patch, post } from './client';
import type {
  PipelineCardCreate,
  PipelineCardDTO,
  PipelineCardUpdate,
  PipelineDefinitionDTO,
  PipelineSnapshotDTO,
  PipelineStageDTO,
} from './types';

function requiredIdentifier(value: string, field: string): string {
  const normalized = value.trim();
  if (!normalized) throw new Error(`${field} is required.`);
  return encodeURIComponent(normalized);
}

export const Pipelines = {
  snapshot: () => get<PipelineSnapshotDTO>('/pipelines/snapshot'),
  definitions: () => get<PipelineDefinitionDTO[]>('/pipelines/definitions'),
  list: (workflowId: string) =>
    get<PipelineCardDTO[]>(`/pipelines/${requiredIdentifier(workflowId, 'workflowId')}`),
  stages: (workflowId: string) =>
    get<PipelineStageDTO[]>(`/pipelines/${requiredIdentifier(workflowId, 'workflowId')}/stages`),
  create: (workflowId: string, body: PipelineCardCreate) =>
    post<PipelineCardDTO>(`/pipelines/${requiredIdentifier(workflowId, 'workflowId')}`, body),
  get: (workflowId: string, cardId: string) =>
    get<PipelineCardDTO>(
      `/pipelines/${requiredIdentifier(workflowId, 'workflowId')}/${requiredIdentifier(cardId, 'cardId')}`,
    ),
  update: (workflowId: string, cardId: string, body: PipelineCardUpdate) =>
    patch<PipelineCardDTO>(
      `/pipelines/${requiredIdentifier(workflowId, 'workflowId')}/${requiredIdentifier(cardId, 'cardId')}`,
      body,
    ),
  remove: (workflowId: string, cardId: string) =>
    del<void>(
      `/pipelines/${requiredIdentifier(workflowId, 'workflowId')}/${requiredIdentifier(cardId, 'cardId')}`,
    ),
};
