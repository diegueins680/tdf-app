import { get, patch } from './client';
import type { PipelineCardDTO, PipelineCardUpdate } from './types';

export const Pipelines = {
  list: (pipelineType: string) => get<PipelineCardDTO[]>(`/pipelines/${pipelineType}`),
  stages: (pipelineType: string) => get<string[]>(`/pipelines/${pipelineType}/stages`),
  update: (pipelineType: string, cardId: string, body: PipelineCardUpdate) =>
    patch<void>(`/pipelines/${pipelineType}/${cardId}`, body),
};
