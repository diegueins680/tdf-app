import type { InternExecutionStatus } from '../api/types';

export const executionEvidenceRequired = (
  evidenceRequirement: string,
  status: InternExecutionStatus,
) => evidenceRequirement === 'strong' || status === 'failed' || status === 'blocked';
