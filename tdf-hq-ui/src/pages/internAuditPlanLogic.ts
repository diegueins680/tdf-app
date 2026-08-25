import type { InternAuditPlanDTO, InternExecutionStatus } from '../api/types';

export const executionEvidenceRequired = (
  evidenceRequirement: string,
  status: InternExecutionStatus,
) => ['passed', 'failed', 'blocked', 'not_applicable', 'verified'].includes(status)
  && (evidenceRequirement === 'strong' || status === 'failed' || status === 'blocked');

export const dailySummaryMutationsAllowed = (
  status: InternAuditPlanDTO['iapStatus'],
  assignedPartyId: number | null | undefined,
  currentPartyId: number | null | undefined,
) => status === 'active'
  && assignedPartyId != null
  && currentPartyId != null
  && assignedPartyId === currentPartyId;
