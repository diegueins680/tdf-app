import type {
  InternAuditPlanDTO,
  InternExecutionStatus,
  InternTestCaseDTO,
  InternTestExecutionDTO,
} from '../api/types';

export const buildInternalReportHref = (
  testCase: Pick<
    InternTestCaseDTO,
    'itcPlanId' | 'itcId' | 'itcModuleName' | 'itcFeatureName' | 'itcEnvironment'
      | 'itcUserRole'
  > & { itcLatestExecution?: Pick<InternTestExecutionDTO, 'itexId'> | null },
  plan: { iapProjectId: string; iapTaskId: string },
) => {
  const query = new URLSearchParams({
    planId: testCase.itcPlanId,
    projectId: plan.iapProjectId,
    taskId: plan.iapTaskId,
    testCaseId: testCase.itcId,
    module: testCase.itcModuleName,
    feature: testCase.itcFeatureName,
    environment: testCase.itcEnvironment,
    accountRole: testCase.itcUserRole,
  });
  if (testCase.itcLatestExecution?.itexId) {
    query.set('executionId', testCase.itcLatestExecution.itexId);
  }
  return `/feedback/interno/nuevo?${query.toString()}`;
};

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

export const adminCompletionAction = (
  status: InternAuditPlanDTO['iapStatus'],
  canComplete: boolean,
) => {
  if (status !== 'active') return 'none' as const;
  return canComplete ? 'standard' as const : 'exception' as const;
};

export const formatGeneratedSnapshot = (snapshot: string): string => {
  try {
    const parsedSnapshot: unknown = JSON.parse(snapshot);
    return JSON.stringify(parsedSnapshot, null, 2);
  } catch {
    return snapshot;
  }
};
