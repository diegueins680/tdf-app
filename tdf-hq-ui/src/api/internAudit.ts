import { get, patch, post, postEmpty, put } from './client';
import type {
  InternAuditPlanDTO,
  InternDailySummaryCreate,
  InternDailySummaryDTO,
  InternFinalSummaryDTO,
  InternTestCaseDTO,
  InternTestExecutionCreate,
  InternTestExecutionDTO,
} from './types';

export const InternAudit = {
  listPlans: () => get<InternAuditPlanDTO[]>('/internships/audit-plans'),
  getPlan: (planId: string) =>
    get<InternAuditPlanDTO>(`/internships/audit-plans/${encodeURIComponent(planId)}`),
  activatePlan: (planId: string) =>
    postEmpty<InternAuditPlanDTO>(`/internships/audit-plans/${encodeURIComponent(planId)}/activate`),
  completePlan: (planId: string, exceptionJustification?: string) =>
    patch<InternAuditPlanDTO>(
      `/internships/audit-plans/${encodeURIComponent(planId)}`,
      exceptionJustification === undefined
        ? { iapuStatus: 'completed' }
        : {
            iapuCompletionJustification: exceptionJustification,
            iapuApproveException: true,
            iapuStatus: 'completed',
          },
    ),
  listCases: (planId: string) =>
    get<InternTestCaseDTO[]>(`/internships/audit-plans/${encodeURIComponent(planId)}/cases`),
  listExecutions: (testCaseId: string) =>
    get<InternTestExecutionDTO[]>(
      `/internships/test-cases/${encodeURIComponent(testCaseId)}/executions`,
    ),
  createExecution: (testCaseId: string, payload: InternTestExecutionCreate) =>
    post<InternTestExecutionDTO>(
      `/internships/test-cases/${encodeURIComponent(testCaseId)}/executions`,
      payload,
    ),
  updateExecution: (executionId: string, payload: Partial<InternTestExecutionCreate>) =>
    patch<InternTestExecutionDTO>(
      `/internships/test-executions/${encodeURIComponent(executionId)}`,
      {
        iteuStatus: payload.itecStatus,
        iteuActualResult: payload.itecActualResult,
        iteuPersistedStateObserved: payload.itecPersistedStateObserved,
        iteuSideEffectsObserved: payload.itecSideEffectsObserved,
        iteuBlockerReason: payload.itecBlockerReason,
        iteuEvidenceSummary: payload.itecEvidenceSummary,
      },
    ),
  listDailySummaries: (planId: string) =>
    get<InternDailySummaryDTO[]>(
      `/internships/audit-plans/${encodeURIComponent(planId)}/daily-summaries`,
    ),
  createDailySummary: (planId: string, payload: InternDailySummaryCreate) =>
    post<InternDailySummaryDTO>(
      `/internships/audit-plans/${encodeURIComponent(planId)}/daily-summaries`,
      payload,
    ),
  getFinalSummary: (planId: string) =>
    get<InternFinalSummaryDTO>(
      `/internships/audit-plans/${encodeURIComponent(planId)}/final-summary`,
    ),
  saveFinalSummary: (planId: string, conclusions: string, submit = false) =>
    put<InternFinalSummaryDTO>(
      `/internships/audit-plans/${encodeURIComponent(planId)}/final-summary`,
      { ifsuConclusions: conclusions, ifsuSubmit: submit },
    ),
};
