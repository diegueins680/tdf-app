import type { InternalReportState } from '../api/types';

const ADMIN_TRANSITIONS: Record<InternalReportState, InternalReportState[]> = {
  draft: [],
  submitted: [],
  received: ['needs_information', 'confirmed', 'duplicate', 'discarded'],
  needs_information: ['received', 'confirmed', 'discarded'],
  confirmed: ['prioritized', 'in_progress', 'duplicate', 'discarded'],
  prioritized: ['in_progress', 'discarded'],
  in_progress: ['ready_for_retest', 'discarded'],
  ready_for_retest: ['verified', 'in_progress'],
  verified: ['closed', 'in_progress'],
  closed: ['received'],
  duplicate: ['received'],
  discarded: ['received'],
};

export const internalReportMutationsAllowed = (auditPlanMutable: boolean) => auditPlanMutable;

export const internalReportAdminTransitions = (
  state: InternalReportState,
  testCaseId: string | null | undefined,
) => ADMIN_TRANSITIONS[state].filter((nextState) =>
  nextState !== 'ready_for_retest' || Boolean(testCaseId));

export const internalReportRetestAllowed = (
  state: InternalReportState,
  testCaseId: string | null | undefined,
  auditPlanMutable: boolean,
) => auditPlanMutable && state === 'ready_for_retest' && Boolean(testCaseId);

export const internalReportContextDefaults = (
  searchParams: URLSearchParams,
  sessionRoles: string[] | null | undefined,
) => {
  const requestedEnvironment = searchParams.get('environment');
  const requestedRole = searchParams.get('accountRole');
  const sessionRole = sessionRoles?.[0];
  return {
    environment: requestedEnvironment === null || requestedEnvironment === ''
      ? 'staging'
      : requestedEnvironment,
    accountRole: requestedRole === null || requestedRole === ''
      ? (sessionRole === undefined || sessionRole === '' ? 'Intern' : sessionRole)
      : requestedRole,
    auditLinked: Boolean(searchParams.get('testCaseId')),
  };
};
