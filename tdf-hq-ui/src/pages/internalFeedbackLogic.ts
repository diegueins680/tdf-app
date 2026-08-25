export const internalReportMutationsAllowed = (auditPlanMutable: boolean) => auditPlanMutable;

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
