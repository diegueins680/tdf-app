import { internalReportContextDefaults, internalReportMutationsAllowed } from './internalFeedbackLogic';

describe('internal report context defaults', () => {
  it('prefers the audit case role and environment over the first session role', () => {
    const context = internalReportContextDefaults(new URLSearchParams({
      testCaseId: 'case-1',
      environment: 'staging',
      accountRole: 'StudioManager',
    }), ['Intern', 'Admin']);

    expect(context).toEqual({
      environment: 'staging',
      accountRole: 'StudioManager',
      auditLinked: true,
    });
  });

  it('uses the current session role for an unlinked administrative draft', () => {
    expect(internalReportContextDefaults(new URLSearchParams(), ['Manager'])).toEqual({
      environment: 'staging',
      accountRole: 'Manager',
      auditLinked: false,
    });
  });
});

describe('internal report mutation controls', () => {
  it('allows changes while the owning audit is active', () => {
    expect(internalReportMutationsAllowed(true)).toBe(true);
  });

  it('makes reports read-only after the owning audit is finalized', () => {
    expect(internalReportMutationsAllowed(false)).toBe(false);
  });
});
