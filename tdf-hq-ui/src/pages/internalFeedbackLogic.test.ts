import {
  internalReportAdminTransitions,
  internalReportContextDefaults,
  internalReportMutationsAllowed,
  internalReportRetestAllowed,
} from './internalFeedbackLogic';

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

describe('internal report retest controls', () => {
  it('does not offer the retest-only state to standalone reports', () => {
    expect(internalReportAdminTransitions('in_progress', null)).toEqual(['discarded']);
    expect(internalReportAdminTransitions('in_progress', 'case-1')).toEqual([
      'ready_for_retest',
      'discarded',
    ]);
  });

  it('only enables retesting for mutable reports linked to a test case', () => {
    expect(internalReportRetestAllowed('ready_for_retest', null, true)).toBe(false);
    expect(internalReportRetestAllowed('ready_for_retest', 'case-1', false)).toBe(false);
    expect(internalReportRetestAllowed('ready_for_retest', 'case-1', true)).toBe(true);
  });
});
