import { dailySummaryMutationsAllowed, executionEvidenceRequired } from './internAuditPlanLogic';

describe('intern audit execution evidence requirements', () => {
  it('requires evidence for every failed or blocked result, including light-evidence cases', () => {
    expect(executionEvidenceRequired('light', 'failed')).toBe(true);
    expect(executionEvidenceRequired('light', 'blocked')).toBe(true);
  });

  it('continues to require evidence for strong cases and keeps non-terminal light cases optional', () => {
    expect(executionEvidenceRequired('strong', 'passed')).toBe(true);
    expect(executionEvidenceRequired('light', 'in_progress')).toBe(false);
  });
});

describe('intern audit daily-summary permissions', () => {
  it('allows only the assigned party to write an active plan summary', () => {
    expect(dailySummaryMutationsAllowed('active', 911, 911)).toBe(true);
    expect(dailySummaryMutationsAllowed('active', 911, 913)).toBe(false);
  });

  it('keeps the summary read-only when the plan is finalized or unassigned', () => {
    expect(dailySummaryMutationsAllowed('completed', 911, 911)).toBe(false);
    expect(dailySummaryMutationsAllowed('active', null, 911)).toBe(false);
  });
});
