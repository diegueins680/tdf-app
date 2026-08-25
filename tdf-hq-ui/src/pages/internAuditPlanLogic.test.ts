import { executionEvidenceRequired } from './internAuditPlanLogic';

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
