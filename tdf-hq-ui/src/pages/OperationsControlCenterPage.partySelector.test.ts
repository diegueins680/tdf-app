import { readFileSync } from 'node:fs';

const source = readFileSync(new URL('./OperationsControlCenterPage.tsx', import.meta.url), 'utf8');

describe('Operations assignment Party relationship regression', () => {
  it('uses valid selector options for assignment and filters', () => {
    expect(source.match(/<PartySelector(?:\s|$)/g)).toHaveLength(3);
    expect(source).toContain('assignmentParty?.partyId ?? null');
    expect(source).toContain('onChange={setAssignmentParty}');
  });

  it('does not expose a numeric assignee relationship input', () => {
    expect(source).not.toContain('assigneeInput');
    expect(source).not.toContain("replace(/\\D/g, '')");
    expect(source).not.toContain("t('operations.assigneeId')");
  });
});
