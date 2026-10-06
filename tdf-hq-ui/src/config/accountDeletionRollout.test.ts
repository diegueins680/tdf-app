import { isAccountDeletionFormEnabled, isAccountDeletionQueueEnabled } from './accountDeletionRollout';

describe('account deletion rollout', () => {
  it.each([undefined, null, '', 'false', 'TRUE', '1', true, ' true '])('fails closed for %p', value => {
    expect(isAccountDeletionFormEnabled(value)).toBe(false);
  });
  it('requires explicit enablement after backend qualification', () => {
    expect(isAccountDeletionFormEnabled('true')).toBe(true);
  });
  it('stays off when the public build setting is absent', () => {
    expect(isAccountDeletionFormEnabled()).toBe(false);
  });
});

it.each([undefined, null, '', 'false', 'TRUE', '1', true, ' true '])('operator rollout fails closed independently for %p', value => {
  expect(isAccountDeletionQueueEnabled(value)).toBe(false);
});
it('keeps processing available while new intake is paused', () => {
  expect(isAccountDeletionFormEnabled('false')).toBe(false);
  expect(isAccountDeletionQueueEnabled('true')).toBe(true);
  expect(isAccountDeletionQueueEnabled()).toBe(false);
});
