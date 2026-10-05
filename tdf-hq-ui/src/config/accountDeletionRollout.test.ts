import { isAccountDeletionFormEnabled } from './accountDeletionRollout';

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
