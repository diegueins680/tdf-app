import { isEventAgreementsEnabled } from './eventAgreementsRollout';

describe('event agreement rollout', () => {
  it('enables only the explicit string true', () => {
    expect(isEventAgreementsEnabled('true')).toBe(true);
  });
  it.each([undefined, null, false, true, '', 'false', 'TRUE', ' true', 'true ', '1', 1, {}, []])(
    'does not enable an absent or noncanonical flag %j', value => {
      expect(isEventAgreementsEnabled(value)).toBe(false);
    },
  );
});
