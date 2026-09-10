import { firstNonEmptyString } from './stringValues';

describe('firstNonEmptyString', () => {
  test('returns the first truthy string', () => {
    expect(firstNonEmptyString(undefined, null, '', 'fallback', 'later')).toBe('fallback');
  });

  test('preserves logical-OR string truthiness', () => {
    expect(firstNonEmptyString('  ', 'fallback')).toBe('  ');
    expect(firstNonEmptyString('', null, undefined)).toBe('');
  });
});
