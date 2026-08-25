import { extractErrorDetails } from './errorMessage';

describe('extractErrorDetails', () => {
  it('returns the body for malformed JSON error payloads', () => {
    const body = '{"message":"broken"';

    expect(extractErrorDetails(body, 'application/json')).toBe(body);
  });
});
